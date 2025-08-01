use logos::{Lexer, Logos};

use crate::lexer::{LexState, LexToken, LinePos};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ParserPos {
    pub line: usize,
    pub column: usize,
}

impl Default for ParserPos {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

pub struct Parser<'source, Token, Error>
where
    Token: LexToken<'source>,
    Token::Extras: LexState,
    Error: From<Token::Error> + From<ExpectationFailed<'source, Token>>,
{
    lexer: Lexer<'source, Token>,
    lookahead_tokens: Vec<Result<Token, Token::Error>>,
    pos: ParserPos,
    closed: bool,
    _boo: std::marker::PhantomData<Error>,
}

impl<'source, Token, Error> Parser<'source, Token, Error>
where
    Token: LexToken<'source>,
    Token::Extras: LexState,
    Error: From<Token::Error> + From<ExpectationFailed<'source, Token>>,
{
    const TAB_SIZE: usize = 8;

    pub fn new(source: &'source Token::Source) -> Self {
        Self {
            lexer: Token::lexer(source),
            lookahead_tokens: Vec::new(),
            pos: ParserPos::default(),
            closed: false,
            _boo: std::marker::PhantomData,
        }
    }

    pub fn close(&mut self) {
        if self.closed {
            return;
        }

        self.closed = true;
        self.lookahead_tokens.clear();

        // When closed, we leave the lexer in EOF state
        self.lookahead_tokens.push(Ok(Token::default()));
    }

    pub fn pos(&self) -> &ParserPos {
        &self.pos
    }

    fn update_pos(&mut self) {
        let LinePos {
            line,
            line_start,
            tab_count,
        } = self.lexer.extras.line_pos();

        self.pos.line = *line;
        self.pos.column = self.lexer.span().end - line_start + 1 + tab_count * (Self::TAB_SIZE - 1);
    }

    pub fn pop_token(&mut self) -> Result<Token, Error> {
        if self.closed {
            // Closed => EOF
            return Ok(Token::default());
        }

        let next = self
            .lookahead_tokens
            .pop()
            .or_else(|| self.lexer.next())
            .unwrap_or_else(|| Ok(Token::default()));

        if self.lookahead_tokens.is_empty() {
            self.update_pos();
        }

        if next.is_err() {
            self.close();
        }

        Ok(next?)
    }

    pub fn push_token(&mut self, tok: Token) {
        self.lookahead_tokens.push(Ok(tok));
    }

    pub fn push_and_then<T>(&mut self, tok: Token, then: impl FnOnce(&mut Self) -> T) -> T {
        self.lookahead_tokens.push(Ok(tok));
        then(self)
    }

    pub fn peek_token(&mut self) -> Result<&Token, Error> {
        if self.closed {
            // We take the EOF token from the lookahead deque
            return Ok(self.lookahead_tokens.last().unwrap().as_ref().ok().unwrap());
        }

        if self.lookahead_tokens.is_empty() {
            self.lookahead_tokens
                .push(self.lexer.next().unwrap_or(Ok(Token::default())));
        }

        self.lookahead_tokens
            .last()
            .unwrap()
            .as_ref()
            .map_err(|err| Error::from(err.clone()))
    }

    pub fn morph<Token2, Error2>(&mut self) -> Parser<'source, Token2, Error2>
    where
        Token2: LexToken<'source>,
        Token2::Extras: LexState,
        Token2: Logos<'source, Source = Token::Source>,
        Token::Extras: Into<Token2::Extras>,
        Error2: From<Token2::Error> + From<ExpectationFailed<'source, Token2>>,
    {
        debug_assert!(self.lookahead_tokens.is_empty());

        let closed = self.closed;
        self.close();

        Parser {
            lexer: self.lexer.clone().morph::<Token2>(),
            lookahead_tokens: Vec::new(),
            pos: self.pos,
            closed,
            _boo: std::marker::PhantomData,
        }
    }

    pub fn morph_into<Token2, Error2>(self, target: &mut Parser<'source, Token2, Error2>)
    where
        Token2: LexToken<'source>,
        Token2::Extras: LexState,
        Token2: Logos<'source, Source = Token::Source>,
        Token::Extras: Into<Token2::Extras>,
        Error2: From<Token2::Error> + From<ExpectationFailed<'source, Token2>>,
    {
        debug_assert!(self.lookahead_tokens.is_empty());

        target.lexer = self.lexer.morph::<Token2>();
        target.lookahead_tokens.clear();
        target.pos = self.pos;
        target.closed = self.closed;
    }

    pub fn pop_and_produce<Ok>(&mut self, val: Ok) -> Result<Ok, Error> {
        self.pop_token()?;
        Ok(val)
    }

    pub fn expect_token(&mut self, expected: Token) -> Result<(), Error> {
        let actual = self.pop_token()?;
        if expected != actual {
            Err(Error::from(ExpectationFailed {
                expected,
                actual,
                _boo: std::marker::PhantomData,
            }))
        } else {
            Ok(())
        }
    }

    pub fn parse_sequence<'iter, Element>(
        &'iter mut self,
        element_parser: fn(&mut Parser<'source, Token, Error>) -> Result<Element, Error>,
        separator: Token,
        end: Token,
    ) -> ParseSequenceIter<'iter, 'source, Token, Element, Error> {
        ParseSequenceIter {
            parser: self,
            separator,
            end,
            element_parser,
            started: false,
            ended: false,
        }
    }
}

pub struct ExpectationFailed<'a, Token>
where
    Token: LexToken<'a>,
    Token::Extras: LexState,
{
    pub expected: Token,
    pub actual: Token,
    _boo: std::marker::PhantomData<&'a ()>,
}

pub struct ParseSequenceIter<'iter, 'source, Token, Element, Error>
where
    Token: LexToken<'source>,
    Token::Extras: LexState,
    Error: From<Token::Error> + From<ExpectationFailed<'source, Token>>,
{
    parser: &'iter mut Parser<'source, Token, Error>,
    separator: Token,
    end: Token,
    element_parser: fn(&mut Parser<'source, Token, Error>) -> Result<Element, Error>,
    started: bool,
    ended: bool,
}
impl<'source, Token, Element, Error> Iterator
    for ParseSequenceIter<'_, 'source, Token, Element, Error>
where
    Token: LexToken<'source>,
    Token::Extras: LexState,
    Error: From<Token::Error> + From<ExpectationFailed<'source, Token>>,
{
    type Item = Result<Element, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ended {
            return None;
        }

        let first = if self.started {
            false
        } else {
            self.started = true;
            true
        };

        let tok = match self.parser.peek_token() {
            Ok(tok) => tok,
            Err(err) => {
                self.ended = true;
                return Some(Err(err));
            }
        };

        if *tok == self.end {
            self.ended = true;
            return None;
        }

        if !first {
            if let Err(err) = self.parser.expect_token(self.separator.clone()) {
                self.ended = true;
                return Some(Err(err));
            }
        }

        let elem = (self.element_parser)(self.parser);
        if elem.is_err() {
            self.ended = true;
        }

        Some(elem)
    }
}
