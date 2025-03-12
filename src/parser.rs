use std::collections::VecDeque;

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
    lookahead_tokens: VecDeque<Result<Token, Token::Error>>,
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
    pub fn new(source: &'source Token::Source) -> Self {
        Self {
            lexer: Token::lexer(source),
            lookahead_tokens: VecDeque::new(),
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
        self.lookahead_tokens.push_front(Ok(Token::default()));
    }

    pub fn pos(&self) -> &ParserPos {
        &self.pos
    }

    fn update_pos(&mut self, LinePos { line, line_start }: LinePos) {
        self.pos.line = line;
        self.pos.column = self.lexer.span().end - line_start + 1;
    }

    pub fn pop_token(&mut self) -> Result<Token, Error> {
        if self.closed {
            // Closed => EOF
            return Ok(Token::default());
        }

        let next = self
            .lookahead_tokens
            .pop_front()
            .or_else(|| self.lexer.next())
            .unwrap_or_else(|| Ok(Token::default()));

        if self.lookahead_tokens.is_empty() {
            self.update_pos(self.lexer.extras.line_pos());
        }

        if next.is_err() {
            self.close();
        }

        Ok(next?)
    }

    pub fn peek_token(&mut self) -> Result<&Token, Error> {
        if self.closed {
            // We take the EOF token from the lookahead deque
            return Ok(self
                .lookahead_tokens
                .front()
                .unwrap()
                .as_ref()
                .ok()
                .unwrap());
        }

        if self.lookahead_tokens.is_empty() {
            self.lookahead_tokens
                .push_back(self.lexer.next().unwrap_or(Ok(Token::default())));
        }

        self.lookahead_tokens
            .front()
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
            lookahead_tokens: VecDeque::new(),
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
        start: Token,
        separator: Token,
        end: Token,
    ) -> ParseSequenceIter<'iter, 'source, Token, Element, Error> {
        ParseSequenceIter {
            parser: self,
            start,
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
    start: Token,
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

        let first = !self.started;

        if !self.started {
            self.started = true;
            if let Err(err) = self.parser.expect_token(self.start.clone()) {
                self.ended = true;
                return Some(Err(err));
            }
        }

        let tok = match self.parser.peek_token() {
            Ok(tok) => tok,
            Err(err) => {
                self.ended = true;
                return Some(Err(err));
            }
        };

        if *tok == self.end {
            self.ended = true;
            if let Err(err) = self.parser.expect_token(self.end.clone()) {
                return Some(Err(err));
            }
            return None;
        }

        if !first {
            if let Err(err) = self.parser.expect_token(self.separator.clone()) {
                self.ended = true;
                return Some(Err(err));
            }
        }

        Some((self.element_parser)(self.parser))
    }
}
