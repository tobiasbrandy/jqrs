use std::collections::VecDeque;

use logos::Lexer;

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

pub struct Parser<'source, Token>
where
    Token: LexToken<'source>,
    Token::Extras: LexState,
{
    lexer: Lexer<'source, Token>,
    lookahead_tokens: VecDeque<Result<Token, Token::Error>>,
    pos: ParserPos,
    closed: bool,
}

impl<'source, Token> Parser<'source, Token>
where
    Token: LexToken<'source>,
    Token::Extras: LexState,
{
    pub fn new(source: &'source Token::Source) -> Self {
        Self {
            lexer: Token::lexer(source),
            lookahead_tokens: VecDeque::new(),
            pos: ParserPos::default(),
            closed: false,
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

    pub fn pop_token(&mut self) -> Result<Token, Token::Error> {
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

        next
    }

    pub fn peek_token(&mut self) -> Result<&Token, Token::Error> {
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
            .map_err(|err| err.clone())
    }
}
