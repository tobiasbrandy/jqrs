#![allow(non_snake_case)]

use crate::{
    lexer::LexSource,
    parser::{ExpectationFailed, Parser, ParserPos},
};

use super::{lexer::{FilterLexError, FilterStringToken, FilterToken}, Filter};

pub struct FilterParser<'a>(Parser<'a, FilterToken, FilterParserError>);
impl<'a> FilterParser<'a> {
    pub fn new(source: &'a LexSource) -> Self {
        Self(Parser::new(source))
    }

    pub fn parse(&mut self) -> Result<Filter, FilterParserError> {
        fn inner(parser: &mut Parser<'_, FilterToken, FilterParserError>) -> Result<Filter, FilterParserError> {
            let filter = Filter(parser)?;

            let next_tok = parser.pop_token()?;

            if !matches!(next_tok, FilterToken::EOF) {
                return Err(FilterParserError::UnexpectedToken(next_tok));
            }

            Ok(filter)
        }

        let ret = inner(&mut self.0);
        self.0.close();
        ret
    }

    pub fn pos(&self) -> &ParserPos {
        self.0.pos()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FilterParserError {
    LexError(FilterLexError),
    StringParserError(FilterStringParserError),
    UnmatchedExpectation(FilterToken, FilterToken), // expected, actual
    UnexpectedToken(FilterToken),
}
impl std::fmt::Display for FilterParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterParserError::LexError(err) => write!(f, "{err}"),
            FilterParserError::StringParserError(err) => write!(f, "error parsing string: {err}"),
            FilterParserError::UnmatchedExpectation(expected, actual) => {
                write!(f, "expected {expected} got {actual}")
            }
            FilterParserError::UnexpectedToken(tok) => write!(f, "unexpected token {tok}"),
        }
    }
}
impl std::error::Error for FilterParserError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            FilterParserError::LexError(err) => Some(err),
            FilterParserError::StringParserError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<FilterLexError> for FilterParserError {
    fn from(err: FilterLexError) -> Self {
        FilterParserError::LexError(err)
    }
}
impl From<ExpectationFailed<'_, FilterToken>> for FilterParserError {
    fn from(
        ExpectationFailed {
            expected, actual, ..
        }: ExpectationFailed<'_, FilterToken>,
    ) -> Self {
        FilterParserError::UnmatchedExpectation(expected, actual)
    }
}
impl From<FilterStringParserError> for FilterParserError {
    fn from(err: FilterStringParserError) -> Self {
        FilterParserError::StringParserError(err)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FilterStringParserError {
    LexError(FilterLexError),
    UnmatchedExpectation(FilterStringToken, FilterStringToken), // expected, actual
    UnexpectedToken(FilterStringToken),
}
impl std::fmt::Display for FilterStringParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterStringParserError::LexError(err) => write!(f, "{err}"),
            FilterStringParserError::UnmatchedExpectation(expected, actual) => {
                write!(f, "expected {expected} got {actual}")
            }
            FilterStringParserError::UnexpectedToken(tok) => write!(f, "Unexpected token {tok}"),
        }
    }
}
impl std::error::Error for FilterStringParserError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            FilterStringParserError::LexError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<FilterLexError> for FilterStringParserError {
    fn from(err: FilterLexError) -> Self {
        FilterStringParserError::LexError(err)
    }
}
impl From<ExpectationFailed<'_, FilterStringToken>> for FilterStringParserError {
    fn from(
        ExpectationFailed {
            expected, actual, ..
        }: ExpectationFailed<'_, FilterStringToken>,
    ) -> Self {
        FilterStringParserError::UnmatchedExpectation(expected, actual)
    }
}

// Convinience type aliases
type FParser<'a> = Parser<'a, FilterToken, FilterParserError>;
type FResult<T> = Result<T, FilterParserError>;

fn Filter(parser: &mut FParser) -> FResult<Filter> {
    match parser.peek_token()? {
        _ => Err(FilterParserError::UnexpectedToken(parser.pop_token()?)),
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::LexSource, parser::ParserPos};

    use super::*;

    const FILTERS: &str = r#"
    [
        "",
        true,
        false,
        {
            "string": "Hello, world!",
            "number": 42,
            "object": {
                "key": "value",
                "b": [1,2]
            },
            "array": [1, "two", false, [1,2,3], {"a": 1, "b": true}],
            "boolean": true,
            "null": null
        }
    ]
    "#;

    #[test]
    fn test_parsing() {
        let source = LexSource::str(FILTERS);
        let mut parser = FilterParser::new(&source);
        match parser.parse() {
            Ok(filter) => {
                println!("{filter:?}");
            }
            Err(err) => {
                let ParserPos { line, column } = parser.pos();
                if let FilterParserError::LexError(err) = err {
                    println!("lexing error: {err} at line {line}, column {column}");
                } else {
                    println!("parsing error: {err} at line {line}, column {column}");
                }
            }
        }
    }

    #[test]
    fn test_lexing() {
        let source = LexSource::str(FILTERS);
        let mut parser = FilterParser::new(&source).0;

        let mut pos = *parser.pos();
        loop {
            let tok = parser.pop_token();
            if matches!(tok, Ok(FilterToken::EOF)) {
                break;
            }

            println!("{pos:?}");
            println!("{tok:?}");
            pos = *parser.pos();
        }
    }
}
