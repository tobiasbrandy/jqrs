#![allow(non_snake_case)]

use std::collections::HashMap;

use crate::{
    lexer::LexSource,
    math::Number,
    parser::{ExpectationFailed, Parser, ParserPos},
};

use super::{
    lexer::{JsonLexError, JsonStringToken, JsonToken},
    Json,
};

use JsonToken as JT;

pub struct JsonParser<'a>(Parser<'a, JsonToken, JsonParserError>);
impl<'a> JsonParser<'a> {
    pub fn new(source: &'a LexSource) -> Self {
        Self(Parser::new(source))
    }

    pub fn parse_next(&mut self) -> Option<Result<Json, JsonParserError>> {
        if let Ok(JT::EOF) = self.0.peek_token() {
            return None;
        }

        let ret = Json(&mut self.0);
        if ret.is_err() {
            // On error, we cannot continue parsing
            self.0.close();
        }

        Some(ret)
    }

    pub fn pos(&self) -> &ParserPos {
        self.0.pos()
    }
}
impl Iterator for JsonParser<'_> {
    type Item = Result<Json, JsonParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_next()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum JsonParserError {
    LexError(JsonLexError),
    StringParserError(JsonStringParserError),
    ExpectedNumber(JsonToken),
    UnmatchedExpectation(JsonToken, JsonToken), // expected, actual
    UnexpectedToken(JsonToken),
}
impl std::fmt::Display for JsonParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexError(err) => write!(f, "{err}"),
            Self::StringParserError(err) => write!(f, "error parsing string: {err}"),
            Self::ExpectedNumber(tok) => write!(f, "expected number got {tok}"),
            Self::UnmatchedExpectation(expected, actual) => {
                write!(f, "expected {expected} got {actual}")
            }
            Self::UnexpectedToken(tok) => write!(f, "unexpected token {tok}"),
        }
    }
}
impl std::error::Error for JsonParserError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::LexError(err) => Some(err),
            Self::StringParserError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<JsonLexError> for JsonParserError {
    fn from(err: JsonLexError) -> Self {
        Self::LexError(err)
    }
}
impl From<ExpectationFailed<'_, JsonToken>> for JsonParserError {
    fn from(
        ExpectationFailed {
            expected, actual, ..
        }: ExpectationFailed<'_, JsonToken>,
    ) -> Self {
        Self::UnmatchedExpectation(expected, actual)
    }
}
impl From<JsonStringParserError> for JsonParserError {
    fn from(err: JsonStringParserError) -> Self {
        Self::StringParserError(err)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum JsonStringParserError {
    LexError(JsonLexError),
    UnmatchedExpectation(JsonStringToken, JsonStringToken), // expected, actual
    UnexpectedToken(JsonStringToken),
}
impl std::fmt::Display for JsonStringParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexError(err) => write!(f, "{err}"),
            Self::UnmatchedExpectation(expected, actual) => {
                write!(f, "expected {expected} got {actual}")
            }
            Self::UnexpectedToken(tok) => write!(f, "Unexpected token {tok}"),
        }
    }
}
impl std::error::Error for JsonStringParserError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::LexError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<JsonLexError> for JsonStringParserError {
    fn from(err: JsonLexError) -> Self {
        Self::LexError(err)
    }
}
impl From<ExpectationFailed<'_, JsonStringToken>> for JsonStringParserError {
    fn from(
        ExpectationFailed {
            expected, actual, ..
        }: ExpectationFailed<'_, JsonStringToken>,
    ) -> Self {
        Self::UnmatchedExpectation(expected, actual)
    }
}

// Convinience type aliases
type JParser<'a> = Parser<'a, JsonToken, JsonParserError>;
type JResult<T> = Result<T, JsonParserError>;

fn Json(parser: &mut JParser) -> JResult<Json> {
    match parser.peek_token()? {
        JT::LBrack => Array(parser).map(Json::Array),
        JT::LBrace => Object(parser).map(Json::Object),
        JT::Quote => String(parser).map(Json::String),
        JT::Num(_) => Number(parser).map(Json::Number),
        JT::True => parser.pop_and_produce(Json::Bool(true)),
        JT::False => parser.pop_and_produce(Json::Bool(false)),
        JT::Null => parser.pop_and_produce(Json::Null),
        JT::NaN => parser.pop_and_produce(Json::Number(Number::nan())),
        JT::InfP => parser.pop_and_produce(Json::Number(Number::infinity())),
        JT::InfM => parser.pop_and_produce(Json::Number(Number::neg_infinity())),
        _ => Err(JsonParserError::UnexpectedToken(parser.pop_token()?)),
    }
}

fn Array(parser: &mut JParser) -> JResult<Vec<Json>> {
    parser.expect_token(JT::LBrack)?;

    let ret = parser
        .parse_sequence(Json, JT::Comma, JT::RBrack)
        .collect::<Result<_, _>>()?;

    parser.expect_token(JT::RBrack)?;

    Ok(ret)
}

fn Object(parser: &mut JParser) -> JResult<HashMap<String, Json>> {
    parser.expect_token(JT::LBrace)?;

    let ret = parser
        .parse_sequence(ObjectElement, JT::Comma, JT::RBrace)
        .collect::<Result<_, _>>()?;

    parser.expect_token(JT::RBrace)?;

    Ok(ret)
}

fn ObjectElement(parser: &mut JParser) -> JResult<(String, Json)> {
    let key = String(parser)?;

    parser.expect_token(JT::KVDelim)?;

    let val = Json(parser)?;

    Ok((key, val))
}

fn String(parser: &mut JParser) -> JResult<String> {
    parser.expect_token(JT::Quote)?;

    // Morph json parser into string parser
    let mut str_parser = parser.morph::<JsonStringToken, JsonStringParserError>();

    let mut str = String::new();
    loop {
        match str_parser.pop_token()? {
            JsonStringToken::Quote => break,
            JsonStringToken::String(s) => str.push_str(&s),
            JsonStringToken::Escaped(c) => str.push(c),
            tok => return Err(JsonStringParserError::UnexpectedToken(tok).into()),
        }
    }

    // Restore json parser
    str_parser.morph_into(parser);

    Ok(str)
}

fn Number(parser: &mut JParser) -> JResult<Number> {
    let tok = parser.pop_token()?;
    if let JT::Num(n) = tok {
        Ok(n)
    } else {
        Err(JsonParserError::ExpectedNumber(tok))
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::LexSource, parser::ParserPos};

    use super::*;

    const JSONS: &str = r#"
    []
    ""
    "hola"
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
    true
    "Hello, world!"
    {"key": "value", "b": [1,2]}
    "#;

    #[test]
    fn test_parsing() {
        let source = LexSource::str(JSONS);
        let mut parser = JsonParser::new(&source);
        while let Some(json) = parser.next() {
            match json {
                Ok(json) => {
                    println!("{json:?}");
                }
                Err(err) => {
                    let ParserPos { line, column } = parser.pos();
                    if let JsonParserError::LexError(err) = err {
                        println!("lexing error: {err} at line {line}, column {column}");
                    } else {
                        println!("parsing error: {err} at line {line}, column {column}");
                    }
                }
            }
        }
    }

    #[test]
    fn test_lexing() {
        let source = LexSource::str(JSONS);
        let mut parser = JsonParser::new(&source).0;

        let mut pos = *parser.pos();
        loop {
            let tok = parser.pop_token();
            if matches!(tok, Ok(JT::EOF)) {
                break;
            }

            println!("{pos:?}");
            println!("{tok:?}");
            pos = *parser.pos();
        }
    }
}
