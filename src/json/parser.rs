use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use crate::{lexer::LexSource, math::Number, parser::Parser};

use super::{
    lexer::{JsonLexError, JsonToken},
    Json,
};

pub struct JsonParser<'a>(Parser<'a, JsonToken>);
impl<'a> JsonParser<'a> {
    pub fn new(source: &'a LexSource) -> Self {
        Self(Parser::new(source))
    }

    pub fn parse_next(&mut self) -> Option<Result<Json, JsonParserError>> {
        if let Ok(JsonToken::EOF) = self.peek_token() {
            return None;
        }

        let ret = parse_json(self);
        if ret.is_err() {
            // On error, we cannot continue parsing
            self.close();
        }

        Some(ret)
    }
}
impl<'a> Deref for JsonParser<'a> {
    type Target = Parser<'a, JsonToken>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for JsonParser<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
    ExpectingString(JsonToken),
    ExpectingNumber(JsonToken),
    UnmatchedExpectation(JsonToken, JsonToken), // expected, actual
    UnexpectedToken(JsonToken),
}
impl std::fmt::Display for JsonParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsonParserError::LexError(err) => write!(f, "{err}"),
            JsonParserError::ExpectingString(tok) => write!(f, "Expected string got {tok}"),
            JsonParserError::ExpectingNumber(tok) => write!(f, "Expected number got {tok}"),
            JsonParserError::UnmatchedExpectation(expected, actual) => {
                write!(f, "expected {expected} got {actual}")
            }
            JsonParserError::UnexpectedToken(tok) => write!(f, "Unexpected token {tok}"),
        }
    }
}
impl std::error::Error for JsonParserError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            JsonParserError::LexError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<JsonLexError> for JsonParserError {
    fn from(err: JsonLexError) -> Self {
        JsonParserError::LexError(err)
    }
}

fn pop_and_produce(parser: &mut JsonParser, val: Json) -> Result<Json, JsonParserError> {
    parser.pop_token()?;
    Ok(val)
}

fn expect_token(parser: &mut JsonParser, expecting: JsonToken) -> Result<(), JsonParserError> {
    let actual = parser.pop_token()?;
    if expecting != actual {
        Err(JsonParserError::UnmatchedExpectation(expecting, actual))
    } else {
        Ok(())
    }
}

fn expect_str(parser: &mut JsonParser) -> Result<String, JsonParserError> {
    let tok = parser.pop_token()?;
    if let JsonToken::Str(s) = tok {
        Ok(s)
    } else {
        Err(JsonParserError::ExpectingString(tok))
    }
}

fn expect_num(parser: &mut JsonParser) -> Result<Number, JsonParserError> {
    let tok = parser.pop_token()?;
    if let JsonToken::Num(n) = tok {
        Ok(n)
    } else {
        Err(JsonParserError::ExpectingNumber(tok))
    }
}

fn parse_json(parser: &mut JsonParser) -> Result<Json, JsonParserError> {
    match parser.peek_token()? {
        JsonToken::LBrack => parse_array(parser),
        JsonToken::LBrace => parse_object(parser),
        JsonToken::Str(_) => Ok(Json::String(expect_str(parser)?)),
        JsonToken::Num(_) => Ok(Json::Number(expect_num(parser)?)),
        JsonToken::True => pop_and_produce(parser, Json::Bool(true)),
        JsonToken::False => pop_and_produce(parser, Json::Bool(false)),
        JsonToken::Null => pop_and_produce(parser, Json::Null),
        JsonToken::NaN => pop_and_produce(parser, Json::Number(Number::nan())),
        JsonToken::InfP => pop_and_produce(parser, Json::Number(Number::infinity())),
        JsonToken::InfM => pop_and_produce(parser, Json::Number(Number::neg_infinity())),
        _ => Err(JsonParserError::UnexpectedToken(parser.pop_token()?)),
    }
}

fn parse_array(parser: &mut JsonParser) -> Result<Json, JsonParserError> {
    expect_token(parser, JsonToken::LBrack)?;

    let mut arr = Vec::new();
    if !matches!(parser.peek_token()?, JsonToken::RBrack) {
        loop {
            arr.push(parse_json(parser)?);

            if matches!(parser.peek_token()?, JsonToken::RBrack) {
                break;
            }

            expect_token(parser, JsonToken::Comma)?;
        }
    }

    expect_token(parser, JsonToken::RBrack)?;

    Ok(Json::Array(arr))
}

fn parse_object(parser: &mut JsonParser) -> Result<Json, JsonParserError> {
    expect_token(parser, JsonToken::LBrace)?;

    let mut object = HashMap::new();
    if !matches!(parser.peek_token()?, JsonToken::RBrace) {
        loop {
            let key = expect_str(parser)?;

            expect_token(parser, JsonToken::KVDelim)?;

            let val = parse_json(parser)?;

            object.insert(key, val);

            if matches!(parser.peek_token()?, JsonToken::RBrace) {
                break;
            }

            expect_token(parser, JsonToken::Comma)?;
        }
    }

    expect_token(parser, JsonToken::RBrace)?;

    Ok(Json::Object(object))
}

#[cfg(test)]
mod tests {
    use crate::{lexer::LexSource, parser::ParserPos};

    use super::*;

    const JSONS: &str = r#"
    ""
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
        let source = LexSource::String(JSONS.to_string());
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
        let source = LexSource::String(JSONS.to_string());
        let mut parser = JsonParser::new(&source);

        let mut pos = *parser.pos();
        loop {
            let tok = parser.pop_token();
            if matches!(tok, Ok(JsonToken::EOF)) {
                break;
            }

            println!("{pos:?}");
            println!("{tok:?}");
            pos = *parser.pos();
        }
    }
}
