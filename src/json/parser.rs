#![allow(non_snake_case)]

use std::collections::HashMap;

pub use crate::parser::ParserPos;
use crate::{
    lexer::LexSource,
    math::Number,
    parser::{ExpectationFailed, Parser},
};

use super::{
    lexer::{JsonLexError, JsonStringToken, JsonToken},
    Json,
};

use self_cell::self_cell;

use thiserror::Error;
use JsonToken as JT;

pub fn parse_json(source: LexSource) -> Result<Json, (ParserPos, JsonParserError)> {
    fn inner(parser: &mut Parser<'_, JsonToken, JsonParserError>) -> Result<Json, JsonParserError> {
        let json = Json(parser)?;

        let next_tok = parser.pop_token()?;

        if !matches!(next_tok, JT::EOF) {
            return Err(JsonParserError::UnexpectedToken(next_tok));
        }

        Ok(json)
    }

    let mut parser = Parser::new(&source);
    inner(&mut parser).map_err(|err| (parser.pos(), err))
}

pub fn parse_raw_json_string<'a>(source: LexSource<'a>) -> Result<String, (ParserPos, JsonParserError)> {
    let mut parser = Parser::new(&source);
    let str = RawString(&mut parser).map_err(|err| (parser.pos(), err))?;
    Ok(str)
}

type JsonParserInnerDependant<'a> = Parser<'a, JsonToken, JsonParserError>;
self_cell!(
    struct JsonParserInner<'a> {
        owner: LexSource<'a>,

        #[not_covariant]
        dependent: JsonParserInnerDependant,
    }
);
pub struct JsonParser<'a>(JsonParserInner<'a>);
impl<'a> JsonParser<'a> {
    pub fn new(source: LexSource<'a>) -> Self {
        Self(JsonParserInner::new(source, |source_ref| {
            Parser::new(source_ref)
        }))
    }

    pub fn parse_next(&mut self) -> Option<Result<Json, (ParserPos, JsonParserError)>> {
        self.0.with_dependent_mut(|_source, parser| {
            if let Ok(JT::EOF) = parser.peek_token() {
                return None;
            }
    
            let ret = Json(parser).map_err(|err| (parser.pos(), err));
            if ret.is_err() {
                // On error, we cannot continue parsing
                parser.close();
            }
    
            Some(ret)
        })
    }
}
impl Iterator for JsonParser<'_> {
    type Item = Result<Json, (ParserPos, JsonParserError)>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_next()
    }
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum JsonParserError {
    #[error(transparent)]
    LexError(#[from] JsonLexError),
    #[error(transparent)]
    ExpectationFailed(#[from] ExpectationFailed<JsonToken>),
    #[error(transparent)]
    StringParserError(#[from] JsonStringParserError),
    #[error("expected number got {0}")]
    ExpectedNumber(JsonToken),
    #[error("unexpected token {0}")]
    UnexpectedToken(JsonToken),
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum JsonStringParserError {
    #[error(transparent)]
    LexError(#[from] JsonLexError),
    #[error(transparent)]
    ExpectationFailed(#[from] ExpectationFailed<JsonStringToken>),
    #[error("unexpected token {0}")]
    UnexpectedToken(JsonStringToken),
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

fn RawString(parser: &mut JParser) -> JResult<String> {
    // Morph json parser into string parser
    let mut str_parser = parser.morph::<JsonStringToken, JsonStringParserError>();

    let mut str = String::new();
    loop {
        match str_parser.pop_token()? {
            JsonStringToken::Quote => str.push('"'),
            JsonStringToken::String(s) => str.push_str(&s),
            JsonStringToken::Escaped(c) => str.push(c),
            JsonStringToken::EOF => break,
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
        for json in JsonParser::new(LexSource::str(JSONS)) {
            match json {
                Ok(json) => {
                    println!("{json:?}");
                }
                Err((ParserPos { line, column }, err)) => {
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
        let mut parser = Parser::<JsonToken, JsonParserError>::new(&source);

        let mut pos = parser.pos();
        loop {
            let tok = parser.pop_token();
            if matches!(tok, Ok(JT::EOF)) {
                break;
            }

            println!("{pos:?}");
            println!("{tok:?}");
            pos = parser.pos();
        }
    }
}
