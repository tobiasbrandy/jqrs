#![allow(non_snake_case)]

use std::sync::Arc;

pub use crate::parser::ParserPos;
use crate::{
    lexer::LexSource,
    math::Number,
    parser::{ExpectationFailed, Parser},
};

use super::{
    Json,
    lexer::{JsonLexError, JsonStringToken, JsonToken},
};

use self_cell::self_cell;

use JsonToken as JT;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Error)]
#[error("{source}, line {}, column {}", .pos.line, .pos.column)]
pub struct JsonParserError {
    pub pos: ParserPos,
    pub source: JsonParserErrorSource,
}

pub fn parse_json(source: LexSource) -> Result<Json, JsonParserError> {
    fn inner(
        parser: &mut Parser<'_, JsonToken, JsonParserErrorSource>,
    ) -> Result<Json, JsonParserErrorSource> {
        let json = Json(parser)?;

        let next_tok = parser.pop_token()?;

        if !matches!(next_tok, JT::EOF) {
            return Err(JsonParserErrorSource::UnexpectedToken(next_tok));
        }

        Ok(json)
    }

    let mut parser = Parser::new(&source);
    inner(&mut parser).map_err(|source| JsonParserError {
        pos: parser.pos(),
        source,
    })
}

pub fn parse_raw_json_string<'a>(
    source: LexSource<'a>,
) -> Result<String, JsonParserError> {
    let mut parser = Parser::new(&source);
    let str = RawString(&mut parser).map_err(|source| JsonParserError {
        pos: parser.pos(),
        source,
    })?;
    Ok(str)
}

type JsonParserInnerDependant<'a> = Parser<'a, JsonToken, JsonParserErrorSource>;
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

    pub fn parse_next(&mut self) -> Option<Result<Json, JsonParserError>> {
        self.0.with_dependent_mut(|_source, parser| {
            if let Ok(JT::EOF) = parser.peek_token() {
                return None;
            }

            let ret = Json(parser).map_err(|source| JsonParserError {
                pos: parser.pos(),
                source,
            });
            if ret.is_err() {
                // On error, we cannot continue parsing
                parser.close();
            }

            Some(ret)
        })
    }
}
impl Iterator for JsonParser<'_> {
    type Item = Result<Json, JsonParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_next()
    }
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum JsonParserErrorSource {
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
type ParserError = JsonParserErrorSource;
type JParser<'a> = Parser<'a, JsonToken, ParserError>;
type JResult<T> = Result<T, ParserError>;

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
        _ => Err(ParserError::UnexpectedToken(parser.pop_token()?)),
    }
}

fn Array(parser: &mut JParser) -> JResult<im::Vector<Arc<Json>>> {
    parser.expect_token(JT::LBrack)?;

    let ret = parser
        .parse_sequence(Json, JT::Comma, JT::RBrack)
        .map(|r| r.map(Arc::new))
        .collect::<Result<_, _>>()?;

    parser.expect_token(JT::RBrack)?;

    Ok(ret)
}

fn Object(parser: &mut JParser) -> JResult<im::HashMap<Arc<str>, Arc<Json>>> {
    parser.expect_token(JT::LBrace)?;

    let ret = parser
        .parse_sequence(ObjectElement, JT::Comma, JT::RBrace)
        .collect::<Result<_, _>>()?;

    parser.expect_token(JT::RBrace)?;

    Ok(ret)
}

fn ObjectElement(parser: &mut JParser) -> JResult<(Arc<str>, Arc<Json>)> {
    let key = String(parser)?;

    parser.expect_token(JT::KVDelim)?;

    let val = Json(parser)?.into();

    Ok((key, val))
}

fn String(parser: &mut JParser) -> JResult<Arc<str>> {
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

    Ok(str.into())
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
        Err(ParserError::ExpectedNumber(tok))
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::LexSource;

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
                Err(err) => println!("{err}"),
            }
        }
    }

    #[test]
    fn test_lexing() {
        let source = LexSource::str(JSONS);
        let mut parser = Parser::<JsonToken, ParserError>::new(&source);

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
