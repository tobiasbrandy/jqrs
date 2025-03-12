#![allow(non_snake_case)]

use crate::{
    lexer::LexSource,
    math::Number,
    parser::{ExpectationFailed, Parser, ParserPos},
};

use super::{
    lexer::{JsonLexError, JsonToken},
    Json,
};

pub struct JsonParser<'a>(Parser<'a, JsonToken, JsonParserError>);
impl<'a> JsonParser<'a> {
    pub fn new(source: &'a LexSource) -> Self {
        Self(Parser::new(source))
    }

    pub fn parse_next(&mut self) -> Option<Result<Json, JsonParserError>> {
        if let Ok(JsonToken::EOF) = self.0.peek_token() {
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
impl From<ExpectationFailed<'_, JsonToken>> for JsonParserError {
    fn from(
        ExpectationFailed {
            expected, actual, ..
        }: ExpectationFailed<'_, JsonToken>,
    ) -> Self {
        JsonParserError::UnmatchedExpectation(expected, actual)
    }
}

// Convinience type aliases
type JParser<'a> = Parser<'a, JsonToken, JsonParserError>;
type JResult<T> = Result<T, JsonParserError>;

fn expect_str(parser: &mut JParser) -> JResult<String> {
    let tok = parser.pop_token()?;
    if let JsonToken::Str(s) = tok {
        Ok(s)
    } else {
        Err(JsonParserError::ExpectingString(tok))
    }
}

fn expect_num(parser: &mut JParser) -> JResult<Number> {
    let tok = parser.pop_token()?;
    if let JsonToken::Num(n) = tok {
        Ok(n)
    } else {
        Err(JsonParserError::ExpectingNumber(tok))
    }
}

fn Json(parser: &mut JParser) -> JResult<Json> {
    match parser.peek_token()? {
        JsonToken::LBrack => Array(parser),
        JsonToken::LBrace => Object(parser),
        JsonToken::Str(_) => Ok(Json::String(expect_str(parser)?)),
        JsonToken::Num(_) => Ok(Json::Number(expect_num(parser)?)),
        JsonToken::True => parser.pop_and_produce(Json::Bool(true)),
        JsonToken::False => parser.pop_and_produce(Json::Bool(false)),
        JsonToken::Null => parser.pop_and_produce(Json::Null),
        JsonToken::NaN => parser.pop_and_produce(Json::Number(Number::nan())),
        JsonToken::InfP => parser.pop_and_produce(Json::Number(Number::infinity())),
        JsonToken::InfM => parser.pop_and_produce(Json::Number(Number::neg_infinity())),
        _ => Err(JsonParserError::UnexpectedToken(parser.pop_token()?)),
    }
}

fn Array(parser: &mut JParser) -> JResult<Json> {
    Ok(Json::Array(
        parser
            .parse_sequence(Json, JsonToken::LBrack, JsonToken::Comma, JsonToken::RBrack)
            .collect::<Result<_, _>>()?,
    ))
}

fn Object(parser: &mut JParser) -> JResult<Json> {
    Ok(Json::Object(
        parser
            .parse_sequence(
                ObjectElement,
                JsonToken::LBrace,
                JsonToken::Comma,
                JsonToken::RBrace,
            )
            .collect::<Result<_, _>>()?,
    ))
}

fn ObjectElement(parser: &mut JParser) -> JResult<(String, Json)> {
    let key = expect_str(parser)?;

    parser.expect_token(JsonToken::KVDelim)?;

    let val = Json(parser)?;

    Ok((key, val))
}

#[cfg(test)]
mod tests {
    use crate::{lexer::LexSource, parser::ParserPos};

    use super::*;

    const JSONS: &str = r#"
    []
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
        let mut parser = JsonParser::new(&source).0;

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
