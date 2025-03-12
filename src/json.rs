use std::{
    collections::HashMap,
    fmt::{Debug, Display}, str::FromStr,
};

use fmt::{format_json, Format};

use crate::{lexer::LexSource, math::Number};

pub mod fmt;
mod lexer;
pub mod parser;

#[derive(Clone, PartialEq)]
pub enum Json {
    Object(HashMap<String, Json>),
    Array(Vec<Json>),
    String(String),
    Number(Number),
    Bool(bool),
    Null,
}
impl Json {
    pub fn parser<'a>(source: &'a LexSource) -> parser::JsonParser<'a> {
        parser::JsonParser::new(source)
    }

    pub fn format(&self, format: Format) -> String {
        format_json(self, format)
    }

    pub fn format_compact(&self) -> String {
        format_json(self, Format::COMPACT)
    }

    pub fn format_pretty(&self) -> String {
        format_json(self, Format::PRETTY)
    }

    pub fn format_pretty_color(&self) -> String {
        format_json(self, Format::pretty_color())
    }
}
impl Debug for Json {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.format_pretty())
    }
}
impl Display for Json {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.format_compact())
    }
}
impl FromStr for Json {
    type Err = parser::JsonParserError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match Json::parser(&LexSource::str(s)).parse_next() {
            Some(ret) => ret,
            None => Err(parser::JsonParserError::UnexpectedToken(lexer::JsonToken::EOF)),
        }
    }
}
