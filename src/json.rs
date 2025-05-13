use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    ops::Deref,
    str::FromStr,
    sync::Arc,
};

use fmt::{format_json, Format};

use crate::{lexer::LexSource, math::Number};

pub mod fmt;
mod lexer;
pub mod parser;

#[derive(Debug, Clone, PartialEq)]
pub enum JsonRef {
    Owned(Json),
    Shared(Arc<Json>),
}
impl From<Json> for JsonRef {
    fn from(j: Json) -> Self {
        JsonRef::Owned(j)
    }
}
impl From<Arc<Json>> for JsonRef {
    fn from(j: Arc<Json>) -> Self {
        JsonRef::Shared(j)
    }
}
impl JsonRef {
    pub fn shareable(&mut self) -> &mut Self {
        if let Self::Owned(j) = self {
            *self = JsonRef::Shared(Arc::from(std::mem::replace(j, Json::Null)));
        }

        self
    }
}
impl Deref for JsonRef {
    type Target = Json;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Owned(j) => j,
            Self::Shared(j) => j.as_ref(),
        }
    }
}
impl AsRef<Json> for JsonRef {
    fn as_ref(&self) -> &Json {
        self.deref()
    }
}
impl std::fmt::Display for JsonRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self, f)
    }
}

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

    pub fn to_bool(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            Self::Null => false,
            _ => true,
        }
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
            None => Err(parser::JsonParserError::UnexpectedToken(
                lexer::JsonToken::EOF,
            )),
        }
    }
}
