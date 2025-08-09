use std::{
    cmp::Ordering,
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

#[derive(Clone)]
pub enum Json {
    Object(HashMap<String, Json>),
    Array(Vec<Json>),
    String(String),
    Number(Number),
    Bool(bool),
    Null,
}
impl Json {
    pub fn parser(source: LexSource) -> parser::JsonParser {
        parser::JsonParser::new(source)
    }

    pub fn format(&self, fmt: &Format) -> String {
        format_json(self, fmt)
    }

    pub fn format_compact(&self) -> String {
        format_json(self, &Format::COMPACT)
    }

    pub fn format_pretty(&self) -> String {
        format_json(self, &Format::PRETTY)
    }

    pub fn format_pretty_color(&self) -> String {
        format_json(self, &Format::pretty_color())
    }

    pub fn to_bool(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            Self::Null => false,
            _ => true,
        }
    }

    pub fn is_nan(&self) -> bool {
        match self {
            Self::Number(n) => n.is_nan(),
            _ => false,
        }
    }
    pub fn is_infinite(&self) -> bool {
        match self {
            Self::Number(n) => n.is_infinite(),
            _ => false,
        }
    }
}
// TODO: Actually follow display flags
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
        parser::parse_json(LexSource::str(s))
    }
}
impl Ord for Json {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            // null
            (Json::Null, Json::Null) => Ordering::Equal,
            (Json::Null, _) => Ordering::Less,
            (_, Json::Null) => Ordering::Greater,
            // false
            (Json::Bool(false), Json::Bool(false)) => Ordering::Equal,
            (Json::Bool(false), _) => Ordering::Less,
            (_, Json::Bool(false)) => Ordering::Greater,
            // true
            (Json::Bool(true), Json::Bool(true)) => Ordering::Equal,
            (Json::Bool(true), _) => Ordering::Less,
            (_, Json::Bool(true)) => Ordering::Greater,
            // number
            (Json::Number(l), Json::Number(r)) => l.cmp(r),
            (Json::Number(_), _) => Ordering::Less,
            (_, Json::Number(_)) => Ordering::Greater,
            // string
            (Json::String(l), Json::String(r)) => l.cmp(r),
            (Json::String(_), _) => Ordering::Less,
            (_, Json::String(_)) => Ordering::Greater,
            // array
            (Json::Array(l), Json::Array(r)) => l.cmp(r),
            (Json::Array(_), _) => Ordering::Less,
            (_, Json::Array(_)) => Ordering::Greater,
            // object
            (Json::Object(l), Json::Object(r)) => {
                let mut l = l.iter().collect::<Vec<_>>();
                l.sort();

                let mut r = r.iter().collect::<Vec<_>>();
                r.sort();

                let key_ord = l.iter().map(|(k, _)| k).cmp(r.iter().map(|(k, _)| k));
                if key_ord == Ordering::Equal {
                    l.iter().map(|(_, v)| v).cmp(r.iter().map(|(_, v)| v))
                } else {
                    key_ord
                }
            }
        }
    }
}
impl PartialOrd for Json {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialEq for Json {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}
impl Eq for Json {}
