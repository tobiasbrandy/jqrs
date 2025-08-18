use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
    str::FromStr,
    sync::{Arc, LazyLock},
};

use fmt::{Format, format_json};

use crate::{lexer::LexSource, math::Number};

pub mod fmt;
mod lexer;
pub mod parser;

#[derive(Clone)]
pub enum Json {
    Object(im::HashMap<Arc<str>, Arc<Json>>),
    Array(im::Vector<Arc<Json>>),
    String(Arc<str>),
    Number(Number),
    Bool(bool),
    Null,
}
impl Json {
    pub fn arc_null() -> Arc<Self> {
        static ARC_NULL: LazyLock<Arc<Json>> = LazyLock::new(|| Arc::new(Json::Null));
        ARC_NULL.clone()
    }

    pub fn arc_true() -> Arc<Self> {
        static ARC_TRUE: LazyLock<Arc<Json>> = LazyLock::new(|| Arc::new(Json::Bool(true)));
        ARC_TRUE.clone()
    }

    pub fn arc_false() -> Arc<Self> {
        static ARC_FALSE: LazyLock<Arc<Json>> = LazyLock::new(|| Arc::new(Json::Bool(false)));
        ARC_FALSE.clone()
    }

    pub fn arc_bool(b: bool) -> Arc<Self> {
        if b {
            Self::arc_true()
        } else {
            Self::arc_false()
        }
    }

    pub fn arc_nan() -> Arc<Self> {
        static ARC_NAN: LazyLock<Arc<Json>> =
            LazyLock::new(|| Arc::new(Json::Number(Number::nan())));
        ARC_NAN.clone()
    }

    pub fn arc_infinity() -> Arc<Self> {
        static ARC_INFINITY: LazyLock<Arc<Json>> =
            LazyLock::new(|| Arc::new(Json::Number(Number::infinity())));
        ARC_INFINITY.clone()
    }

    pub fn arc_empty_array() -> Arc<Self> {
        static ARC_EMPTY_ARRAY: LazyLock<Arc<Json>> =
            LazyLock::new(|| Arc::new(Json::Array(im::Vector::new())));
        ARC_EMPTY_ARRAY.clone()
    }

    pub fn arc_empty_object() -> Arc<Self> {
        static ARC_EMPTY_OBJECT: LazyLock<Arc<Json>> =
            LazyLock::new(|| Arc::new(Json::Object(im::HashMap::new())));
        ARC_EMPTY_OBJECT.clone()
    }

    pub fn arc_empty_string() -> Arc<Self> {
        static ARC_EMPTY_STRING: LazyLock<Arc<Json>> =
            LazyLock::new(|| Arc::new(Json::String("".into())));
        ARC_EMPTY_STRING.clone()
    }

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
