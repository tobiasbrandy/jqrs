use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use fmt::{format_json, Format};

use crate::math::Number;

pub mod fmt;
pub mod lexer;
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
