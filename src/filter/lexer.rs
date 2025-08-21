use std::{ops::Deref, str::FromStr, sync::Arc};

use derive_more::Display;
use logos::Logos;
use rug::float::ParseFloatError;
use thiserror::Error;

use crate::{
    lexer::{parse, parse_escaped, register_newline, register_tab, LexSource, LinePos},
    math::Number,
};

#[derive(Debug, Clone, PartialEq, Eq, Default, Error)]
pub enum FilterLexError {
    #[error(transparent)]
    NumberParseError(#[from] ParseFloatError),
    #[error("invalid escape sequence {0}")]
    InvalidEscapeSeq(String),
    #[error("invalid operator {0}")]
    InvalidOperator(String),
    #[default]
    #[error("invalid token")]
    InvalidToken,
}

#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum Op {
    // Arithmetic Operators
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("*")]
    Times,
    #[display("/")]
    Div,
    #[display("%")]
    Mod,

    // Flow Operators
    #[display("|")]
    Pipe,
    #[display("//")]
    Alt,
    #[display("?")]
    Opt,
    #[display("?//")]
    OptAlt,
    #[display(",")]
    Comma,

    // Assignment Operators
    #[display("=")]
    Assign,
    #[display("+=")]
    PlusA,
    #[display("-=")]
    MinusA,
    #[display("*=")]
    TimesA,
    #[display("/=")]
    DivA,
    #[display("%=")]
    ModA,
    #[display("|=")]
    PipeA,
    #[display("//=")]
    AltA,

    // Comparison Operators
    #[display("==")]
    Eq,
    #[display("!=")]
    Neq,
    #[display("<")]
    Lt,
    #[display("<=")]
    Le,
    #[display(">")]
    Gt,
    #[display(">=")]
    Ge,
    #[display("or")]
    Or,
    #[display("and")]
    And,
}
impl FromStr for Op {
    type Err = FilterLexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Self::Plus),
            "-" => Ok(Self::Minus),
            "*" => Ok(Self::Times),
            "/" => Ok(Self::Div),
            "%" => Ok(Self::Mod),
            "|" => Ok(Self::Pipe),
            "//" => Ok(Self::Alt),
            "?" => Ok(Self::Opt),
            "?//" => Ok(Self::OptAlt),
            "," => Ok(Self::Comma),
            "=" => Ok(Self::Assign),
            "+=" => Ok(Self::PlusA),
            "-=" => Ok(Self::MinusA),
            "*=" => Ok(Self::TimesA),
            "/=" => Ok(Self::DivA),
            "%=" => Ok(Self::ModA),
            "|=" => Ok(Self::PipeA),
            "//=" => Ok(Self::AltA),
            "==" => Ok(Self::Eq),
            "!=" => Ok(Self::Neq),
            "<" => Ok(Self::Lt),
            "<=" => Ok(Self::Le),
            ">" => Ok(Self::Gt),
            ">=" => Ok(Self::Ge),
            "or" => Ok(Self::Or),
            "and" => Ok(Self::And),
            _ => Err(FilterLexError::InvalidToken),
        }
    }
}

#[derive(Logos, Debug, Clone, PartialEq, Default, Display)]
#[logos(extras = LinePos)]
#[logos(error = FilterLexError)]
#[logos(source = LexSource<'s>)]
#[logos(skip r"[\ \f\v\r\uFEFF]+")]
pub enum FilterToken {
    // Literals
    #[token("true")]
    #[display("true")]
    True,

    #[token("false")]
    #[display("false")]
    False,

    #[token("null")]
    #[display("null")]
    Null,

    #[regex(r#"([0-9]+\.?|[0-9]*\.[0-9]+)([eE][\+\-]?[0-9]+)?"#, |lex| lex.slice().parse())]
    #[display("{_0}")]
    Num(Number),

    // Keywords
    #[token("module")]
    #[display("module")]
    Module,

    #[token("import")]
    #[display("import")]
    Import,

    #[token("include")]
    #[display("include")]
    Include,

    #[token("def")]
    #[display("def")]
    Def,

    #[token("as")]
    #[display("as")]
    As,

    #[token("if")]
    #[display("if")]
    If,

    #[token("then")]
    #[display("then")]
    Then,

    #[token("else")]
    #[display("else")]
    Else,

    #[token("elif")]
    #[display("elif")]
    Elif,

    #[token("end")]
    #[display("end")]
    End,

    #[token("reduce")]
    #[display("reduce")]
    Reduce,

    #[token("foreach")]
    #[display("foreach")]
    Foreach,

    #[token("try")]
    #[display("try")]
    Try,

    #[token("catch")]
    #[display("catch")]
    Catch,

    #[token("label")]
    #[display("label")]
    Label,

    #[token("break")]
    #[display("break")]
    Break,

    #[token("__loc__")]
    #[display("__loc__")]
    Loc,

    // Arithmetic Operators
    #[token("+", parse)]
    #[token("-", parse)]
    #[token("*", parse)]
    #[token("/", parse)]
    #[token("%", parse)]
    // Flow Operators
    #[token("|", parse)]
    #[token("//", parse)]
    #[token("?", parse)]
    #[token("?//", parse)]
    #[token(",", parse)]
    // Assignment Operators
    #[token("=", parse)]
    #[token("+=", parse)]
    #[token("-=", parse)]
    #[token("*=", parse)]
    #[token("/=", parse)]
    #[token("%=", parse)]
    #[token("|=", parse)]
    #[token("//=", parse)]
    // Comparison Operators
    #[token("==", parse)]
    #[token("!=", parse)]
    #[token("<", parse)]
    #[token("<=", parse)]
    #[token(">", parse)]
    #[token(">=", parse)]
    #[token("or", parse)]
    #[token("and", parse)]
    #[display("{_0}")]
    Op(Op),

    // Special Filters
    #[token(".")]
    #[display(".")]
    Dot,

    #[token("..")]
    #[display("..")]
    Recr,

    // Parentheses
    #[token("(")]
    #[display("(")]
    LPar,

    #[token(")")]
    #[display(")")]
    RPar,

    // Lists
    #[token("[")]
    #[display("[")]
    LBrack,

    #[token("]")]
    #[display("]")]
    RBrack,

    // Objects
    #[token("{")]
    #[display("{{")]
    LBrace,

    #[token("}")]
    #[display("}}")]
    RBrace,

    #[token(":")]
    #[display(":")]
    Colon,

    // Params
    #[token(";")]
    #[display(";")]
    Semicolon,

    // Variables
    #[token("$")]
    #[display("$")]
    Var,

    #[token("\"")]
    #[display("\"")]
    Quote,

    // Identifiers
    #[regex(r#"([a-zA-Z_][a-zA-Z_0-9]*::)*[a-zA-Z_][a-zA-Z_0-9]*"#, |lex| Arc::from(lex.slice().deref()))]
    #[display("{_0}")]
    Id(Arc<str>),

    #[regex(r#"\.[a-zA-Z_][a-zA-Z_0-9]*"#, |lex| Arc::from(&lex.slice()[1..]))]
    #[display(".{_0}")]
    Field(Arc<str>),

    #[regex(r#"@[a-zA-Z0-9_]+"#, |lex| Arc::from(&lex.slice()[1..]))]
    #[display("@{_0}")]
    Format(Arc<str>),

    // Control
    // TODO: backlash comment continuing
    #[regex(r"#[^\n]*", logos::skip)]
    #[display("")]
    _Comment,

    #[token("\n", register_newline)]
    #[display(r#"\n"#)]
    _Newline,

    #[token("\t", register_tab)]
    #[display(r#"\t"#)]
    _Tab,

    #[default]
    #[display("<EOF>")]
    #[allow(clippy::upper_case_acronyms)]
    EOF,
}

#[derive(Logos, Debug, Clone, PartialEq, Default, Display)]
#[logos(extras = LinePos)]
#[logos(error = FilterLexError)]
#[logos(source = LexSource<'s>)]
pub enum FilterStringToken {
    #[token("\"")]
    #[display("\"")]
    Quote,

    #[regex(r#"[^\"\\\x00-\x1F\x7F]+"#, |lex| lex.slice().to_string())]
    #[display("{_0}")]
    String(String),

    #[regex(r#"\\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})"#, |lex| parse_escaped(&lex.slice()).map_err(FilterLexError::InvalidEscapeSeq))]
    #[display("\\{_0}")]
    Escaped(char),

    #[token("\\(")]
    #[display("\\(")]
    Interpolation,

    #[default]
    #[display("<EOF>")]
    #[allow(clippy::upper_case_acronyms)]
    EOF,
}

#[cfg(test)]
mod tests {
    use super::*;

    const FILTER: &str = r#"
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
    "#;

    #[test]
    fn test_lexer() {
        let source = LexSource::String(FILTER.to_string());
        let mut lexer = FilterToken::lexer(&source);

        for token in lexer.by_ref() {
            println!("{token:?}");
        }

        let state = lexer.extras;
        println!("{state:#?}");
    }
}
