use logos::Logos;
use rug::float::ParseFloatError;

use crate::{
    lexer::{parse_escaped, register_newline, register_tab, LexSource, LinePos},
    math::{parse_number, Number},
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum FilterLexError {
    NumberParseError(ParseFloatError),
    InvalidEscapeSeq(String),
    #[default]
    InvalidToken,
}
impl std::fmt::Display for FilterLexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterLexError::NumberParseError(err) => write!(f, "{err}"),
            FilterLexError::InvalidEscapeSeq(s) => write!(f, "invalid escape sequence {s}"),
            FilterLexError::InvalidToken => write!(f, "invalid token"),
        }
    }
}
impl std::error::Error for FilterLexError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            FilterLexError::NumberParseError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<ParseFloatError> for FilterLexError {
    fn from(err: ParseFloatError) -> Self {
        FilterLexError::NumberParseError(err)
    }
}

#[derive(Logos, Debug, Clone, PartialEq, Default)]
#[logos(extras = LinePos)]
#[logos(error = FilterLexError)]
#[logos(source = LexSource<'s>)]
#[logos(skip r"[\ \f\v\r\uFEFF]+")]
pub enum FilterToken {
    // Literals
    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("null")]
    Null,

    #[regex(r#"([0-9]+\.?|[0-9]*\.[0-9]+)([eE][\+\-]?[0-9]+)?"#, |lex| parse_number(&lex.slice()))]
    Num(Number),

    // Keywords
    #[token("module")]
    Module,

    #[token("import")]
    Import,

    #[token("include")]
    Include,

    #[token("def")]
    Def,

    #[token("as")]
    As,

    #[token("if")]
    If,

    #[token("then")]
    Then,

    #[token("else")]
    Else,

    #[token("elif")]
    Elif,

    #[token("end")]
    End,

    #[token("reduce")]
    Reduce,

    #[token("foreach")]
    Foreach,

    #[token("try")]
    Try,

    #[token("catch")]
    Catch,

    #[token("label")]
    Label,

    #[token("break")]
    Break,

    #[token("__loc__")]
    Loc,

    // Arithmetic Operators
    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Times,

    #[token("/")]
    Div,

    #[token("%")]
    Mod,

    // Flow Operators
    #[token("|")]
    Pipe,

    #[token("//")]
    Alt,

    #[token("?")]
    Opt,

    #[token("?//")]
    OptAlt,

    #[token(",")]
    Comma,

    // Assignment Operators
    #[token("=")]
    Assign,

    #[token("+=")]
    PlusA,

    #[token("-=")]
    MinusA,

    #[token("*=")]
    TimesA,

    #[token("/=")]
    DivA,

    #[token("%=")]
    ModA,

    #[token("|=")]
    PipeA,

    #[token("//=")]
    AltA,

    // Comparison Operators
    #[token("==")]
    Eq,

    #[token("!=")]
    Neq,

    #[token("<")]
    Lt,

    #[token("<=")]
    Le,

    #[token(">")]
    Gt,

    #[token(">=")]
    Ge,

    #[token("or")]
    Or,

    #[token("and")]
    And,

    // Special Filters
    #[token(".")]
    Dot,

    #[token("..")]
    Recr,

    // Parentheses
    #[token("(")]
    LPar,

    #[token(")")]
    RPar,

    // Lists
    #[token("[")]
    LBrack,

    #[token("]")]
    RBrack,

    // Objects
    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token(":")]
    KVDelim,

    // Params
    #[token(";")]
    ArgDelim,

    // Variables
    #[token("$")]
    Var,

    #[token("\"")]
    Quote,

    // Identifiers
    #[regex(r#"([a-zA-Z_][a-zA-Z_0-9]*::)*[a-zA-Z_][a-zA-Z_0-9]*"#, |lex| lex.slice().to_string())]
    Id(String),

    #[regex(r#"\.[a-zA-Z_][a-zA-Z_0-9]*"#, |lex| lex.slice()[1..].to_string())]
    Field(String),

    #[regex(r#"@[a-zA-Z0-9_]+"#, |lex| lex.slice()[1..].to_string())]
    Format(String),

    // Control
    #[regex(r"#.*\n", register_newline)]
    #[regex(r"#.*\r", logos::skip)]
    _Comment,

    #[token("\n", register_newline)]
    _Newline,

    #[token("\t", register_tab)]
    _Tab,

    #[allow(clippy::upper_case_acronyms)]
    #[default]
    EOF,
}

#[derive(Logos, Debug, Clone, PartialEq, Default)]
#[logos(extras = LinePos)]
#[logos(error = FilterLexError)]
#[logos(source = LexSource<'s>)]
pub enum FilterStringToken {
    #[token("\"")]
    Quote,

    #[regex(r#"[^\"\\\x00-\x1F\x7F]+"#, |lex| lex.slice().to_string())]
    String(String),

    #[regex(r#"\\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})"#, |lex| parse_escaped(&lex.slice()).map_err(FilterLexError::InvalidEscapeSeq))]
    Escaped(char),

    #[token("\\(")]
    Interpolation,

    #[allow(clippy::upper_case_acronyms)]
    #[default]
    EOF,
}

impl std::fmt::Display for FilterToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterToken::True => write!(f, "true"),
            FilterToken::False => write!(f, "false"),
            FilterToken::Null => write!(f, "null"),
            FilterToken::Num(n) => write!(f, "{n}"),
            FilterToken::Module => write!(f, "module"),
            FilterToken::Import => write!(f, "import"),
            FilterToken::Include => write!(f, "include"),
            FilterToken::Def => write!(f, "def"),
            FilterToken::As => write!(f, "as"),
            FilterToken::If => write!(f, "if"),
            FilterToken::Then => write!(f, "then"),
            FilterToken::Else => write!(f, "else"),
            FilterToken::Elif => write!(f, "elif"),
            FilterToken::End => write!(f, "end"),
            FilterToken::Reduce => write!(f, "reduce"),
            FilterToken::Foreach => write!(f, "foreach"),
            FilterToken::Try => write!(f, "try"),
            FilterToken::Catch => write!(f, "catch"),
            FilterToken::Label => write!(f, "label"),
            FilterToken::Break => write!(f, "break"),
            FilterToken::Loc => write!(f, "__loc__"),
            FilterToken::Plus => write!(f, "+"),
            FilterToken::Minus => write!(f, "-"),
            FilterToken::Times => write!(f, "*"),
            FilterToken::Div => write!(f, "/"),
            FilterToken::Mod => write!(f, "%"),
            FilterToken::Pipe => write!(f, "|"),
            FilterToken::Alt => write!(f, "//"),
            FilterToken::Opt => write!(f, "?"),
            FilterToken::OptAlt => write!(f, "?//"),
            FilterToken::Comma => write!(f, ","),
            FilterToken::Assign => write!(f, "="),
            FilterToken::PlusA => write!(f, "+="),
            FilterToken::MinusA => write!(f, "-="),
            FilterToken::TimesA => write!(f, "*="),
            FilterToken::DivA => write!(f, "/="),
            FilterToken::ModA => write!(f, "%="),
            FilterToken::PipeA => write!(f, "|="),
            FilterToken::AltA => write!(f, "//="),
            FilterToken::Eq => write!(f, "=="),
            FilterToken::Neq => write!(f, "!="),
            FilterToken::Lt => write!(f, "<"),
            FilterToken::Le => write!(f, "<="),
            FilterToken::Gt => write!(f, ">"),
            FilterToken::Ge => write!(f, ">="),
            FilterToken::Or => write!(f, "or"),
            FilterToken::And => write!(f, "and"),
            FilterToken::Dot => write!(f, "."),
            FilterToken::Recr => write!(f, ".."),
            FilterToken::LPar => write!(f, "("),
            FilterToken::RPar => write!(f, ")"),
            FilterToken::LBrack => write!(f, "["),
            FilterToken::RBrack => write!(f, "]"),
            FilterToken::LBrace => write!(f, "{{"),
            FilterToken::RBrace => write!(f, "}}"),
            FilterToken::KVDelim => write!(f, ":"),
            FilterToken::ArgDelim => write!(f, ";"),
            FilterToken::Var => write!(f, "$"),
            FilterToken::Quote => write!(f, "\""),
            FilterToken::Id(s) => write!(f, "{s}"),
            FilterToken::Field(s) => write!(f, ".{s}"),
            FilterToken::Format(s) => write!(f, "@{s}"),
            FilterToken::_Comment => write!(f, ""),
            FilterToken::_Newline => write!(f, r#"\n"#),
            FilterToken::_Tab => write!(f, r#"\t"#),
            FilterToken::EOF => write!(f, "<EOF>"),
        }?;
        Ok(())
    }
}
impl std::fmt::Display for FilterStringToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterStringToken::Quote => write!(f, "\""),
            FilterStringToken::String(s) => write!(f, "{s}"),
            FilterStringToken::Escaped(c) => write!(f, "\\{c}"),
            FilterStringToken::Interpolation => write!(f, "("),
            FilterStringToken::EOF => write!(f, "<EOF>"),
        }?;
        Ok(())
    }
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
