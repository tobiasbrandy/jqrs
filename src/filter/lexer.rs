use std::str::FromStr;

use logos::Logos;
use rug::float::ParseFloatError;

use crate::{
    lexer::{parse, parse_escaped, register_newline, register_tab, LexSource, LinePos},
    math::Number,
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum FilterLexError {
    NumberParseError(ParseFloatError),
    InvalidEscapeSeq(String),
    InvalidOperator(String),
    #[default]
    InvalidToken,
}
impl std::fmt::Display for FilterLexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NumberParseError(err) => write!(f, "{err}"),
            Self::InvalidEscapeSeq(s) => write!(f, "invalid escape sequence {s}"),
            Self::InvalidOperator(s) => write!(f, "invalid operator {s}"),
            Self::InvalidToken => write!(f, "invalid token"),
        }
    }
}
impl std::error::Error for FilterLexError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::NumberParseError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<ParseFloatError> for FilterLexError {
    fn from(err: ParseFloatError) -> Self {
        Self::NumberParseError(err)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    // Arithmetic Operators
    Plus,
    Minus,
    Times,
    Div,
    Mod,

    // Flow Operators
    Pipe,
    Alt,
    Opt,
    OptAlt,
    Comma,

    // Assignment Operators
    Assign,
    PlusA,
    MinusA,
    TimesA,
    DivA,
    ModA,
    PipeA,
    AltA,

    // Comparison Operators
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    Or,
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

    #[regex(r#"([0-9]+\.?|[0-9]*\.[0-9]+)([eE][\+\-]?[0-9]+)?"#, |lex| lex.slice().parse())]
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
    Op(Op),

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
    Colon,

    // Params
    #[token(";")]
    Semicolon,

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

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Times => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::Pipe => write!(f, "|"),
            Self::Alt => write!(f, "//"),
            Self::Opt => write!(f, "?"),
            Self::OptAlt => write!(f, "?//"),
            Self::Comma => write!(f, ","),
            Self::Assign => write!(f, "="),
            Self::PlusA => write!(f, "+="),
            Self::MinusA => write!(f, "-="),
            Self::TimesA => write!(f, "*="),
            Self::DivA => write!(f, "/="),
            Self::ModA => write!(f, "%="),
            Self::PipeA => write!(f, "|="),
            Self::AltA => write!(f, "//="),
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::Gt => write!(f, ">"),
            Self::Ge => write!(f, ">="),
            Self::Or => write!(f, "or"),
            Self::And => write!(f, "and"),
        }
    }
}
impl std::fmt::Display for FilterToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Null => write!(f, "null"),
            Self::Num(n) => write!(f, "{n}"),
            Self::Module => write!(f, "module"),
            Self::Import => write!(f, "import"),
            Self::Include => write!(f, "include"),
            Self::Def => write!(f, "def"),
            Self::As => write!(f, "as"),
            Self::If => write!(f, "if"),
            Self::Then => write!(f, "then"),
            Self::Else => write!(f, "else"),
            Self::Elif => write!(f, "elif"),
            Self::End => write!(f, "end"),
            Self::Reduce => write!(f, "reduce"),
            Self::Foreach => write!(f, "foreach"),
            Self::Try => write!(f, "try"),
            Self::Catch => write!(f, "catch"),
            Self::Label => write!(f, "label"),
            Self::Break => write!(f, "break"),
            Self::Loc => write!(f, "__loc__"),
            Self::Op(op) => write!(f, "{op}"),
            Self::Dot => write!(f, "."),
            Self::Recr => write!(f, ".."),
            Self::LPar => write!(f, "("),
            Self::RPar => write!(f, ")"),
            Self::LBrack => write!(f, "["),
            Self::RBrack => write!(f, "]"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::Colon => write!(f, ":"),
            Self::Semicolon => write!(f, ";"),
            Self::Var => write!(f, "$"),
            Self::Quote => write!(f, "\""),
            Self::Id(s) => write!(f, "{s}"),
            Self::Field(s) => write!(f, ".{s}"),
            Self::Format(s) => write!(f, "@{s}"),
            Self::_Comment => write!(f, ""),
            Self::_Newline => write!(f, r#"\n"#),
            Self::_Tab => write!(f, r#"\t"#),
            Self::EOF => write!(f, "<EOF>"),
        }
    }
}
impl std::fmt::Display for FilterStringToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Quote => write!(f, "\""),
            Self::String(s) => write!(f, "{s}"),
            Self::Escaped(c) => write!(f, "\\{c}"),
            Self::Interpolation => write!(f, "("),
            Self::EOF => write!(f, "<EOF>"),
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
