use logos::Logos;
use rug::float::ParseFloatError;

use crate::{
    lexer::{parse, parse_escaped, register_newline, register_tab, LexSource, LinePos},
    math::Number,
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum JsonLexError {
    NumberParseError(ParseFloatError),
    InvalidEscapeSeq(String),
    #[default]
    InvalidToken,
}
impl std::fmt::Display for JsonLexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NumberParseError(err) => write!(f, "{err}"),
            Self::InvalidEscapeSeq(s) => write!(f, "invalid escape sequence {s}"),
            Self::InvalidToken => write!(f, "invalid token"),
        }
    }
}
impl std::error::Error for JsonLexError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::NumberParseError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<ParseFloatError> for JsonLexError {
    fn from(err: ParseFloatError) -> Self {
        Self::NumberParseError(err)
    }
}

#[derive(Logos, Debug, Clone, PartialEq, Default)]
#[logos(extras = LinePos)]
#[logos(error = JsonLexError)]
#[logos(source = LexSource<'s>)]
#[logos(skip r"[\ \f\v\r\uFEFF]+")]
pub enum JsonToken {
    // Parsed using JsonStringToken
    Str(String),

    #[regex(r#"[\+\-]?([0-9]+\.?|[0-9]*\.[0-9]+)([eE][\+\-]?[0-9]+)?"#, parse)]
    Num(Number),

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("null")]
    Null,

    // Special nums
    #[regex(r"[\+\-]?[Nn]a[Nn]")]
    NaN,

    #[regex(r"\-[Ii]nfinity")]
    InfP,

    #[regex(r"\+?[Ii]nfinity")]
    InfM,

    // Array
    #[token("[")]
    LBrack,

    #[token("]")]
    RBrack,

    #[token(",")]
    Comma,

    // Object
    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token(":")]
    KVDelim,

    // String
    #[token("\"")]
    Quote,

    // Control
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
#[logos(error = JsonLexError)]
#[logos(source = LexSource<'s>)]
pub enum JsonStringToken {
    #[token("\"")]
    Quote,

    #[regex(r#"[^\"\\\x00-\x1F\x7F]+"#, |lex| lex.slice().to_string())]
    String(String),

    #[regex(r#"\\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})"#, |lex| parse_escaped(&lex.slice()).map_err(JsonLexError::InvalidEscapeSeq))]
    Escaped(char),

    #[allow(clippy::upper_case_acronyms)]
    #[default]
    EOF,
}

impl std::fmt::Display for JsonToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(s) => write!(f, "\"{s}\""),
            Self::Num(n) => write!(f, "{n}"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Null => write!(f, "null"),
            Self::NaN => write!(f, "NaN"),
            Self::InfP => write!(f, "+Infinity"),
            Self::InfM => write!(f, "-Infinity"),
            Self::LBrack => write!(f, "["),
            Self::RBrack => write!(f, "]"),
            Self::Comma => write!(f, ","),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::KVDelim => write!(f, ":"),
            Self::Quote => write!(f, "\""),
            Self::_Newline => write!(f, r#"\n"#),
            Self::_Tab => write!(f, r#"\t"#),
            Self::EOF => write!(f, "<EOF>"),
        }?;
        Ok(())
    }
}
impl std::fmt::Display for JsonStringToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Quote => write!(f, "\""),
            Self::String(s) => write!(f, "{s}"),
            Self::Escaped(c) => write!(f, "\\{c}"),
            Self::EOF => write!(f, "<EOF>"),
        }?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const JSONS: &str = r#"
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
    fn test_lexer() {
        let source = LexSource::String(JSONS.to_string());
        let mut lexer = JsonToken::lexer(&source);

        for token in lexer.by_ref() {
            println!("{token:?}");
        }

        let state = lexer.extras;
        println!("{state:#?}");
    }
}
