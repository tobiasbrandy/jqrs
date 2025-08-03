use derive_more::Display;
use logos::Logos;
use rug::float::ParseFloatError;
use thiserror::Error;

use crate::{
    lexer::{parse, parse_escaped, register_newline, register_tab, LexSource, LinePos},
    math::Number,
};

#[derive(Debug, Clone, PartialEq, Eq, Default, Error)]
pub enum JsonLexError {
    #[error(transparent)]
    NumberParseError(#[from] ParseFloatError),
    #[error("invalid escape sequence {0}")]
    InvalidEscapeSeq(String),
    #[default]
    #[error("invalid token")]
    InvalidToken,
}

#[derive(Logos, Debug, Clone, PartialEq, Default, Display)]
#[logos(extras = LinePos)]
#[logos(error = JsonLexError)]
#[logos(source = LexSource<'s>)]
#[logos(skip r"[\ \f\v\r\uFEFF]+")]
pub enum JsonToken {
    // Parsed using JsonStringToken
    #[display("\"{_0}\"")]
    Str(String),

    #[regex(r#"[\+\-]?([0-9]+\.?|[0-9]*\.[0-9]+)([eE][\+\-]?[0-9]+)?"#, parse)]
    #[display("{_0}")]
    Num(Number),

    #[token("true")]
    #[display("true")]
    True,

    #[token("false")]
    #[display("false")]
    False,

    #[token("null")]
    #[display("null")]
    Null,

    // Special nums
    #[regex(r"[\+\-]?[Nn]a[Nn]")]
    #[display("NaN")]
    NaN,

    #[regex(r"\-[Ii]nfinity")]
    #[display("+Infinity")]
    InfP,

    #[regex(r"\+?[Ii]nfinity")]
    #[display("-Infinity")]
    InfM,

    // Array
    #[token("[")]
    #[display("[")]
    LBrack,

    #[token("]")]
    #[display("]")]
    RBrack,

    #[token(",")]
    #[display(",")]
    Comma,

    // Object
    #[token("{")]
    #[display("{{")]
    LBrace,

    #[token("}")]
    #[display("}}")]
    RBrace,

    #[token(":")]
    #[display(":")]
    KVDelim,

    // String
    #[token("\"")]
    #[display("\"")]
    Quote,

    // Control
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
#[logos(error = JsonLexError)]
#[logos(source = LexSource<'s>)]
pub enum JsonStringToken {
    #[token("\"")]
    #[display("\"")]
    Quote,

    #[regex(r#"[^\"\\\x00-\x1F\x7F]+"#, |lex| lex.slice().to_string())]
    #[display("{_0}")]
    String(String),

    #[regex(r#"\\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})"#, |lex| parse_escaped(&lex.slice()).map_err(JsonLexError::InvalidEscapeSeq))]
    #[display("\\{_0}")]
    Escaped(char),

    #[default]
    #[display("<EOF>")]
    #[allow(clippy::upper_case_acronyms)]
    EOF,
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
