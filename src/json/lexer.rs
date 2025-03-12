use logos::Logos;
use rug::float::ParseFloatError;

use crate::{
    lexer::{register_newline, LexSource, LexState, LinePos},
    math::{parse_number, Number},
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct JsonLexState {
    pub line_pos: LinePos,
}
impl LexState for JsonLexState {
    fn line_pos(&self) -> LinePos {
        self.line_pos
    }
    fn line_pos_mut(&mut self) -> &mut LinePos {
        &mut self.line_pos
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum JsonLexError {
    NumberParseError(ParseFloatError),
    InvalidJsonEscapeSeq(String),
    #[default]
    InvalidToken,
}
impl std::fmt::Display for JsonLexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsonLexError::NumberParseError(err) => write!(f, "{err}"),
            JsonLexError::InvalidJsonEscapeSeq(s) => write!(f, "invalid json escaped sequence {s}"),
            JsonLexError::InvalidToken => write!(f, "invalid token"),
        }
    }
}
impl std::error::Error for JsonLexError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            JsonLexError::NumberParseError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<ParseFloatError> for JsonLexError {
    fn from(err: ParseFloatError) -> Self {
        JsonLexError::NumberParseError(err)
    }
}

#[derive(Logos, Debug, Clone, PartialEq, Default)]
#[logos(extras = JsonLexState)]
#[logos(error = JsonLexError)]
#[logos(source = LexSource)]
#[logos(skip r"[\ \t\f\v\r\uFEFF]+")]
pub enum JsonToken {
    // Parsed using JsonStringToken
    Str(String),

    #[regex(r#"[\+\-]?([0-9]+\.?|[0-9]*\.[0-9]+)([eE][\+\-]?[0-9]+)?"#, |lex| parse_number(&lex.slice()))]
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

    #[default]
    EOF,
}

#[derive(Logos, Debug, Clone, PartialEq, Default)]
#[logos(extras = JsonLexState)]
#[logos(error = JsonLexError)]
#[logos(source = LexSource)]
pub enum JsonStringToken {
    #[token("\"")]
    Quote,

    #[regex(r#"[^\"\\\x00-\x1F\x7F]+"#, |lex| lex.slice().to_string())]
    String(String),

    #[regex(r#"\\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})"#, |lex| parse_escaped(&lex.slice()))]
    Escaped(char),

    #[token("\n", register_newline)]
    _Newline,

    #[default]
    EOF,
}

fn parse_escaped(s: &str) -> Result<char, JsonLexError> {
    match s {
        "\\\"" => Ok('\"'),
        "\\\\" => Ok('\\'),
        "\\/" => Ok('/'),
        "\\b" => Ok('\x08'),
        "\\f" => Ok('\x0C'),
        "\\n" => Ok('\n'),
        "\\r" => Ok('\r'),
        "\\t" => Ok('\t'),
        s if s.len() == 6 && s.starts_with("\\u") => {
            let i = u32::from_str_radix(&s[2..6], 16).map_err(|_| JsonLexError::InvalidJsonEscapeSeq(s.to_string()))?;
            char::from_u32(i).ok_or_else(|| JsonLexError::InvalidJsonEscapeSeq(s.to_string()))
        },
        s => Err(JsonLexError::InvalidJsonEscapeSeq(s.to_string())),
    }
}

impl std::fmt::Display for JsonToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsonToken::Str(s) => write!(f, "\"{s}\""),
            JsonToken::Num(n) => write!(f, "{n}"),
            JsonToken::True => write!(f, "true"),
            JsonToken::False => write!(f, "false"),
            JsonToken::Null => write!(f, "null"),
            JsonToken::NaN => write!(f, "NaN"),
            JsonToken::InfP => write!(f, "+Infinity"),
            JsonToken::InfM => write!(f, "-Infinity"),
            JsonToken::LBrack => write!(f, "["),
            JsonToken::RBrack => write!(f, "]"),
            JsonToken::Comma => write!(f, ","),
            JsonToken::LBrace => write!(f, "{{"),
            JsonToken::RBrace => write!(f, "}}"),
            JsonToken::KVDelim => write!(f, ":"),
            JsonToken::Quote => write!(f, "\""),
            JsonToken::_Newline => write!(f, r#"\n"#),
            JsonToken::EOF => write!(f, "<EOF>"),
        }?;
        Ok(())
    }
}
impl std::fmt::Display for JsonStringToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsonStringToken::Quote => write!(f, "\""),
            JsonStringToken::String(s) => write!(f, "{s}"),
            JsonStringToken::Escaped(c) => write!(f, "\\{c}"),
            JsonStringToken::_Newline => write!(f, r#"\n"#),
            JsonStringToken::EOF => write!(f, "<EOF>"),
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
    fn test_lexer() -> Result<(), Box<dyn std::error::Error>> {
        let source = LexSource::String(JSONS.to_string());
        let mut lexer = JsonToken::lexer(&source);

        for token in lexer.by_ref() {
            println!("{token:?}");
        }

        let state = lexer.extras;
        println!("{state:#?}");

        Ok(())
    }
}
