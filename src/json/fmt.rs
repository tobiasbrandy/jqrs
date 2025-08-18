use std::{str::FromStr, sync::Arc};

use either::Either;

use crate::math::Number;

use super::Json;

// Terminal Colors
const COLOR_RESET: &str = "\x1B[0m";

#[derive(Debug, Clone)]
pub struct Colors {
    pub null: String,
    pub false_: String,
    pub true_: String,
    pub number: String,
    pub string: String,
    pub array: String,
    pub object: String,
    pub obj_key: String,
}
impl Default for Colors {
    fn default() -> Self {
        Self {
            null: "\x1B[1;30m".to_string(),
            false_: "\x1B[0;39m".to_string(),
            true_: "\x1B[0;39m".to_string(),
            number: "\x1B[0;39m".to_string(),
            string: "\x1B[0;32m".to_string(),
            array: "\x1B[1;39m".to_string(),
            object: "\x1B[1;39m".to_string(),
            obj_key: "\x1B[1;34m".to_string(),
        }
    }
}
impl FromStr for Colors {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn escape(s: &str) -> String {
            format!("\x1B[{s}m")
        }
    
        fn try_parse_color_string(s: &str) -> Option<Colors> {
            let mut colors = s.split(':');
    
            Some(Colors {
                null: escape(colors.next()?),
                false_: escape(colors.next()?),
                true_: escape(colors.next()?),
                number: escape(colors.next()?),
                string: escape(colors.next()?),
                array: escape(colors.next()?),
                object: escape(colors.next()?),
                obj_key: escape(colors.next()?),
            })
        }
    
        try_parse_color_string(s).ok_or(())
    }
}

// Indentation per level of nesting
#[derive(Debug, Clone)]
pub enum Indent {
    Spaces(u8),
    Tab,
    Compact, // Removes all whitespace from the output
}

#[derive(Debug, Clone)]
pub struct Format {
    // Indentation per level of nesting
    pub indent: Indent,
    // Sort object keys by lexicographic order
    pub sort: bool,
    // Wether to add color for terminal output
    pub colors: Option<Colors>,
    // If the output is a string, don't quote it
    pub raw_str: bool,
    // Whether to escape non-ASCII characters in strings
    pub ascii: bool,
    // Whether to add a trailing newline to the output
    pub trailing_char: Option<char>,
}
impl Format {
    pub const COMPACT: Self = Self {
        indent: Indent::Spaces(0),
        sort: false,
        colors: None,
        raw_str: false,
        ascii: false,
        trailing_char: None,
    };

    pub const PRETTY: Self = Self {
        indent: Indent::Spaces(2),
        sort: true,
        colors: None,
        raw_str: false,
        ascii: false,
        trailing_char: Some('\n'),
    };

    pub fn pretty_color() -> Self {
        Self {
            indent: Indent::Spaces(2),
            sort: true,
            colors: Some(Colors::default()),
            raw_str: false,
            ascii: false,
            trailing_char: Some('\n'),
        }
    }
}
impl Default for Format {
    fn default() -> Self {
        Self::COMPACT
    }
}

#[derive(Debug, Clone)]
struct State {
    level: usize,
}

#[derive(Debug, Clone)]
struct Config<'a> {
    indent: String,
    newline: &'static str,
    item_sep: &'static str,
    key_val_sep: &'static str,
    sort: bool,
    ascii: bool,
    colors: Option<&'a Colors>,
}

fn colorize(s: &str, color: Option<&str>, out: &mut String) {
    if let Some(color) = color {
        out.push_str(color);
        out.push_str(s);
        out.push_str(COLOR_RESET);
    } else {
        out.push_str(s);
    }
}

pub fn format_json(json: &Json, format: &Format) -> String {
    let Format {
        indent,
        sort,
        colors,
        raw_str,
        ascii,
        trailing_char,
    } = format;

    if *raw_str && let Json::String(s) = json {
        let mut out = String::new();
        if *ascii {
            out.push('"');
            quote(s, &mut out, *ascii);
            out.push('"');
        } else {
            out.push_str(s);
        }
        if let Some(trailing_char) = trailing_char {
            out.push(*trailing_char);
        }
        return out;
    }

    let mut state = State { level: 0 };
    let config = Config {
        indent: match indent {
            Indent::Spaces(n) => " ".repeat(*n as usize),
            Indent::Tab => "\t".to_string(),
            Indent::Compact => "".to_string(),
        },
        newline: match indent {
            Indent::Compact => "",
            _ => "\n",
        },
        item_sep: ",",
        key_val_sep: match indent {
            Indent::Compact => ":",
            _ => ": ",
        },
        sort: *sort,
        ascii: *ascii,
        colors: colors.as_ref(),
    };

    let mut out = String::new();
    format_json_value(json, &config, &mut state, &mut out);
    if let Some(trailing_char) = trailing_char {
        out.push(*trailing_char);
    }
    out
}

fn format_json_value(json: &Json, config: &Config, state: &mut State, out: &mut String) {
    match json {
        Json::Object(map) => format_object(map, config, state, out),
        Json::Array(arr) => format_array(arr, config, state, out),
        Json::String(s) => format_string(s, config, out),
        Json::Number(n) => format_number(n, config, out),
        Json::Bool(b) => format_bool(*b, config, out),
        Json::Null => format_null(config, out),
    }
}

fn format_null(Config { colors, .. }: &Config, out: &mut String) {
    let enc = "null";

    match colors {
        Some(Colors { null, .. }) => {
            out.push_str(null.as_str());
            out.push_str(enc);
            out.push_str(COLOR_RESET);
        }
        None => out.push_str(enc),
    };
}

fn format_bool(b: bool, Config { colors, .. }: &Config, out: &mut String) {
    let enc = if b { "true" } else { "false" };

    match colors {
        Some(Colors { true_, false_, .. }) => {
            out.push_str(if b { true_.as_str() } else { false_.as_str() });
            out.push_str(enc);
            out.push_str(COLOR_RESET);
        }
        None => out.push_str(enc),
    }
}

fn encode_whitespace(
    Config {
        indent, newline, ..
    }: &Config,
    State { level, .. }: &State,
    out: &mut String,
) {
    out.push_str(newline);
    out.push_str(indent.repeat(*level).as_str());
}

fn format_array(
    arr: &im::Vector<Arc<Json>>,
    config @ Config {
        item_sep, colors, ..
    }: &Config,
    state: &mut State,
    out: &mut String,
) {
    let color = colors.as_ref().map(|Colors { array, .. }| array.as_str());

    colorize("[", color, out);

    state.level += 1;

    let mut iter = arr.iter();
    if let Some(first) = iter.next() {
        encode_whitespace(config, state, out);
        format_json_value(first, config, state, out);

        for elem in iter {
            colorize(item_sep, color, out);
            encode_whitespace(config, state, out);
            format_json_value(elem, config, state, out);
        }

        state.level -= 1;
        encode_whitespace(config, state, out);
        state.level += 1;
    }

    state.level -= 1;

    colorize("]", color, out);
}

fn format_object(
    map: &im::HashMap<Arc<str>, Arc<Json>>,
    config @ Config {
        sort,
        item_sep,
        colors,
        ..
    }: &Config,
    state: &mut State,
    out: &mut String,
) {
    fn encode(
        key: &str,
        val: &Json,
        key_color: Option<&str>,
        config: &Config,
        state: &mut State,
        out: &mut String,
    ) {
        encode_whitespace(config, state, out);

        if let Some(key_color) = key_color {
            out.push_str(key_color);
        }
        out.push('"');
        quote(key, out, config.ascii);
        out.push('"');
        if key_color.is_some() {
            out.push_str(COLOR_RESET);
        }

        out.push_str(config.key_val_sep); // Not colored

        format_json_value(val, config, state, out);
    }

    let key_color = colors
        .as_ref()
        .map(|Colors { obj_key, .. }| obj_key.as_str());
    let obj_color = colors.as_ref().map(|Colors { object, .. }| object.as_str());

    colorize("{", obj_color, out);

    state.level += 1;

    let mut iter = {
        if *sort {
            let mut items = map.iter().collect::<Vec<_>>();
            items.sort_by_key(|(key, _)| *key);
            Either::Left(items.into_iter())
        } else {
            Either::Right(map.iter())
        }
    };

    if let Some((first_key, first_val)) = iter.next() {
        encode(first_key, first_val, key_color, config, state, out);

        for (key, val) in iter {
            colorize(item_sep, obj_color, out);
            encode(key, val, key_color, config, state, out);
        }

        state.level -= 1;
        encode_whitespace(config, state, out);
        state.level += 1;
    }

    state.level -= 1;

    colorize("}", obj_color, out);
}

fn format_number(n: &Number, Config { colors, .. }: &Config, out: &mut String) {
    fn encoding(n: &Number, out: &mut String) {
        match n {
            Number::Int(i) => out.push_str(i.to_string().as_str()),
            Number::Decimal(f) if f.is_nan() => out.push_str("null"),
            Number::Decimal(f) if f.is_infinite() && f.is_sign_positive() => {
                out.push_str("Infinity")
            }
            Number::Decimal(f) if f.is_infinite() && f.is_sign_negative() => {
                out.push_str("-Infinity")
            }
            Number::Decimal(f) => out.push_str(f.to_string().as_str()),
        }
    }

    match colors {
        Some(Colors { number, .. }) => {
            out.push_str(number.as_str());
            encoding(n, out);
            out.push_str(COLOR_RESET);
        }
        None => encoding(n, out),
    }
}

fn format_string(s: &str, Config { colors, ascii, .. }: &Config, out: &mut String) {
    fn encode(s: &str, out: &mut String, ascii: bool) {
        out.push('"');
        quote(s, out, ascii);
        out.push('"');
    }

    match colors {
        Some(Colors { string, .. }) => {
            out.push_str(string.as_str());
            encode(s, out, *ascii);
            out.push_str(COLOR_RESET);
        }
        None => encode(s, out, *ascii),
    }
}

/// Escapes a string in a JSON-like manner.
/// For each character in `s`, if it is a double quote, backslash,
/// or a control character (< 0x20), then it is replaced with its
/// corresponding escape sequence. Otherwise, the character is emitted as-is.
/// If `ascii` is true, all non-ASCII characters are escaped.
fn quote(s: &str, out: &mut String, ascii: bool) {
    /// Determines whether a character needs escaping.
    fn needs_escape(c: char, ascii: bool) -> bool {
        c == '"' || c == '\\' || c < '\x20' || (ascii && !c.is_ascii())
    }

    /// Escapes a single character.
    /// For standard escapes it returns strings like "\\\"", "\\n", etc.
    /// For control characters (less than 0x20) not handled by the standard ones,
    /// it produces a Unicode escape like "\\u001F".
    fn escape(c: char, out: &mut String, ascii: bool) {
        match c {
            '"' => out.push_str(r#"\""#),
            '\\' => out.push_str(r"\\"),
            '\x08' => out.push_str(r"\b"), // backspace
            '\x0C' => out.push_str(r"\f"), // form feed
            '\n' => out.push_str(r"\n"),
            '\r' => out.push_str(r"\r"),
            '\t' => out.push_str(r"\t"),
            c if c < '\x20' || (ascii && !c.is_ascii()) => {
                // Produce a Unicode escape sequence with 4-digit padding.
                let hex = format!("{:x}", c as u32);
                let pad = "0".repeat(4 - hex.len());
                out.push_str(format!("\\u{pad}{hex}").as_str())
            }
            _ => out.push(c), // Should not happen: we checks needs_escape previously
        }
    }

    // We'll iterate over the string by char indices so that we can
    // copy over large chunks that do not require escaping.
    let mut last = 0;
    for (i, ch) in s.char_indices() {
        if needs_escape(ch, ascii) {
            // Push the part of the string that does not need escaping.
            out.push_str(&s[last..i]);
            // Append the escape sequence for the character.
            escape(ch, out, ascii);
            // Update the index after the escaped character.
            last = i + ch.len_utf8();
        }
    }
    // Push any remaining characters.
    out.push_str(&s[last..]);
}

// TODO: tests
