use std::process::ExitCode;

use jqrs::{
    json::parser::{JsonParser, JsonParserError},
    lexer::LexSource,
    parser::ParserPos,
};

pub enum SourceType {
    File,
    Stdin,
    String,
}

fn main() -> ExitCode {
    let source_type = SourceType::Stdin;
    let input_path = "tests/big.jsonl";

    let source = match source_type {
        SourceType::File => match LexSource::from_path(input_path) {
            Ok(source) => source,
            Err(err) => {
                println!("error reading file {input_path}: {err}");
                return ExitCode::FAILURE;
            }
        },
        SourceType::Stdin => LexSource::stdin(),
        SourceType::String => match std::fs::read_to_string(input_path) {
            Ok(source) => LexSource::String(source),
            Err(err) => {
                println!("error reading file {input_path}: {err}");
                return ExitCode::FAILURE;
            }
        },
    };

    let mut parser = JsonParser::new(&source);
    while let Some(json) = parser.next() {
        match json {
            Ok(json) => {
                let json_fmt = json.format_pretty_color();
                print!("{json_fmt}");
            }
            Err(err) => {
                let ParserPos { line, column } = parser.pos();
                if let JsonParserError::LexError(err) = err {
                    println!("lexing error: {err} at line {line}, column {column}");
                } else {
                    println!("parsing error: {err} at line {line}, column {column}");
                }
            }
        }
    }

    ExitCode::SUCCESS
}
