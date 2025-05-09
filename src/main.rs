use std::process::ExitCode;

use jqrs::{
    filter::{self, Filter},
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

    let filter = std::env::args()
        .nth(1)
        .unwrap_or(".".to_string())
        .parse::<Filter>();
    let filter = match filter {
        Ok(filter) => filter,
        Err(err) => {
            println!("error parsing filter: {err}");
            return ExitCode::FAILURE;
        }
    };

    let ctx = filter::run::RunCtx::new();

    let mut parser = JsonParser::new(&source);
    while let Some(json) = parser.next() {
        match json {
            Ok(json) => {
                let mut results = filter.run(&ctx, &json);
                for result in &mut results {
                    print!("{}", result.format_pretty_color());
                }
                if let Some(end) = results.end() {
                    match end {
                        filter::run::RunEndValue::Error(json) => println!("error: {json}"),
                        filter::run::RunEndValue::Break(_) => todo!(),
                        filter::run::RunEndValue::Halt(_) => todo!(),
                    };
                    return ExitCode::FAILURE;
                }
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
