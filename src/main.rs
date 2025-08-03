use std::process::ExitCode;

pub mod options;

use jqrs::{
    filter::{self, Filter},
    json::{
        parser::{JsonParser, JsonParserError},
        Json,
    },
    lexer::LexSource,
    parser::ParserPos,
};

use crate::options::{FilterSource, InputSource, JqOptions};

fn process_parser_error(parser: &JsonParser, err: JsonParserError) -> ! {
    let ParserPos { line, column } = parser.pos();
    if let JsonParserError::LexError(err) = err {
        println!("lexing error: {err} at line {line}, column {column}");
    } else {
        println!("parsing error: {err} at line {line}, column {column}");
    }
    std::process::exit(1)
}

fn process_json(_options: &JqOptions, ctx: &filter::run::RunCtx, filter: &Filter, json: Json) {
    let mut results = filter.run(ctx, &json);
    for result in &mut results {
        print!("{}", result.format_pretty_color());
    }
    if let Some(end) = results.end() {
        match end {
            filter::run::RunEndValue::Error(json) => println!("error: {json}"),
            filter::run::RunEndValue::Break(_) => todo!(),
            filter::run::RunEndValue::Halt(_) => todo!(),
        };
    }
}

fn main() -> ExitCode {
    let options = JqOptions::parse(std::env::args_os());
    println!("{options:#?}");

    let filter = match &options.filter_source {
        FilterSource::Identity => Filter::Identity,
        FilterSource::Literal(filter) => filter.parse().unwrap_or_else(|err| {
            println!("error parsing filter: {err}");
            std::process::exit(1)
        }),
        FilterSource::File(path) => std::fs::read_to_string(path)
            .unwrap_or_else(|err| {
                println!("error parsing filter: {err}");
                std::process::exit(1)
            })
            .parse()
            .unwrap_or_else(|err| {
                println!("error parsing filter: {err}");
                std::process::exit(1)
            }),
    };

    let ctx = filter::run::RunCtx::new();

    match &options.input_source {
        InputSource::Null => {
            process_json(&options, &ctx, &filter, Json::Null);
        }
        InputSource::Stdin => {
            let source = LexSource::stdin();
            let mut parser = JsonParser::new(&source);
            while let Some(json) = parser.next() {
                match json {
                    Ok(json) => process_json(&options, &ctx, &filter, json),
                    Err(err) => process_parser_error(&parser, err),
                }
            }
        }
        InputSource::Files(paths) => {
            for path in paths {
                match LexSource::from_path(path) {
                    Ok(source) => {
                        let mut parser = JsonParser::new(&source);
                        while let Some(json) = parser.next() {
                            match json {
                                Ok(json) => process_json(&options, &ctx, &filter, json),
                                Err(err) => process_parser_error(&parser, err),
                            }
                        }
                    }
                    Err(err) => {
                        println!("error reading file {path}: {err}");
                    }
                }
            }
        }
    }

    ExitCode::SUCCESS
}
