use std::{
    io::{BufRead, BufReader, Read},
    process::ExitCode,
};

pub mod options;

use auto_enums::enum_derive;
use jqrs::{
    filter::{self, Filter},
    json::{
        parser::{JsonParser, JsonParserError},
        Json,
    },
    lexer::LexSource,
    parser::ParserPos,
};

use crate::options::{FilterSource, InputMode, InputSource, JqOptions};

const CRATE_NAME: &str = env!("CARGO_PKG_NAME");

fn write_io_error(err: &std::io::Error) {
    eprintln!("{CRATE_NAME}: error: {err}");
}

fn write_parser_error(err: &JsonParserError, pos: &ParserPos) {
    let ParserPos { line, column } = pos;
    if let JsonParserError::LexError(err) = err {
        eprintln!("{CRATE_NAME}: lexing error: {err} at line {line}, column {column}");
    } else {
        eprintln!("{CRATE_NAME}: parsing error: {err} at line {line}, column {column}");
    }
}

fn process_json(ctx: &filter::run::RunCtx, filter: &Filter, json: Result<Json, JsonParserError>) {
    match json {
        Ok(json) => {
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
        Err(err) => write_parser_error(&err, &ParserPos::default()),
    }
}

#[derive(Debug, Clone)]
enum ReadSource {
    Stdin,
    File(String),
}
impl ReadSource {
    fn to_lex_source<'a>(&self) -> std::io::Result<LexSource<'a>> {
        match self {
            ReadSource::Stdin => Ok(LexSource::stdin()),
            ReadSource::File(path) => LexSource::from_path(path),
        }
    }

    fn to_buf_read(&self) -> std::io::Result<impl BufRead> {
        #[enum_derive(BufRead)]
        enum OpenReadSource {
            Stdin(std::io::StdinLock<'static>),
            File(BufReader<std::fs::File>),
        }

        match self {
            ReadSource::Stdin => Ok(OpenReadSource::Stdin(std::io::stdin().lock())),
            ReadSource::File(path) => Ok(OpenReadSource::File(BufReader::new(
                std::fs::File::open(path)?,
            ))),
        }
    }

    fn read_to_string(&self, buf: &mut String) -> std::io::Result<usize> {
        match self {
            ReadSource::Stdin => std::io::stdin().read_to_string(buf),
            ReadSource::File(path) => std::fs::File::open(path)?.read_to_string(buf),
        }
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

    let read_sources = match options.input_source {
        InputSource::Null => {
            process_json(&ctx, &filter, Ok(Json::Null));
            return ExitCode::SUCCESS;
        }
        InputSource::Stdin => vec![ReadSource::Stdin],
        InputSource::Files(paths) => paths.into_iter().map(ReadSource::File).collect(),
    };

    match &options.input_mode {
        InputMode::Default => {
            for read_source in read_sources {
                match read_source.to_lex_source() {
                    Ok(source) => {
                        let parser = JsonParser::new(&source);
                        for json in parser {
                            process_json(&ctx, &filter, json);
                        }
                    }
                    Err(err) => write_io_error(&err),
                }
            }
        }
        InputMode::Slurp => {
            fn parse_items(read_sources: &[ReadSource]) -> Result<Json, JsonParserError> {
                let mut items = Vec::new();

                for read_source in read_sources {
                    match read_source.to_lex_source() {
                        Ok(source) => {
                            for json in JsonParser::new(&source) {
                                match json {
                                    Ok(json) => items.push(json),
                                    Err(err) => return Err(err),
                                }
                            }
                        }
                        Err(err) => write_io_error(&err),
                    }
                }

                Ok(Json::Array(items))
            }

            process_json(&ctx, &filter, parse_items(&read_sources));
        }
        InputMode::Raw => {
            let json = read_sources
                .into_iter()
                .map(|read_source| read_source.to_buf_read())
                .filter_map(|result| result.inspect_err(write_io_error).ok())
                .flat_map(|source| {
                    source
                        .lines()
                        .filter_map(|result| result.inspect_err(write_io_error).ok())
                        .map(|line| {
                            JsonParser::new(&LexSource::String(line))
                                .parse_raw_string()
                                .map(Json::String)
                        })
                })
                .collect::<Result<Vec<_>, _>>()
                .map(Json::Array);

            process_json(&ctx, &filter, json);
        }
        InputMode::RawSlurp => {
            let mut raw_string = String::new();
            for read_source in read_sources {
                if let Err(err) = read_source.read_to_string(&mut raw_string) {
                    write_io_error(&err);
                }
            }

            let json = JsonParser::new(&LexSource::String(raw_string))
                .parse_raw_string()
                .map(Json::String);

            process_json(&ctx, &filter, json);
        }
    }

    ExitCode::SUCCESS
}
