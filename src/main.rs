use std::{
    io::{BufRead, BufReader, Read},
    process::ExitCode,
};

pub mod options;

use auto_enums::enum_derive;
use jqrs::{
    filter::{self, parser::FilterParserError, Filter},
    json::{
        parser::{parse_raw_json_string, JsonParserError},
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

fn write_filter_parser_error(pos: &ParserPos, err: &FilterParserError) {
    let ParserPos { line, column } = pos;
    eprintln!("{CRATE_NAME}: error: syntax error: {err}, line {line}, column {column}");
}

fn write_json_parser_error(pos: &ParserPos, err: &JsonParserError) {
    let ParserPos { line, column } = pos;
    if let JsonParserError::LexError(err) = err {
        eprintln!("{CRATE_NAME}: lexing error: {err} at line {line}, column {column}");
    } else {
        eprintln!("{CRATE_NAME}: parsing error: {err} at line {line}, column {column}");
    }
}

fn process_json(
    ctx: &filter::run::RunCtx,
    filter: &Filter,
    json: Result<Json, (ParserPos, JsonParserError)>,
) {
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
        Err((pos, err)) => write_json_parser_error(&pos, &err),
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
        FilterSource::Literal(filter) => match filter.parse() {
            Ok(filter) => filter,
            Err((pos, err)) => {
                write_filter_parser_error(&pos, &err);
                return ExitCode::FAILURE;
            }
        },
        FilterSource::File(path) => {
            let content = match std::fs::read_to_string(path) {
                Ok(content) => content,
                Err(err) => {
                    write_io_error(&err);
                    return ExitCode::FAILURE;
                }
            };
            match content.parse() {
                Ok(filter) => filter,
                Err((pos, err)) => {
                    write_filter_parser_error(&pos, &err);
                    return ExitCode::FAILURE;
                }
            }
        }
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
            read_sources
                .iter()
                .map(|read_source| read_source.to_lex_source())
                .filter_map(|result| result.inspect_err(write_io_error).ok())
                .flat_map(|source| Json::parser(source))
                .for_each(|json| process_json(&ctx, &filter, json))
        }
        InputMode::Slurp => {
            let json = read_sources
                .iter()
                .map(|read_source| read_source.to_lex_source())
                .filter_map(|result| result.inspect_err(write_io_error).ok())
                .flat_map(|source| Json::parser(source))
                .collect::<Result<Vec<_>, _>>()
                .map(Json::Array);

            process_json(&ctx, &filter, json)
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
                            parse_raw_json_string(LexSource::String(line)).map(Json::String)
                        })
                })
                .collect::<Result<Vec<_>, _>>()
                .map(Json::Array);

            process_json(&ctx, &filter, json)
        }
        InputMode::RawSlurp => {
            let mut raw_string = String::new();
            for read_source in read_sources {
                if let Err(err) = read_source.read_to_string(&mut raw_string) {
                    write_io_error(&err);
                }
            }

            let json = parse_raw_json_string(LexSource::String(raw_string)).map(Json::String);

            process_json(&ctx, &filter, json)
        }
    }

    ExitCode::SUCCESS
}
