pub mod options;

use std::{
    collections::HashMap,
    io::{BufRead, BufReader, IsTerminal, Read, Write},
    process::ExitCode,
    sync::Arc,
};

use auto_enums::enum_derive;
use jqrs::{
    filter::{self, Filter, parser::FilterParserError},
    json::{
        self, Json,
        parser::{JsonParserError, parse_raw_json_string},
    },
    lexer::LexSource,
};
use thiserror::Error;

use crate::options::{
    ColorMode, FilterSource, IndentMode, InputArg, InputMode, InputSource, JqOptions, OutputMode,
};

#[derive(Debug, Error)]
enum AppError {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("syntax error: {0}")]
    Filter(#[from] FilterParserError),
    #[error("parsing error: {0}")]
    Json(#[from] JsonParserError),
}
fn write_error<Err>(err: Err)
where
    AppError: From<Err>,
{
    const CRATE_NAME: &str = env!("CARGO_PKG_NAME");
    let err = AppError::from(err);
    eprintln!("{CRATE_NAME}: error: {err}");
}

fn process_json(
    options: &JqOptions,
    fmt: &json::fmt::Format,
    ctx: &filter::run::RunCtx,
    filter: &Filter,
    json: Result<Json, JsonParserError>,
) -> ExitCode {
    match json {
        Ok(json) => {
            let mut exit_status = if options.exit_status { 4 } else { 0 };

            let mut results = filter.run(ctx, &json);
            for result in &mut results {
                print!("{}", result.format(fmt));

                if options.unbuffered {
                    let _ = std::io::stdout().flush();
                }

                if options.exit_status {
                    exit_status = if result.to_bool() { 0 } else { 1 };
                }
            }
            if let Some(end) = results.end() {
                match end {
                    filter::run::RunEndValue::Error(json) => println!("error: {json}"),
                    filter::run::RunEndValue::Break(_) => todo!(),
                    filter::run::RunEndValue::Halt(_) => todo!(),
                };
                return ExitCode::from(5);
            }

            ExitCode::from(exit_status)
        }
        Err(err) => {
            write_error(err);
            ExitCode::from(5)
        }
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

    fn to_buf_read(&self) -> std::io::Result<impl BufRead + use<>> {
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

fn parse_filter(filter_source: &FilterSource) -> Result<Filter, AppError> {
    Ok(match filter_source {
        FilterSource::Identity => Filter::Identity,
        FilterSource::Literal(filter) => filter.parse()?,
        FilterSource::File(path) => std::fs::read_to_string(path)?.parse()?,
    })
}

fn build_json_fmt(options: &JqOptions) -> json::fmt::Format {
    let indent = match options.indent_mode {
        IndentMode::Default => json::fmt::Indent::Spaces(2),
        IndentMode::Compact => json::fmt::Indent::Compact,
        IndentMode::Spaces(n) => json::fmt::Indent::Spaces(n),
        IndentMode::Tab => json::fmt::Indent::Tab,
    };

    let ascii = options.ascii_output;
    let sort = options.sort_keys;

    let (raw_str, trailing_char) = match options.output_mode {
        OutputMode::Default => (false, Some('\n')),
        OutputMode::Raw => (true, Some('\n')),
        OutputMode::Raw0 => (true, Some('\0')), // TODO: If string contains NUL char, fail
        OutputMode::Join => (true, None),
    };

    let colors = {
        let use_color = match options.color_mode {
            ColorMode::Auto => std::io::stdout().is_terminal(),
            ColorMode::Always => true,
            ColorMode::Never => false,
        };

        if use_color {
            Some(match std::env::var("JQ_COLOR") {
                Ok(color_str) => color_str
                    .parse()
                    .inspect_err(|_| eprintln!("Failed to set $JQ_COLORS"))
                    .unwrap_or_default(),
                Err(std::env::VarError::NotPresent) => json::fmt::Colors::default(),
                Err(std::env::VarError::NotUnicode(_)) => {
                    eprintln!("Failed to set $JQ_COLORS");
                    json::fmt::Colors::default()
                }
            })
        } else {
            None
        }
    };

    json::fmt::Format {
        indent,
        sort,
        colors,
        raw_str,
        ascii,
        trailing_char,
    }
}

fn build_run_vars(options: &JqOptions) -> Result<HashMap<Arc<str>, Json>, AppError> {
    fn parse_input_arg(arg: &InputArg) -> Result<Json, AppError> {
        Ok(match arg {
            InputArg::String(arg) => Json::String(parse_raw_json_string(LexSource::Str(arg))?),
            InputArg::Json(arg) => arg.parse()?,
            InputArg::SlurpFile(path) => Json::Array(
                Json::parser(LexSource::from_path(path)?).collect::<Result<Vec<_>, _>>()?,
            ),
            InputArg::RawFile(path) => {
                Json::String(parse_raw_json_string(LexSource::from_path(path)?)?)
            }
        })
    }

    let positional_args: Vec<Json> = options
        .positional_args
        .iter()
        .map(parse_input_arg)
        .collect::<Result<_, _>>()?;
    let named_args: HashMap<String, Json> = options
        .named_args
        .iter()
        .map(|(key, arg)| Ok((key.into(), parse_input_arg(arg)?)))
        .collect::<Result<_, AppError>>()?;

    let mut vars = named_args
        .iter()
        .map(|(key, val)| (key.as_str().into(), val.clone()))
        .collect::<HashMap<_, _>>();
    vars.insert(
        "ARGS".into(),
        Json::Object(HashMap::from([
            ("positional".into(), Json::Array(positional_args)),
            ("named".into(), Json::Object(named_args)),
        ])),
    );

    Ok(vars)
}

fn main() -> ExitCode {
    // TODO: Stream mode
    // TODO: seq
    // TODO: binary
    // TODO: build configuration
    // TODO: run tests
    let options = JqOptions::from_cli(std::env::args_os());

    let filter = match parse_filter(&options.filter_source) {
        Ok(filter) => filter,
        Err(err) => {
            write_error(err);
            return ExitCode::from(3);
        }
    };

    let json_fmt = build_json_fmt(&options);

    let run_vars = match build_run_vars(&options) {
        Ok(vars) => vars,
        Err(err) => {
            write_error(err);
            return ExitCode::from(2);
        }
    };
    let ctx = filter::run::RunCtx::new(run_vars);

    let read_sources = match options.input_source.clone() {
        InputSource::Null => {
            return process_json(&options, &json_fmt, &ctx, &filter, Ok(Json::Null));
        }
        InputSource::Stdin => vec![ReadSource::Stdin],
        InputSource::Files(paths) => paths.into_iter().map(ReadSource::File).collect(),
    };

    match &options.input_mode {
        InputMode::Default => read_sources
            .iter()
            .map(|read_source| read_source.to_lex_source())
            .filter_map(|result| result.map_err(write_error).ok())
            .flat_map(|source| Json::parser(source))
            .map(|json| process_json(&options, &json_fmt, &ctx, &filter, json))
            .last()
            .unwrap_or(if options.exit_status {
                ExitCode::from(4)
            } else {
                ExitCode::from(0)
            }),
        InputMode::Slurp => {
            let json = read_sources
                .iter()
                .map(|read_source| read_source.to_lex_source())
                .filter_map(|result| result.map_err(write_error).ok())
                .flat_map(|source| Json::parser(source))
                .collect::<Result<Vec<_>, _>>()
                .map(Json::Array);

            process_json(&options, &json_fmt, &ctx, &filter, json)
        }
        InputMode::Raw => {
            let json = read_sources
                .into_iter()
                .map(|read_source| read_source.to_buf_read())
                .filter_map(|result| result.map_err(write_error).ok())
                .flat_map(|source| {
                    source
                        .lines()
                        .filter_map(|result| result.map_err(write_error).ok())
                        .map(|line| {
                            parse_raw_json_string(LexSource::String(line)).map(Json::String)
                        })
                })
                .collect::<Result<Vec<_>, _>>()
                .map(Json::Array);

            process_json(&options, &json_fmt, &ctx, &filter, json)
        }
        InputMode::RawSlurp => {
            let mut raw_string = String::new();
            for read_source in read_sources {
                if let Err(err) = read_source.read_to_string(&mut raw_string) {
                    write_error(err);
                }
            }

            let json = parse_raw_json_string(LexSource::String(raw_string)).map(Json::String);

            process_json(&options, &json_fmt, &ctx, &filter, json)
        }
    }
}
