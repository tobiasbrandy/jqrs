use std::ffi::OsString;

use clap::{
    command, crate_authors, crate_description, crate_name, crate_version, value_parser, ArgAction,
    CommandFactory, FromArgMatches, Parser,
};

#[derive(Parser, Debug, Clone, PartialEq, Eq)]
#[command(name = crate_name!())]
#[command(version = crate_version!())]
#[command(author = crate_authors!())]
#[command(about = concat!(crate_name!(), " - ", crate_description!(), " [version ", crate_version!(), "]"))]
#[command(override_usage = concat!(
    " ", crate_name!(), " [options] <jq filter> [file...])\n",
    "        ", crate_name!(), " [options] --args <jq filter> [strings...]\n",
    "        ", crate_name!(), " [options] --jsonargs <jq filter> [JSON_TEXTS...]",
))]
struct Cli {
    /// Use `null` as the single input value
    #[arg(short = 'n', long)]
    null_input: bool,

    /// Read each line as string instead of JSON
    #[arg(short = 'R', long)]
    raw_input: bool,

    /// Read all inputs into an array and use it as the single input value
    #[arg(short = 's', long)]
    slurp: bool,

    /// Compact instead of pretty-printed output
    #[arg(short = 'c', long, overrides_with_all = ["tab", "indent"])]
    compact_output: bool,

    /// Output strings without escapes and quotes
    #[arg(short = 'r', long)]
    raw_output: bool,

    /// Implies -r and output NUL after each output
    #[arg(long)]
    raw_output0: bool,

    /// Implies -r and output without newline after each output
    #[arg(short = 'j', long)]
    join_output: bool,

    /// Output strings by only ASCII characters using escape sequences
    #[arg(short = 'a', long)]
    ascii_output: bool,

    /// Sort keys of each object on output
    #[arg(short = 'S', long)]
    sort_keys: bool,

    /// Colorize JSON output
    #[arg(short = 'C', long, conflicts_with = "monochrome_output")]
    color_output: bool,

    /// Disable colored output
    #[arg(short = 'M', long, conflicts_with = "color_output")]
    monochrome_output: bool,

    /// Use tabs for indentation
    #[arg(long, overrides_with_all = ["indent", "compact_output"])]
    tab: bool,

    /// Use n spaces for indentation (max 7 spaces)
    #[arg(long, value_name = "n", overrides_with_all = ["tab", "compact_output"], value_parser = value_parser!(u8).range(0..=7))]
    indent: Option<u8>,

    /// Flush output stream after each output
    #[arg(long)]
    unbuffered: bool,

    /// Parse the input value in streaming fashion
    #[arg(long)]
    stream: bool,

    /// Implies --stream and report parse error as an array
    #[arg(long)]
    stream_errors: bool,

    /// Parse input/output as application/json-seq
    #[arg(long)]
    seq: bool,

    /// Load the filter from a file
    #[arg(short = 'f', long, value_name = "file")]
    from_file: Option<String>,

    /// Search modules from the directory
    #[arg(short = 'L', long, value_name = "dir")]
    library_path: Vec<String>,

    /// Set $name to the string value
    #[arg(long, value_names = ["name", "value"])]
    arg: Vec<String>,

    /// Set $name to the JSON value
    #[arg(long, value_names = ["name", "value"])]
    argjson: Vec<String>,

    /// Set $name to an array of JSON values read from the file
    #[arg(long, value_names = ["name", "file"])]
    slurpfile: Vec<String>,

    /// Set $name to string contents of file
    #[arg(long, value_names = ["name", "file"])]
    rawfile: Vec<String>,

    // Flags don't track the position of each occurrence, so we need to emulate flags with value-less options to get the same result
    // Strategy for tracking flags position taken from https://docs.rs/clap/latest/clap/_cookbook/find/index.html
    /// Consume remaining arguments as positional string values
    #[arg(long, num_args = 0, value_parser = value_parser!(bool), default_missing_value = "true", default_value = "false", action = ArgAction::Append)]
    args: Vec<bool>,

    /// Consume remaining arguments as positional JSON values
    #[arg(long, num_args = 0, value_parser = value_parser!(bool), default_missing_value = "true", default_value = "false", action = ArgAction::Append)]
    jsonargs: Vec<bool>,

    /// Set exit status code based on the output
    #[arg(short = 'e', long)]
    exit_status: bool,

    /// Force CRLF to LF conversion (useful for Windows users using WSL, MSYS2, or Cygwin)
    #[arg(short = 'b', long, hide = true)]
    binary: bool,

    /// Show jq's build configuration
    #[arg(long)]
    build_configuration: bool,

    /// Run tests from file or stdin
    #[arg(long, value_name = "filename", hide = true)]
    run_tests: Option<Option<String>>, // TODO: test this. Validate it's the last option.

    #[arg(hide = true)]
    pos_args: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FilterSource {
    Identity,        // Default filter
    Literal(String), // Filter comes from literal string
    File(String),    // Filter comes from file
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputSource {
    Null,               // Use `null` as the single input value
    Stdin,              // Input from stdin
    Files(Vec<String>), // Input from files
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputMode {
    Default,  // Default input mode
    Raw,      // Read each line as string instead of JSON
    Slurp,    // Read all inputs into an array and use it as the single input value
    RawSlurp, // Read all inputs into an array and use it as the single input value
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputMode {
    Default, // Normal JSON output
    Raw,     // Raw output with newlines
    Raw0,    // Raw output with NUL separators
    Join,    // Raw output without newlines
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorMode {
    Auto,   // Based on terminal detection
    Always, // Force colors
    Never,  // No colors
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndentMode {
    Default,    // Use default indentation
    Compact,    // Compact output
    Tab,        // Use tabs for indentation
    Spaces(u8), // Use n spaces for indentation (max 7 spaces)
}

#[derive(Debug, Clone, Copy)]
pub enum StreamMode {
    Off,      // No streaming
    On,       // Streaming
    OnErrors, // Streaming with error reporting
}

#[derive(Debug, Clone)]
pub enum InputArg {
    String(String),    // Literal string value
    Json(String),      // Literal JSON value
    SlurpFile(String), // Array of JSON values read from file
    RawFile(String),   // String contents of file
}

#[derive(Debug, Clone)]
pub enum TestMode {
    Off,          // No testing
    Stdin,        // Test from stdin
    File(String), // Test from file
}

#[derive(Debug, Clone)]
pub struct JqOptions {
    pub filter_source: FilterSource,

    pub input_source: InputSource,

    pub input_mode: InputMode,

    pub output_mode: OutputMode,

    // Output strings by only ASCII characters using escape sequences
    pub ascii_output: bool,

    // Sort keys of each object on output
    pub sort_keys: bool,

    pub color_mode: ColorMode,

    pub indent_mode: IndentMode,

    // Flush output stream after each output
    pub unbuffered: bool,

    pub stream_mode: StreamMode,

    // ???
    pub seq: bool,

    // Search modules from the directory
    pub library_path: Vec<String>,

    pub named_args: Vec<(String, InputArg)>,

    pub positional_args: Vec<InputArg>,

    // Set exit status code based on the output
    pub exit_status: bool,

    // Force CRLF to LF conversion (useful for Windows users using WSL, MSYS2, or Cygwin)
    pub binary: bool,

    // Show jq's build configuration
    pub build_configuration: bool,

    pub test_mode: TestMode,
}

impl JqOptions {
    pub fn parse<I, T>(cli_args: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<OsString> + Clone,
    {
        let mut matches = Cli::command().get_matches_from(cli_args);

        // Deduce the type of each positional argument from the position of the --args and --jsonargs flags
        #[derive(Debug, Clone, Copy)]
        enum PosArgType {
            File,
            String,
            Json,
        }
        let pos_arg_types = {
            let mut all_args = std::iter::empty()
                .chain(
                    matches
                        .indices_of("pos_args")
                        .into_iter()
                        .flatten()
                        .map(|i| (i, PosArgType::File)),
                )
                .chain(
                    matches
                        .indices_of("args")
                        .into_iter()
                        .flatten()
                        .map(|i| (i, PosArgType::String)),
                )
                .chain(
                    matches
                        .indices_of("jsonargs")
                        .into_iter()
                        .flatten()
                        .map(|i| (i, PosArgType::Json)),
                )
                .collect::<Vec<_>>();

            all_args.sort_by_key(|(i, _)| *i);

            let mut pos_arg_types = Vec::with_capacity(all_args.len());

            let mut current_type = PosArgType::File;
            for (_, arg_type) in all_args {
                match arg_type {
                    PosArgType::File => {
                        pos_arg_types.push(current_type);
                    }
                    PosArgType::String => {
                        current_type = PosArgType::String;
                    }
                    PosArgType::Json => {
                        current_type = PosArgType::Json;
                    }
                }
            }

            pos_arg_types
        };

        let res =
            Cli::from_arg_matches_mut(&mut matches).map_err(|err| err.format(&mut Cli::command()));
        let cli = match res {
            Ok(s) => s,
            Err(e) => e.exit(),
        };

        let pos_args_count = cli.pos_args.len();
        let mut pos_args = std::iter::zip(cli.pos_args, pos_arg_types);

        // Filter may come from multiple sources
        let filter_source = {
            if let Some(filter_file) = cli.from_file {
                // Filter comes from file
                FilterSource::File(filter_file)
            } else if let Some((filter, _)) = pos_args.next() {
                // Filter comes from first positional arg
                FilterSource::Literal(filter)
            } else {
                // No filter provided, use default filter
                FilterSource::Identity
            }
        };

        // Parse positional args
        let mut files = Vec::with_capacity(pos_args_count);
        let mut positional_args = Vec::with_capacity(pos_args_count);
        for (arg, arg_type) in pos_args {
            match arg_type {
                PosArgType::File => files.push(arg),
                PosArgType::String => positional_args.push(InputArg::String(arg)),
                PosArgType::Json => positional_args.push(InputArg::Json(arg)),
            }
        }

        let input_source = {
            if cli.null_input {
                InputSource::Null
            } else if files.is_empty() {
                InputSource::Stdin
            } else {
                InputSource::Files(files)
            }
        };

        let input_mode = {
            if cli.raw_input && cli.slurp {
                InputMode::RawSlurp
            } else if cli.raw_input {
                InputMode::Raw
            } else if cli.slurp {
                InputMode::Slurp
            } else {
                InputMode::Default
            }
        };

        let output_mode = {
            if cli.raw_output0 {
                OutputMode::Raw0
            } else if cli.raw_output {
                OutputMode::Raw
            } else if cli.join_output {
                OutputMode::Join
            } else {
                OutputMode::Default
            }
        };

        let color_mode = {
            if cli.color_output {
                ColorMode::Always
            } else if cli.monochrome_output || std::env::var("NO_COLOR").is_ok() {
                ColorMode::Never
            } else {
                ColorMode::Auto
            }
        };

        let indent_mode = {
            if cli.compact_output {
                IndentMode::Compact
            } else if cli.tab {
                IndentMode::Tab
            } else if let Some(indent) = cli.indent {
                IndentMode::Spaces(indent)
            } else {
                IndentMode::Default
            }
        };

        let stream_mode = {
            if cli.stream_errors {
                StreamMode::OnErrors
            } else if cli.stream {
                StreamMode::On
            } else {
                StreamMode::Off
            }
        };

        let named_args = {
            let mut named_args = vec![];

            let mut arg_iter = cli.arg.into_iter();
            while let (Some(name), Some(value)) = (arg_iter.next(), arg_iter.next()) {
                named_args.push((name, InputArg::String(value)));
            }

            let mut jsonarg_iter = cli.argjson.into_iter();
            while let (Some(name), Some(value)) = (jsonarg_iter.next(), jsonarg_iter.next()) {
                named_args.push((name, InputArg::Json(value)));
            }

            let mut slurpfile_iter = cli.slurpfile.into_iter();
            while let (Some(name), Some(file)) = (slurpfile_iter.next(), slurpfile_iter.next()) {
                named_args.push((name, InputArg::SlurpFile(file)));
            }

            let mut rawfile_iter = cli.rawfile.into_iter();
            while let (Some(name), Some(file)) = (rawfile_iter.next(), rawfile_iter.next()) {
                named_args.push((name, InputArg::RawFile(file)));
            }

            named_args
        };

        let test_mode = {
            match cli.run_tests {
                Some(Some(file)) => TestMode::File(file),
                Some(None) => TestMode::Stdin,
                None => TestMode::Off,
            }
        };

        JqOptions {
            filter_source,
            input_source,
            input_mode,
            output_mode,
            ascii_output: cli.ascii_output,
            sort_keys: cli.sort_keys,
            color_mode,
            indent_mode,
            unbuffered: cli.unbuffered,
            stream_mode,
            seq: cli.seq,
            library_path: cli.library_path,
            named_args,
            positional_args,
            exit_status: cli.exit_status,
            binary: cli.binary,
            build_configuration: cli.build_configuration,
            test_mode,
        }
    }
}

// TODO: Add tests for the options
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        let options = JqOptions::parse(vec![
            "jqrs",
            "1",
            "2",
            "--args",
            "3",
            "4",
            "--jsonargs",
            "6",
            "8",
            "--args",
            "8",
            "--jsonargs",
            "9",
            "--",
            "10",
            "--",
            "11",
        ]);
        println!("{options:#?}");
    }
}
