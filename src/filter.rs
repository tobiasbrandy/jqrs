use std::{collections::HashMap, str::FromStr, sync::Arc};

use crate::{json::Json, lexer::LexSource, math::Number};

mod lexer;
pub mod parser;
pub mod run;

type FilterRef = Box<Filter>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuncParam {
    VarParam(Arc<str>),
    FilterParam(Arc<str>),
}
impl std::fmt::Display for FuncParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VarParam(name) => write!(f, "${name}"),
            Self::FilterParam(name) => write!(f, "{name}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Filter {
    // Basic
    Identity,   // .
    Empty,      // <empty>
    Json(Json), // <json>

    // Variable
    Var(Arc<str>),                          // $<name>
    VarDef(Arc<str>, FilterRef, FilterRef), // <name> as <body> | <next>

    // Literals
    ArrayLit(FilterRef),              // [<array>]
    ObjectLit(Vec<(Filter, Filter)>), // {<key>: <value>, ...}

    // Projections
    Project(FilterRef, FilterRef), // <term>.<field>
    Slice(FilterRef, Option<FilterRef>, Option<FilterRef>), // <term>[<left>:<right>]
    Iter,

    // Flow Opterators
    Pipe(FilterRef, FilterRef),              // <left> | <right>
    Alt(FilterRef, FilterRef),               // <left // <right>
    TryCatch(FilterRef, FilterRef),          // try <try> catch <catch>
    Comma(FilterRef, FilterRef),             // <left>, <right>
    IfElse(FilterRef, FilterRef, FilterRef), // if <cond> then <then> else <else>

    // Reductions
    Reduce(FilterRef, Arc<str>, FilterRef, FilterRef), // reduce <exp> as $<name> (<init>; <update>)
    Foreach(FilterRef, Arc<str>, FilterRef, FilterRef, FilterRef), // foreach <exp> as $<name> (<init>; <update>; <extract>)

    // Functions
    FuncDef(Arc<str>, Vec<FuncParam>, FilterRef, FilterRef), // def <name>(<params>): <body>; <next>
    FuncCall(Arc<str>, Vec<Filter>),                         // <name>(<args>)

    // Label & Break
    Label(Arc<str>, FilterRef), // label $<name> | <next>
    Break(Arc<str>),            // break $<name>

    // Special
    Loc(Arc<str>, usize), // $__loc__
}
impl Filter {
    pub fn run<'a>(&'a self, ctx: &'a run::RunCtx, json: &'a Json) -> run::RunGen<impl std::future::Future<Output = run::RunEnd> + 'a> {
        run::run(ctx, self, json)
    }

    pub fn string(s: String) -> Self {
        Self::Json(Json::String(s))
    }

    pub fn number(n: Number) -> Self {
        Self::Json(Json::Number(n))
    }

    pub fn bool(b: bool) -> Self {
        Self::Json(Json::Bool(b))
    }

    pub fn null() -> Self {
        Self::Json(Json::Null)
    }

    pub fn array(arr: Vec<Json>) -> Self {
        Self::Json(Json::Array(arr))
    }

    pub fn object(obj: HashMap<String, Json>) -> Self {
        Self::Json(Json::Object(obj))
    }
}
impl std::fmt::Display for Filter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identity => write!(f, "."),
            Self::Empty => write!(f, ""),
            Self::Json(json) => write!(f, "{json}"),
            Self::Var(name) => write!(f, "${name}"),
            Self::VarDef(name, body, next) => write!(f, "{body} as ${name} | {next}"),
            Self::ArrayLit(arr) => write!(f, "[{arr}]"),
            Self::ObjectLit(obj) => write!(
                f,
                "{{{}}}",
                obj.iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Project(term, proj) => write!(f, "{term}.{proj}"),
            Self::Slice(term, left, right) => {
                write!(f, "{term}[")?;
                if let Some(left) = left {
                    write!(f, "{left}")?;
                }
                write!(f, ":")?;
                if let Some(right) = right {
                    write!(f, "{right}")?;
                }
                write!(f, "]")
            }
            Self::Iter => write!(f, "[]"),
            Self::Pipe(left, right) => write!(f, "{left} | {right}"),
            Self::Alt(left, right) => write!(f, "{left} // {right}"),
            Self::TryCatch(try_, catch_) => {
                write!(f, "try {try_}")?;
                if !matches!(**catch_, Self::Empty) {
                    write!(f, " catch {catch_}")?;
                }
                Ok(())
            }
            Self::Comma(left, right) => write!(f, "{left}, {right}"),
            Self::IfElse(cond, then, else_) => write!(f, "if {cond} then {then} else {else_}"),
            Self::Reduce(stream, pattern, init, update) => {
                write!(f, "reduce {stream} as {pattern} ({init}; {update})")
            }
            Self::Foreach(stream, pattern, init, update, extract) => write!(
                f,
                "foreach {stream} as {pattern} ({init}; {update}; {extract})"
            ),
            Self::FuncDef(name, params, body, next) => {
                write!(
                    f,
                    "def {name}{}: {body}; {next}",
                    if params.is_empty() {
                        "".to_string()
                    } else {
                        params
                            .iter()
                            .map(|param| param.to_string())
                            .collect::<Vec<_>>()
                            .join("; ")
                    }
                )
            }
            Self::FuncCall(name, args) => {
                write!(f, "{name}")?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    f.write_str(
                        &args
                            .iter()
                            .map(|arg| arg.to_string())
                            .collect::<Vec<_>>()
                            .join("; "),
                    )?;
                    write!(f, ")")?;
                }
                Ok(())
            }
            Self::Label(name, next) => write!(f, "label ${name} | {next}"),
            Self::Break(name) => write!(f, "break ${name}"),
            Self::Loc(_, _) => write!(f, "$__loc__"),
        }
    }
}
impl FromStr for Filter {
    type Err = parser::FilterParserError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parser::parse_filter(LexSource::str(s))
    }
}
