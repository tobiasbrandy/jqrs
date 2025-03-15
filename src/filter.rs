use std::collections::HashMap;

use crate::{json::Json, math::Number};

mod lexer;
pub mod parser;

type FilterRef = Box<Filter>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuncParam {
    VarParam(String),
    FilterParam(String),
}
impl std::fmt::Display for FuncParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FuncParam::VarParam(name) => write!(f, "${name}"),
            FuncParam::FilterParam(name) => write!(f, "{name}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Filter {
    // Basic
    Identity,       // .
    Empty,          // <empty>
    Json(Json),     // <json>

    // Variable
    Var(String),    // $<name>
    VarDef(String, FilterRef, FilterRef), // <name> as <body> | <next>

    // Literals
    ArrayLit(FilterRef),                // [<array>]
    ObjectLit(Vec<(Filter, Filter)>),   // {<key>: <value>, ...}

    // Projections
    Project(FilterRef, FilterRef),                          // <term>.<field>
    Slice(FilterRef, Option<FilterRef>, Option<FilterRef>), // <term>[<left>:<right>]
    Iter,

    // Flow Opterators
    Pipe(FilterRef, FilterRef),              // <left> | <right>
    Alt(FilterRef, FilterRef),               // <left // <right>
    TryCatch(FilterRef, FilterRef),          // try <try> catch <catch>
    Comma(FilterRef, FilterRef),             // <left>, <right>
    IfElse(FilterRef, FilterRef, FilterRef), // if <cond> then <then> else <else>

    // Reductions
    Reduce(FilterRef, String, FilterRef, FilterRef),                // reduce <exp> as $<name> (<init>; <update>)
    Foreach(FilterRef, String, FilterRef, FilterRef, FilterRef),    // foreach <exp> as $<name> (<init>; <update>; <extract>)

    // Functions
    FuncDef(String, Vec<FuncParam>, FilterRef, FilterRef),  // def <name>(<params>): <body>; <next>
    FuncCall(String, Vec<Filter>),                          // <name>(<args>)

    // Label & Break
    Label(String, FilterRef), // label $<name> | <next>
    Break(String),            // break $<name>

    // Special
    Loc(String, usize),     // $__loc__
}
impl Filter {
    pub fn json_string(s: String) -> Self {
        Filter::Json(Json::String(s))
    }

    pub fn json_number(n: Number) -> Self {
        Filter::Json(Json::Number(n))
    }

    pub fn json_bool(b: bool) -> Self {
        Filter::Json(Json::Bool(b))
    }

    pub fn json_null() -> Self {
        Filter::Json(Json::Null)
    }

    pub fn json_array(arr: Vec<Json>) -> Self {
        Filter::Json(Json::Array(arr))
    }

    pub fn json_object(obj: HashMap<String, Json>) -> Self {
        Filter::Json(Json::Object(obj))
    }
}
impl std::fmt::Display for Filter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Filter::Identity => write!(f, "."),
            Filter::Empty => write!(f, ""),
            Filter::Json(json) => write!(f, "{json}"),
            Filter::Var(name) => write!(f, "${name}"),
            Filter::VarDef(name, body, next) => write!(f, "{body} as ${name} | {next}"),
            Filter::ArrayLit(arr) => write!(f, "[{arr}]"),
            Filter::ObjectLit(obj) => write!(
                f,
                "{{{}}}",
                obj.iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Filter::Project(term, proj) => write!(f, "{term}.{proj}"),
            Filter::Slice(term, left, right) => {
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
            Filter::Iter => write!(f, "[]"),
            Filter::Pipe(left, right) => write!(f, "{left} | {right}"),
            Filter::Alt(left, right) => write!(f, "{left} // {right}"),
            Filter::TryCatch(try_, catch_) => {
                write!(f, "try {try_}")?;
                if !matches!(**catch_, Filter::Empty) {
                    write!(f, " catch {catch_}")?;
                }
                Ok(())
            }
            Filter::Comma(left, right) => write!(f, "{left}, {right}"),
            Filter::IfElse(cond, then, else_) => write!(f, "if {cond} then {then} else {else_}"),
            Filter::Reduce(stream, pattern, init, update) => {
                write!(f, "reduce {stream} as {pattern} ({init}; {update})")
            }
            Filter::Foreach(stream, pattern, init, update, extract) => write!(
                f,
                "foreach {stream} as {pattern} ({init}; {update}; {extract})"
            ),
            Filter::FuncDef(name, params, body, next) => {
                write!(
                    f,
                    "def {name}{}: {body}; {next}",
                    if params.is_empty() {
                        "".to_string()
                    } else {
                        params.iter().map(|param| param.to_string()).collect::<Vec<_>>().join("; ")
                    }
                )
            }
            Filter::FuncCall(name, args) => {
                write!(f, "{name}")?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    f.write_str(&args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join("; "))?;
                    write!(f, ")")?;
                }
                Ok(())
            }
            Filter::Label(name, next) => write!(f, "label ${name} | {next}"),
            Filter::Break(name) => write!(f, "break ${name}"),
            Filter::Loc(_, _) => write!(f, "$__loc__"),
        }
    }
}
