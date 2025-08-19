use std::{str::FromStr, sync::{Arc, LazyLock}};

use crate::{json::Json, lexer::LexSource, math::Number};

mod lexer;
pub mod parser;
pub mod run;
pub mod run_vm;

type FilterRef = Arc<Filter>;

#[derive(Debug, Clone, PartialEq)]
pub enum Filter {
    // Basic
    Identity,   // .
    Empty,      // <empty>
    Json(Arc<Json>), // <json>

    // Variable
    Var(Arc<str>),                          // $<name>
    VarDef(Arc<str>, FilterRef, FilterRef), // <name> as <body> | <next>

    // Literals
    ArrayLit(FilterRef),                    // [<array>]
    ObjectLit(Vec<(FilterRef, FilterRef)>), // {<key>: <value>, ...}

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
    FuncDef(Arc<str>, Arc<[Arc<str>]>, FilterRef, FilterRef), // def <name>(<params>): <body>; <next>
    FuncCall(Arc<str>, Vec<FilterRef>),                         // <name>(<args>)

    // Label & Break
    Label(Arc<str>, FilterRef), // label $<name> | <next>
    Break(Arc<str>),            // break $<name>

    // Special
    Loc(Arc<str>, usize), // $__loc__
}
impl Filter {
    pub fn run<'a>(self: &'a Arc<Self>, ctx: &'a run::RunCtx, json: &'a Arc<Json>) -> run::RunGen<impl std::future::Future<Output = run::RunEnd> + 'a> {
        run::run(ctx, self, json)
    }

    pub fn string(s: Arc<str>) -> Self {
        Self::Json(Json::String(s).into())
    }

    pub fn number(n: Number) -> Self {
        Self::Json(Json::Number(n).into())
    }

    pub fn arc_null() -> Arc<Self> {
        static ARC_NULL: LazyLock<Arc<Filter>> = LazyLock::new(|| Arc::new(Filter::Json(Json::arc_null())));
        ARC_NULL.clone()
    }

    pub fn arc_true() -> Arc<Self> {
        static ARC_TRUE: LazyLock<Arc<Filter>> = LazyLock::new(|| Arc::new(Filter::Json(Json::arc_true())));
        ARC_TRUE.clone()
    }

    pub fn arc_false() -> Arc<Self> {
        static ARC_FALSE: LazyLock<Arc<Filter>> = LazyLock::new(|| Arc::new(Filter::Json(Json::arc_false())));
        ARC_FALSE.clone()
    }

    pub fn arc_bool(b: bool) -> Arc<Self> {
        if b {
            Self::arc_true()
        } else {
            Self::arc_false()
        }
    }

    pub fn array(arr: im::Vector<Arc<Json>>) -> Self {
        Self::Json(Json::Array(arr).into())
    }

    pub fn object(obj: im::HashMap<Arc<str>, Arc<Json>>) -> Self {
        Self::Json(Json::Object(obj).into())
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
