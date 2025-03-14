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

#[derive(Debug, Clone, PartialEq)]
pub enum Filter {
    // Basic
    Identity,
    Empty,
    Json(Json),

    // Variable
    Var(String),
    VarDef(String, FilterRef, FilterRef),

    // Literals
    ArrayLit(FilterRef),
    ObjectLit(Vec<(Filter, Filter)>),

    // Projections
    Project(FilterRef, FilterRef),
    Slice(FilterRef, Option<FilterRef>, Option<FilterRef>),
    Iter,

    // Flow Opterators
    Pipe(FilterRef, FilterRef),              // |
    Alt(FilterRef, FilterRef),               // //
    TryCatch(FilterRef, FilterRef),          // try .. catch ..
    Comma(FilterRef, FilterRef),             // ,
    IfElse(FilterRef, FilterRef, FilterRef), // if cond then path1 else path2

    // Reductions
    Reduce(FilterRef, String, FilterRef, FilterRef), // reduce stream_expression as $name (initial_value; update_expression)
    Foreach(FilterRef, String, FilterRef, FilterRef, FilterRef), // foreach stream_expression as $name (initial_value; update_expression; extract_expression)

    // Functions
    FuncDef(String, Vec<FuncParam>, FilterRef, FilterRef), // name(params): body; next
    FuncCall(String, Vec<Filter>),

    // Label & Break
    Label(String, FilterRef),
    Break(String),

    // Special
    Loc(String, usize),
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
