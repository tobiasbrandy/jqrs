use crate::json::Json;

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
    VerDef(String, FilterRef, FilterRef),

    // Literals
    ArrayLit(FilterRef),
    ObjectLit(Vec<(Filter, Filter)>),

    // Projections
    Project(FilterRef, FilterRef),
    Slice(FilterRef, Option<FilterRef>, Option<FilterRef>),
    Iter,

    // Flow Opterators
    Pipe(FilterRef, FilterRef),                 // |
    Alt(FilterRef, FilterRef),                  // //
    TryCatch(FilterRef, FilterRef),             // try .. catch ..
    Comma(FilterRef, FilterRef),                // ,
    IfElse(FilterRef, FilterRef, FilterRef),  // if cond then path1 else path2

    // Reductions
    Reduce(FilterRef, String, FilterRef, FilterRef),                // reduce stream_expression as $name (initial_value; update_expression)
    Foreach(FilterRef, String, FilterRef, FilterRef, FilterRef),    // foreach stream_expression as $name (initial_value; update_expression; extract_expression)

    // Functions
    FuncDef(String, Vec<FuncParam>, FilterRef, FilterRef),
    FuncCall(String, Vec<Filter>),

    // Label & Break
    Label(String, FilterRef),
    Break(String),

    // Special
    Loc(String, usize),
}
