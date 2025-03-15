#![allow(non_snake_case)]

use crate::{
    filter::lexer::Op,
    lexer::LexSource,
    parser::{ExpectationFailed, Parser, ParserPos},
};

use super::{
    lexer::{FilterLexError, FilterStringToken, FilterToken},
    Filter, FuncParam,
};

pub struct FilterParser<'a>(Parser<'a, FilterToken, FilterParserError>);
impl<'a> FilterParser<'a> {
    pub fn new(source: &'a LexSource) -> Self {
        Self(Parser::new(source))
    }

    pub fn parse(&mut self) -> Result<Filter, FilterParserError> {
        fn inner(
            parser: &mut Parser<'_, FilterToken, FilterParserError>,
        ) -> Result<Filter, FilterParserError> {
            let filter = Filter(parser)?;

            let next_tok = parser.pop_token()?;

            if !matches!(next_tok, FilterToken::EOF) {
                return Err(FilterParserError::UnexpectedToken(next_tok));
            }

            Ok(filter)
        }

        let ret = inner(&mut self.0);
        self.0.close();
        ret
    }

    pub fn pos(&self) -> &ParserPos {
        self.0.pos()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FilterParserError {
    LexError(FilterLexError),
    StringParserError(FilterStringParserError),
    ExpectedNumber(FilterToken),
    UnsupportedOperator(Op),
    NonAssocOp(Op),
    UnmatchedExpectation(FilterToken, FilterToken), // expected, actual
    UnexpectedToken(FilterToken),
}
impl std::fmt::Display for FilterParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterParserError::LexError(err) => write!(f, "{err}"),
            FilterParserError::StringParserError(err) => write!(f, "error parsing string: {err}"),
            FilterParserError::ExpectedNumber(tok) => write!(f, "expected number got {tok}"),
            FilterParserError::UnsupportedOperator(op) => write!(f, "unsupported operator {op}"),
            FilterParserError::NonAssocOp(op) => write!(f, "non-associative operator {op}"),
            FilterParserError::UnmatchedExpectation(expected, actual) => {
                write!(f, "expected {expected} got {actual}")
            }
            FilterParserError::UnexpectedToken(tok) => write!(f, "unexpected token {tok}"),
        }
    }
}
impl std::error::Error for FilterParserError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            FilterParserError::LexError(err) => Some(err),
            FilterParserError::StringParserError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<FilterLexError> for FilterParserError {
    fn from(err: FilterLexError) -> Self {
        FilterParserError::LexError(err)
    }
}
impl From<ExpectationFailed<'_, FilterToken>> for FilterParserError {
    fn from(
        ExpectationFailed {
            expected, actual, ..
        }: ExpectationFailed<'_, FilterToken>,
    ) -> Self {
        FilterParserError::UnmatchedExpectation(expected, actual)
    }
}
impl From<FilterStringParserError> for FilterParserError {
    fn from(err: FilterStringParserError) -> Self {
        FilterParserError::StringParserError(err)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FilterStringParserError {
    LexError(FilterLexError),
    UnmatchedExpectation(FilterStringToken, FilterStringToken), // expected, actual
    UnexpectedToken(FilterStringToken),
}
impl std::fmt::Display for FilterStringParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterStringParserError::LexError(err) => write!(f, "{err}"),
            FilterStringParserError::UnmatchedExpectation(expected, actual) => {
                write!(f, "expected {expected} got {actual}")
            }
            FilterStringParserError::UnexpectedToken(tok) => write!(f, "Unexpected token {tok}"),
        }
    }
}
impl std::error::Error for FilterStringParserError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            FilterStringParserError::LexError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<FilterLexError> for FilterStringParserError {
    fn from(err: FilterLexError) -> Self {
        FilterStringParserError::LexError(err)
    }
}
impl From<ExpectationFailed<'_, FilterStringToken>> for FilterStringParserError {
    fn from(
        ExpectationFailed {
            expected, actual, ..
        }: ExpectationFailed<'_, FilterStringToken>,
    ) -> Self {
        FilterStringParserError::UnmatchedExpectation(expected, actual)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum OpAssoc {
    Left,
    Right,
    None,
}

fn op_precedence(op: &Op) -> (u8, OpAssoc) {
    match op {
        Op::Pipe => (1, OpAssoc::Right),
        Op::Comma => (2, OpAssoc::Left),
        Op::Alt => (3, OpAssoc::Right),
        Op::Assign
        | Op::PipeA
        | Op::PlusA
        | Op::MinusA
        | Op::TimesA
        | Op::DivA
        | Op::ModA
        | Op::AltA => (4, OpAssoc::None),
        Op::Or => (5, OpAssoc::Left),
        Op::And => (6, OpAssoc::Left),
        Op::Eq | Op::Neq | Op::Lt | Op::Gt | Op::Le | Op::Ge => (7, OpAssoc::None),
        Op::Plus | Op::Minus => (8, OpAssoc::Left),
        Op::Times | Op::Div | Op::Mod => (9, OpAssoc::Left),

        Op::Opt => (11, OpAssoc::Left),
        Op::OptAlt => (12, OpAssoc::None), // Not Supported Yet
    }
}
fn op_prefix_precedence(op: &Op) -> u8 {
    match op {
        Op::Minus => 10,
        _ => unreachable!(),
    }
}

// Convinience type aliases
type FParser<'a> = Parser<'a, FilterToken, FilterParserError>;
type FResult<T> = Result<T, FilterParserError>;

/// Filter
///   : FuncDefs
///   | Exp
///   | {- empty -}
fn Filter(parser: &mut FParser) -> FResult<Filter> {
    match parser.peek_token()? {
        FilterToken::Def => FuncDefs(parser),
        FilterToken::EOF => Ok(Filter::Empty),
        _ => Exp(parser),
    }
}

/// FuncDefs
///   : FuncDef
///   | FuncDef FuncDefs
fn FuncDefs(parser: &mut FParser) -> FResult<Filter> {
    let mut defs = Vec::new();

    // At least 1 function definition
    let (name, params, body) = FuncDef(parser)?;
    defs.push((name, params, Box::new(body)));

    while let FilterToken::Def = parser.peek_token()? {
        let (name, params, body) = FuncDef(parser)?;
        defs.push((name, params, Box::new(body)));
    }

    let (name, params, body) = defs.pop().unwrap();
    let last = Filter::FuncDef(name, params, body, Box::new(Filter::Empty));

    Ok(defs.into_iter().rfold(last, |acc, (name, params, body)| {
        Filter::FuncDef(name, params, body, Box::new(acc))
    }))
}

/// FuncDef
///   : def id ':' Exp ';'
///   | def id '(' Params ')' ':' Exp ';'
fn FuncDef(parser: &mut FParser) -> FResult<(String, Vec<FuncParam>, Filter)> {
    parser.expect_token(FilterToken::Def)?;

    let name = match parser.pop_token()? {
        FilterToken::Id(name) => name,
        tok => return Err(FilterParserError::UnexpectedToken(tok)),
    };

    let params = if let FilterToken::LPar = parser.peek_token()? {
        parser.expect_token(FilterToken::LPar)?;

        let params = Params(parser)?;

        parser.expect_token(FilterToken::RPar)?;

        params
    } else {
        Vec::new()
    };

    parser.expect_token(FilterToken::Colon)?;

    let body = Exp(parser)?;

    parser.expect_token(FilterToken::Semicolon)?;

    Ok((name, params, body))
}

/// Params
///   : Param
///   | Params ';' Param
fn Params(parser: &mut FParser) -> FResult<Vec<FuncParam>> {
    parser
        .parse_sequence(Param, FilterToken::Semicolon, FilterToken::RPar)
        .collect::<Result<_, _>>()
}

/// Param
///   : '$' id
///   | '$' Keyword
///   | id   
fn Param(parser: &mut FParser) -> FResult<FuncParam> {
    let tok = parser.pop_token()?;

    if let FilterToken::Id(id) = tok {
        return Ok(FuncParam::FilterParam(id));
    }

    if tok != FilterToken::Var {
        return Err(FilterParserError::UnexpectedToken(tok));
    }

    let tok = parser.pop_token()?;
    if let FilterToken::Id(id) = tok {
        return Ok(FuncParam::VarParam(id));
    }

    if is_keyword(&tok) {
        return Ok(FuncParam::VarParam(tok.to_string()));
    }

    Err(FilterParserError::UnexpectedToken(tok))
}

/// Exp
///   : FuncDef Exp
///   | Term as Pattern '|' Exp
///   | reduce  Term as Pattern '(' Exp ';' Exp ')'
///   | foreach Term as Pattern '(' Exp ';' Exp ';' Exp ')'
///   | foreach Term as Pattern '(' Exp ';' Exp ')'
///   | if Exp then Exp end
///   | if Exp then Exp ElseBody
///   | try Exp catch Exp
///   | try Exp
///   | label '$' id '|' Exp
///   | Exp '?'
///   | Exp '='   Exp
///   | Exp or    Exp
///   | Exp and   Exp
///   | Exp '//'  Exp
///   | Exp '//=' Exp
///   | Exp '|='  Exp
///   | Exp '|'   Exp
///   | Exp ','   Exp
///   | Exp '+'   Exp
///   | Exp '+='  Exp
///   |     '-'   Exp
///   | Exp '-'   Exp
///   | Exp '-='  Exp
///   | Exp '*'   Exp
///   | Exp '*='  Exp
///   | Exp '/'   Exp
///   | Exp '/='  Exp
///   | Exp '%'   Exp
///   | Exp '%='  Exp
///   | Exp '=='  Exp
///   | Exp '!='  Exp
///   | Exp '<'   Exp
///   | Exp '>'   Exp
///   | Exp '<='  Exp
///   | Exp '>='  Exp
///   | Term
fn Exp(parser: &mut FParser) -> FResult<Filter> {
    fn ExpOp(parser: &mut FParser, min_precedence: u8, assoc: OpAssoc) -> FResult<Filter> {
        let mut left = match parser.pop_token()? {
            FilterToken::Def => {
                let (name, params, body) = parser.push_and_then(FilterToken::Def, FuncDef)?;
                let next = Exp(parser)?;
                Filter::FuncDef(name, params, Box::new(body), Box::new(next))
            }
            FilterToken::Reduce => {
                let term = Term(parser)?;
                parser.expect_token(FilterToken::As)?;
                let pattern = Pattern(parser)?;
                parser.expect_token(FilterToken::LPar)?;
                let init = Exp(parser)?;
                parser.expect_token(FilterToken::Semicolon)?;
                let update = Exp(parser)?;
                parser.expect_token(FilterToken::RPar)?;
                Filter::Reduce(Box::new(term), pattern, Box::new(init), Box::new(update))
            }
            FilterToken::Foreach => {
                let term = Term(parser)?;
                parser.expect_token(FilterToken::As)?;
                let pattern = Pattern(parser)?;
                parser.expect_token(FilterToken::LPar)?;
                let init = Exp(parser)?;
                parser.expect_token(FilterToken::Semicolon)?;
                let update = Exp(parser)?;

                let extract = match parser.pop_token()? {
                    FilterToken::RPar => Filter::Identity,
                    FilterToken::Semicolon => Exp(parser)?,
                    tok => return Err(FilterParserError::UnexpectedToken(tok)),
                };

                parser.expect_token(FilterToken::RPar)?;
                Filter::Foreach(
                    Box::new(term),
                    pattern,
                    Box::new(init),
                    Box::new(update),
                    Box::new(extract),
                )
            }
            FilterToken::If => {
                let cond = Exp(parser)?;
                parser.expect_token(FilterToken::Then)?;
                let then = Exp(parser)?;

                let else_ = if let FilterToken::End = parser.peek_token()? {
                    parser.pop_and_produce(Filter::Identity)?
                } else {
                    ElseBody(parser)?
                };

                Filter::IfElse(Box::new(cond), Box::new(then), Box::new(else_))
            }
            FilterToken::Try => {
                let try_ = Exp(parser)?;

                let catch = if let FilterToken::Catch = parser.peek_token()? {
                    parser.expect_token(FilterToken::Catch)?;
                    Exp(parser)?
                } else {
                    Filter::Empty
                };

                Filter::TryCatch(Box::new(try_), Box::new(catch))
            }
            FilterToken::Label => {
                parser.expect_token(FilterToken::Var)?;
                let name = match parser.pop_token()? {
                    FilterToken::Id(name) => name,
                    tok => return Err(FilterParserError::UnexpectedToken(tok)),
                };
                parser.expect_token(FilterToken::Op(Op::Pipe))?;
                let next = Exp(parser)?;
                Filter::Label(name, Box::new(next))
            }
            FilterToken::Op(Op::Minus) => Filter::FuncCall(
                "_negate".to_string(),
                vec![ExpOp(
                    parser,
                    op_prefix_precedence(&Op::Minus),
                    OpAssoc::None,
                )?],
            ),
            tok if is_term(&tok) => {
                // Term
                let term = parser.push_and_then(tok, Term)?;

                if matches!(parser.peek_token()?, FilterToken::As) {
                    parser.expect_token(FilterToken::As)?;
                    let pattern = Pattern(parser)?;
                    parser.expect_token(FilterToken::Op(Op::Pipe))?;
                    let next = Exp(parser)?;
                    Filter::VarDef(pattern, Box::new(term), Box::new(next))
                } else {
                    term
                }
            }
            tok => return Err(FilterParserError::UnexpectedToken(tok)),
        };

        // Operations
        loop {
            let op = match parser.pop_token()? {
                FilterToken::Op(op) => op,
                tok => {
                    parser.push_token(tok);
                    break;
                }
            };

            let (new_precedence, new_assoc) = op_precedence(&op);
            if new_precedence == min_precedence && assoc == OpAssoc::None {
                return Err(FilterParserError::NonAssocOp(op));
            }
            if new_precedence < min_precedence
                || (new_precedence == min_precedence && assoc == OpAssoc::Left)
            {
                // Reduce
                parser.push_token(FilterToken::Op(op));
                break;
            }

            let right = if let Op::Opt = op {
                // Special case for postfix operator
                Filter::Empty
            } else {
                ExpOp(parser, new_precedence, new_assoc)?
            };

            // Helper function to more easily define operator behaviour
            fn op_call(name: &str, left: Filter, right: Filter) -> Filter {
                Filter::FuncCall(name.to_string(), vec![left, right])
            }
            fn assign_op(name: &str, left: Filter, right: Filter) -> Filter {
                Filter::VarDef(
                    "_tmp".to_string(),
                    Box::new(right),
                    Box::new(op_call(
                        "_modify",
                        left,
                        op_call(name, Filter::Identity, Filter::Var("_tmp".to_string())),
                    )),
                )
            }

            // Shift
            left = match op {
                Op::Opt => Filter::TryCatch(Box::new(left), Box::new(Filter::Empty)),
                Op::Assign => Filter::FuncCall("_assign".to_string(), vec![left, right]),
                Op::Or => Filter::IfElse(
                    Box::new(left),
                    Box::new(Filter::json_bool(true)),
                    Box::new(Filter::IfElse(
                        Box::new(right),
                        Box::new(Filter::json_bool(true)),
                        Box::new(Filter::json_bool(false)),
                    )),
                ),
                Op::And => Filter::IfElse(
                    Box::new(left),
                    Box::new(Filter::IfElse(
                        Box::new(right),
                        Box::new(Filter::json_bool(true)),
                        Box::new(Filter::json_bool(false)),
                    )),
                    Box::new(Filter::json_bool(false)),
                ),
                Op::Alt => Filter::Alt(Box::new(left), Box::new(right)),
                Op::AltA => Filter::VarDef(
                    "_tmp".to_string(),
                    Box::new(right),
                    Box::new(op_call(
                        "_modify",
                        left,
                        Filter::Alt(
                            Box::new(Filter::Identity),
                            Box::new(Filter::Var("_tmp".to_string())),
                        ),
                    )),
                ),
                Op::PipeA => op_call("_modify", left, right),
                Op::Pipe => Filter::Pipe(Box::new(left), Box::new(right)),
                Op::Comma => Filter::Comma(Box::new(left), Box::new(right)),
                Op::Plus => op_call("_plus", left, right),
                Op::PlusA => assign_op("_plus", left, right),
                Op::Minus => op_call("_minus", left, right),
                Op::MinusA => assign_op("_minus", left, right),
                Op::Times => op_call("_multiply", left, right),
                Op::TimesA => assign_op("_multiply", left, right),
                Op::Div => op_call("_divide", left, right),
                Op::DivA => assign_op("_divide", left, right),
                Op::Mod => op_call("_mod", left, right),
                Op::ModA => assign_op("_mod", left, right),
                Op::Eq => op_call("_equal", left, right),
                Op::Neq => op_call("_notequal", left, right),
                Op::Lt => op_call("_less", left, right),
                Op::Gt => op_call("_greater", left, right),
                Op::Le => op_call("_lesseq", left, right),
                Op::Ge => op_call("_greatereq", left, right),
                Op::OptAlt => return Err(FilterParserError::UnsupportedOperator(Op::OptAlt)),
            };
        }

        Ok(left)
    }

    ExpOp(parser, 0, OpAssoc::None)
}

/// Pattern
///   : '$' id
fn Pattern(parser: &mut FParser) -> FResult<String> {
    parser.expect_token(FilterToken::Var)?;

    match parser.pop_token()? {
        FilterToken::Id(id) => Ok(id),
        tok => Err(FilterParserError::UnexpectedToken(tok)),
    }
}

/// ElseBody
///   : elif Exp then Exp ElseBody
///   | else Exp end
fn ElseBody(parser: &mut FParser) -> FResult<Filter> {
    let mut elifs = Vec::new();

    while let FilterToken::Elif = parser.peek_token()? {
        parser.expect_token(FilterToken::Elif)?;

        let cond = Exp(parser)?;

        parser.expect_token(FilterToken::Then)?;

        let then = Exp(parser)?;

        elifs.push((Box::new(cond), Box::new(then)));
    }

    parser.expect_token(FilterToken::Else)?;

    let else_ = Exp(parser)?;

    Ok(elifs.into_iter().rfold(else_, |acc, (cond, then)| {
        Filter::IfElse(cond, then, Box::new(acc))
    }))
}

/// Term
///   : '.'
///   | '.' String
///   | '.' String '?'
///   | '..'
///   | String
///   | number
///   | true
///   | false
///   | null
///   | break '$' id
///   | format
///   | '(' Exp ')'
///   | '[' Exp ']'
///   | '[' ']'
///   | '{' MkDict '}'
///   | '$' loc
///   | '$' id
///   | '$' KeywordNoLoc
///   | id
///   | id '(' Args ')'
///   | field
///   | field '?'
///   | Term field
///   | Term field '?'
///   | Term '.' String
///   | Term '.' String '?'
///   | Term '[' Exp ']'
///   | Term '[' Exp ']' '?'
///   | Term '[' ']'
///   | Term '[' ']' '?'
///   | Term '[' Exp ':' Exp ']'
///   | Term '[' Exp ':' Exp ']' '?'
///   | Term '[' Exp ':' ']'
///   | Term '[' Exp ':' ']' '?'
///   | Term '[' ':' Exp ']'
///   | Term '[' ':' Exp ']' '?'
fn Term(parser: &mut FParser) -> FResult<Filter> {
    fn try_opt(parser: &mut FParser, filter: Filter) -> FResult<Filter> {
        if let FilterToken::Op(Op::Opt) = parser.peek_token()? {
            parser.expect_token(FilterToken::Op(Op::Opt))?;
            Ok(Filter::TryCatch(Box::new(filter), Box::new(Filter::Empty)))
        } else {
            Ok(filter)
        }
    }

    let mut term = match parser.pop_token()? {
        FilterToken::Dot => {
            if matches!(
                parser.peek_token()?,
                FilterToken::Quote | FilterToken::Format(_)
            ) {
                let filter = Filter::Project(Box::new(Filter::Identity), Box::new(String(parser)?));
                try_opt(parser, filter)?
            } else {
                Filter::Identity
            }
        }
        FilterToken::Recr => Filter::FuncCall("recurse".to_string(), vec![]),
        FilterToken::Quote => parser.push_and_then(FilterToken::Quote, String)?,
        FilterToken::Num(n) => parser.push_and_then(FilterToken::Num(n), Number)?,
        FilterToken::True => Filter::json_bool(true),
        FilterToken::False => Filter::json_bool(false),
        FilterToken::Null => Filter::json_null(),
        FilterToken::Break => {
            parser.expect_token(FilterToken::Var)?;
            let tok = parser.pop_token()?;
            if let FilterToken::Id(id) = tok {
                Filter::Break(id)
            } else {
                return Err(FilterParserError::UnexpectedToken(tok));
            }
        }
        FilterToken::Format(format) => {
            if let FilterToken::Quote = parser.peek_token()? {
                parser.push_and_then(FilterToken::Format(format), String)?
            } else {
                Filter::FuncCall("format".to_string(), vec![Filter::json_string(format)])
            }
        }
        FilterToken::LPar => {
            let exp = Exp(parser)?;
            parser.expect_token(FilterToken::RPar)?;
            exp
        }
        FilterToken::LBrack => {
            if let FilterToken::RBrack = parser.peek_token()? {
                parser.pop_and_produce(Filter::ArrayLit(Box::new(Filter::Empty)))?
            } else {
                let exp = Exp(parser)?;
                parser.expect_token(FilterToken::RBrack)?;
                Filter::ArrayLit(Box::new(exp))
            }
        }
        FilterToken::LBrace => {
            let dict = MkDict(parser)?;
            parser.expect_token(FilterToken::RBrace)?;
            Filter::ObjectLit(dict)
        }
        FilterToken::Var => match parser.pop_token()? {
            FilterToken::Loc => Filter::Loc("<top-level>".to_string(), parser.pos().line),
            FilterToken::Id(id) => Filter::Var(id),
            tok if is_keyword(&tok) => Filter::Var(tok.to_string()),
            tok => return Err(FilterParserError::UnexpectedToken(tok)),
        },
        FilterToken::Id(id) => {
            if let FilterToken::LPar = parser.peek_token()? {
                parser.expect_token(FilterToken::LPar)?;
                let args = Args(parser)?;
                parser.expect_token(FilterToken::RPar)?;
                Filter::FuncCall(id, args)
            } else {
                Filter::FuncCall(id, vec![])
            }
        }
        FilterToken::Field(field) => {
            let filter = Filter::Project(
                Box::new(Filter::Identity),
                Box::new(Filter::json_string(field)),
            );
            try_opt(parser, filter)?
        }
        tok => return Err(FilterParserError::UnexpectedToken(tok)),
    };

    loop {
        term = match parser.pop_token()? {
            FilterToken::Field(field) => {
                Filter::Project(Box::new(term), Box::new(Filter::json_string(field)))
            }
            FilterToken::Dot => Filter::Project(Box::new(term), Box::new(String(parser)?)),
            FilterToken::LBrack => {
                fn parse_slice(parser: &mut FParser, term: Filter) -> FResult<Filter> {
                    let left = match parser.pop_token()? {
                        FilterToken::RBrack => {
                            // Term[]
                            return Ok(Filter::Pipe(Box::new(term), Box::new(Filter::Iter)));
                        }
                        FilterToken::Colon => None, // Term[:Exp]
                        tok => Some(Box::new(parser.push_and_then(tok, Exp)?)), // Term[Exp] | Term[Exp:] | Term[:Exp]
                    };

                    match parser.pop_token()? {
                        FilterToken::RBrack => {
                            if let Some(left) = left {
                                // Term[Exp]
                                return Ok(Filter::Project(Box::new(term), left));
                            }
                            // Term[:] => Invalid
                            return Err(FilterParserError::UnexpectedToken(FilterToken::RBrack));
                        }
                        FilterToken::Colon => (), // Term[Exp:Exp] | Term[:Exp] | Term[Exp:]
                        tok => return Err(FilterParserError::UnexpectedToken(tok)),
                    };

                    let right = match parser.pop_token()? {
                        FilterToken::RBrack => None,                            // Term[Exp:]
                        tok => Some(Box::new(parser.push_and_then(tok, Exp)?)), // Term[Exp:Exp] | Term[:Exp]
                    };

                    parser.expect_token(FilterToken::RBrack)?;

                    Ok(Filter::Slice(Box::new(term), left, right))
                }

                parse_slice(parser, term)?
            }
            tok => {
                // Finished
                parser.push_token(tok);
                break;
            }
        };

        if let Filter::Slice(_, _, _) = term {
            return try_opt(parser, term);
        }

        term = try_opt(parser, term)?;
    }

    Ok(term)
}

fn is_term(tok: &FilterToken) -> bool {
    matches!(
        tok,
        FilterToken::Dot
            | FilterToken::Recr
            | FilterToken::Quote
            | FilterToken::Num(_)
            | FilterToken::True
            | FilterToken::False
            | FilterToken::Null
            | FilterToken::Break
            | FilterToken::Format(_)
            | FilterToken::LPar
            | FilterToken::LBrack
            | FilterToken::LBrace
            | FilterToken::Var
            | FilterToken::Id(_)
            | FilterToken::Field(_)
    )
}

/// Args
///   : Exp
///   | Args ';' Exp
fn Args(parser: &mut FParser) -> FResult<Vec<Filter>> {
    parser
        .parse_sequence(Exp, FilterToken::Semicolon, FilterToken::RPar)
        .collect::<Result<_, _>>()
}

/// MkDict
///   : {- empty -}
///   | MkDictPair
///   | MkDict ',' MkDictPair
fn MkDict(parser: &mut FParser) -> FResult<Vec<(Filter, Filter)>> {
    parser
        .parse_sequence(MkDictPair, FilterToken::Op(Op::Comma), FilterToken::RBrace)
        .collect::<Result<_, _>>()
}

/// MkDictPair :: { (Filter, Filter) }
///   : id      ':' ExpD
///   | Keyword ':' ExpD
///   | String  ':' ExpD
///   | String
///   | '$' id
///   | '$' Keyword
///   | id
///   | Keyword
///   | '(' Exp ')' ':' ExpD
fn MkDictPair(parser: &mut FParser) -> FResult<(Filter, Filter)> {
    let mut tok = parser.pop_token()?;

    let is_var = matches!(tok, FilterToken::Var);
    let is_exp = matches!(tok, FilterToken::LPar);

    if is_var {
        tok = parser.pop_token()?;
    }

    let key = match tok {
        FilterToken::Id(id) => Filter::json_string(id),
        tok if is_keyword(&tok) => Filter::json_string(tok.to_string()),
        FilterToken::Quote | FilterToken::Format(_) => parser.push_and_then(tok, String)?,
        FilterToken::LPar => {
            let exp = Exp(parser)?;
            parser.expect_token(FilterToken::RPar)?;
            exp
        }
        tok => return Err(FilterParserError::UnexpectedToken(tok)),
    };

    fn default_val(key: &Filter) -> Filter {
        Filter::Project(Box::new(Filter::Identity), Box::new(key.clone()))
    }

    let val = if is_var {
        default_val(&key)
    } else if let FilterToken::Colon = parser.peek_token()? {
        ExpD(parser)?
    } else if is_exp {
        return Err(FilterParserError::UnmatchedExpectation(
            FilterToken::Colon,
            parser.pop_token()?,
        ));
    } else {
        default_val(&key)
    };

    Ok((key, val))
}

/// ExpD
///   : ExpD '|' ExpD
///   | '-' ExpD
///   | Term
fn ExpD(parser: &mut FParser) -> FResult<Filter> {
    fn unary_expd(parser: &mut FParser) -> FResult<Filter> {
        let mut minus_count: usize = 0;

        while let FilterToken::Op(Op::Minus) = parser.peek_token()? {
            parser.expect_token(FilterToken::Op(Op::Minus))?;
            minus_count += 1;
        }

        let mut expd = Term(parser)?;

        while minus_count > 0 {
            minus_count -= 1;
            expd = Filter::FuncCall("_negate".to_string(), vec![expd]);
        }

        Ok(expd)
    }

    let left_expd = unary_expd(parser)?;

    if !matches!(parser.peek_token()?, FilterToken::Op(Op::Pipe)) {
        return Ok(left_expd);
    }

    parser.expect_token(FilterToken::Op(Op::Pipe))?;

    let right_expd = unary_expd(parser)?;

    Ok(Filter::Pipe(Box::new(left_expd), Box::new(right_expd)))
}

/// String
///   : lq InterpString rq
///   | format lq InterpString rq
fn String(parser: &mut FParser) -> FResult<Filter> {
    let format = match parser.pop_token()? {
        FilterToken::Format(format) => format,
        tok => {
            parser.push_token(tok);
            "text".to_string()
        }
    };

    parser.expect_token(FilterToken::Quote)?;

    let string = InterpString(parser)?;

    parser.expect_token(FilterToken::Quote)?;

    Ok(Filter::VarDef(
        "_fmt".to_string(),
        Box::new(Filter::json_string(format)),
        Box::new(string),
    ))
}

/// InterpString
///   : string
///   | InterpString l_interp Exp r_interp string
fn InterpString(parser: &mut FParser) -> FResult<Filter> {
    fn parse_string(
        str_parser: &mut Parser<'_, FilterStringToken, FilterStringParserError>,
    ) -> Result<Filter, FilterStringParserError> {
        let mut str = String::new();

        loop {
            match str_parser.pop_token()? {
                FilterStringToken::String(s) => str.push_str(&s),
                FilterStringToken::Escaped(c) => str.push(c),
                tok @ (FilterStringToken::Quote | FilterStringToken::Interpolation) => {
                    str_parser.push_token(tok);
                    break;
                }
                tok => return Err(FilterStringParserError::UnexpectedToken(tok)),
            }
        }

        Ok(Filter::json_string(str))
    }

    // Morph json parser into string parser
    let mut str_parser = parser.morph::<FilterStringToken, FilterStringParserError>();

    let mut filter = parse_string(&mut str_parser)?;

    loop {
        if let FilterStringToken::Quote = str_parser.peek_token()? {
            // End of string
            str_parser.expect_token(FilterStringToken::Quote)?;
            break;
        }

        // String didn't end => interpolation
        str_parser.expect_token(FilterStringToken::Interpolation)?;

        // Parse interpolated expression
        let mut inner_parser = str_parser.morph::<FilterToken, FilterParserError>();
        let exp = Exp(&mut inner_parser)?;
        inner_parser.expect_token(FilterToken::RPar)?;
        inner_parser.morph_into(&mut str_parser);

        // Parse string after interpolated expression
        let string = parse_string(&mut str_parser)?;

        // Concat: previous filter + interpolated expression + string after interpolation
        filter = Filter::FuncCall(
            "_plus".to_string(),
            vec![
                filter,
                Filter::FuncCall(
                    "_plus".to_string(),
                    vec![
                        Filter::Pipe(
                            Box::new(exp),
                            Box::new(Filter::FuncCall(
                                "format".to_string(),
                                vec![Filter::Var("_fmt".to_string())],
                            )),
                        ),
                        string,
                    ],
                ),
            ],
        );
    }

    // Restore json parser
    str_parser.morph_into(parser);
    parser.push_token(FilterToken::Quote);

    Ok(filter)
}

/// number
fn Number(parser: &mut FParser) -> FResult<Filter> {
    let tok = parser.pop_token()?;
    if let FilterToken::Num(n) = tok {
        Ok(Filter::json_number(n))
    } else {
        Err(FilterParserError::ExpectedNumber(tok))
    }
}

fn is_keyword(tok: &FilterToken) -> bool {
    matches!(
        tok,
        FilterToken::Module
            | FilterToken::Import
            | FilterToken::Include
            | FilterToken::Def
            | FilterToken::As
            | FilterToken::If
            | FilterToken::Then
            | FilterToken::Else
            | FilterToken::Elif
            | FilterToken::End
            | FilterToken::Reduce
            | FilterToken::Foreach
            | FilterToken::Try
            | FilterToken::Catch
            | FilterToken::Label
            | FilterToken::Break
            | FilterToken::Loc
    )
}

#[cfg(test)]
mod tests {
    use crate::{lexer::LexSource, parser::ParserPos};

    use super::*;

    const FILTERS: &str = r#"
    "tobi\(1+1+1)tobi"
    "#;

    #[test]
    fn test_parsing() {
        let source = LexSource::str(FILTERS);
        let mut parser = FilterParser::new(&source);
        match parser.parse() {
            Ok(filter) => {
                println!("{filter:#?}");
            }
            Err(err) => {
                let ParserPos { line, column } = parser.pos();
                if let FilterParserError::LexError(err) = err {
                    println!("lexing error: {err} at line {line}, column {column}");
                } else {
                    println!("parsing error: {err} at line {line}, column {column}");
                }
            }
        }
    }

    #[test]
    fn test_lexing() {
        let source = LexSource::str(FILTERS);
        let mut parser = FilterParser::new(&source).0;

        let mut pos = *parser.pos();
        loop {
            let tok = parser.pop_token();
            if matches!(tok, Ok(FilterToken::EOF)) {
                break;
            }

            println!("{pos:?}");
            println!("{tok:?}");
            pos = *parser.pos();
        }
    }
}
