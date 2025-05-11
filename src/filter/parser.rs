#![allow(non_snake_case)]

use std::sync::Arc;

use crate::{
    filter::lexer::Op,
    lexer::LexSource,
    parser::{ExpectationFailed, Parser, ParserPos},
};

use super::{
    lexer::{FilterLexError, FilterStringToken, FilterToken},
    Filter, FuncParam,
};

use FilterToken as FT;

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

            if !matches!(next_tok, FT::EOF) {
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
            Self::LexError(err) => write!(f, "{err}"),
            Self::StringParserError(err) => write!(f, "error parsing string: {err}"),
            Self::ExpectedNumber(tok) => write!(f, "expected number got {tok}"),
            Self::UnsupportedOperator(op) => write!(f, "unsupported operator {op}"),
            Self::NonAssocOp(op) => write!(f, "non-associative operator {op}"),
            Self::UnmatchedExpectation(expected, actual) => {
                write!(f, "expected {expected} got {actual}")
            }
            Self::UnexpectedToken(tok) => write!(f, "unexpected token {tok}"),
        }
    }
}
impl std::error::Error for FilterParserError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::LexError(err) => Some(err),
            Self::StringParserError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<FilterLexError> for FilterParserError {
    fn from(err: FilterLexError) -> Self {
        Self::LexError(err)
    }
}
impl From<ExpectationFailed<'_, FilterToken>> for FilterParserError {
    fn from(
        ExpectationFailed {
            expected, actual, ..
        }: ExpectationFailed<'_, FilterToken>,
    ) -> Self {
        Self::UnmatchedExpectation(expected, actual)
    }
}
impl From<FilterStringParserError> for FilterParserError {
    fn from(err: FilterStringParserError) -> Self {
        Self::StringParserError(err)
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
            Self::LexError(err) => write!(f, "{err}"),
            Self::UnmatchedExpectation(expected, actual) => {
                write!(f, "expected {expected} got {actual}")
            }
            Self::UnexpectedToken(tok) => write!(f, "Unexpected token {tok}"),
        }
    }
}
impl std::error::Error for FilterStringParserError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::LexError(err) => Some(err),
            _ => None,
        }
    }
}
impl From<FilterLexError> for FilterStringParserError {
    fn from(err: FilterLexError) -> Self {
        Self::LexError(err)
    }
}
impl From<ExpectationFailed<'_, FilterStringToken>> for FilterStringParserError {
    fn from(
        ExpectationFailed {
            expected, actual, ..
        }: ExpectationFailed<'_, FilterStringToken>,
    ) -> Self {
        Self::UnmatchedExpectation(expected, actual)
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
        FT::Def => FuncDefs(parser),
        FT::EOF => Ok(Filter::Identity),
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

    while let FT::Def = parser.peek_token()? {
        let (name, params, body) = FuncDef(parser)?;
        defs.push((name, params, Box::new(body)));
    }

    let (name, params, body) = defs.pop().unwrap();
    let last = Filter::FuncDef(name, params, body, Box::new(Filter(parser)?));

    Ok(defs.into_iter().rfold(last, |acc, (name, params, body)| {
        Filter::FuncDef(name, params, body, Box::new(acc))
    }))
}

/// FuncDef
///   : def id ':' Exp ';'
///   | def id '(' Params ')' ':' Exp ';'
fn FuncDef(parser: &mut FParser) -> FResult<(Arc<str>, Vec<FuncParam>, Filter)> {
    parser.expect_token(FT::Def)?;

    let name = match parser.pop_token()? {
        FT::Id(name) => name,
        tok => return Err(FilterParserError::UnexpectedToken(tok)),
    };

    let params = if let FT::LPar = parser.peek_token()? {
        parser.expect_token(FT::LPar)?;

        let params = Params(parser)?;

        parser.expect_token(FT::RPar)?;

        params
    } else {
        Vec::new()
    };

    parser.expect_token(FT::Colon)?;

    let body = Exp(parser)?;

    parser.expect_token(FT::Semicolon)?;

    Ok((name, params, body))
}

/// Params
///   : Param
///   | Params ';' Param
fn Params(parser: &mut FParser) -> FResult<Vec<FuncParam>> {
    parser
        .parse_sequence(Param, FT::Semicolon, FT::RPar)
        .collect::<Result<_, _>>()
}

/// Param
///   : '$' id
///   | '$' Keyword
///   | id   
fn Param(parser: &mut FParser) -> FResult<FuncParam> {
    let tok = parser.pop_token()?;

    if let FT::Id(id) = tok {
        return Ok(FuncParam::FilterParam(id));
    }

    if tok != FT::Var {
        return Err(FilterParserError::UnexpectedToken(tok));
    }

    let tok = parser.pop_token()?;
    if let FT::Id(id) = tok {
        return Ok(FuncParam::VarParam(id));
    }

    if is_keyword(&tok) {
        return Ok(FuncParam::VarParam(tok.to_string().into()));
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
            FT::Def => {
                let (name, params, body) = parser.push_and_then(FT::Def, FuncDef)?;
                let next = Exp(parser)?;
                Filter::FuncDef(name, params, Box::new(body), Box::new(next))
            }
            FT::Reduce => {
                let term = Term(parser)?;
                parser.expect_token(FT::As)?;
                let pattern = Pattern(parser)?;
                parser.expect_token(FT::LPar)?;
                let init = Exp(parser)?;
                parser.expect_token(FT::Semicolon)?;
                let update = Exp(parser)?;
                parser.expect_token(FT::RPar)?;
                Filter::Reduce(Box::new(term), pattern, Box::new(init), Box::new(update))
            }
            FT::Foreach => {
                let term = Term(parser)?;
                parser.expect_token(FT::As)?;
                let pattern = Pattern(parser)?;
                parser.expect_token(FT::LPar)?;
                let init = Exp(parser)?;
                parser.expect_token(FT::Semicolon)?;
                let update = Exp(parser)?;

                let extract = match parser.pop_token()? {
                    FT::RPar => Filter::Identity,
                    FT::Semicolon => {
                        let exp = Exp(parser)?;
                        parser.expect_token(FT::RPar)?;
                        exp
                    }
                    tok => return Err(FilterParserError::UnexpectedToken(tok)),
                };

                Filter::Foreach(
                    Box::new(term),
                    pattern,
                    Box::new(init),
                    Box::new(update),
                    Box::new(extract),
                )
            }
            FT::If => {
                let cond = Exp(parser)?;
                parser.expect_token(FT::Then)?;
                let then = Exp(parser)?;

                let else_ = if let FT::End = parser.peek_token()? {
                    parser.pop_and_produce(Filter::Identity)?
                } else {
                    ElseBody(parser)?
                };

                Filter::IfElse(Box::new(cond), Box::new(then), Box::new(else_))
            }
            FT::Try => {
                let try_ = Exp(parser)?;

                let catch = if let FT::Catch = parser.peek_token()? {
                    parser.expect_token(FT::Catch)?;
                    Exp(parser)?
                } else {
                    Filter::Empty
                };

                Filter::TryCatch(Box::new(try_), Box::new(catch))
            }
            FT::Label => {
                parser.expect_token(FT::Var)?;
                let name = match parser.pop_token()? {
                    FT::Id(name) => name,
                    tok => return Err(FilterParserError::UnexpectedToken(tok)),
                };
                parser.expect_token(FT::Op(Op::Pipe))?;
                let next = Exp(parser)?;
                Filter::Label(name, Box::new(next))
            }
            FT::Op(Op::Minus) => Filter::FuncCall(
                "_negate".into(),
                vec![ExpOp(
                    parser,
                    op_prefix_precedence(&Op::Minus),
                    OpAssoc::None,
                )?],
            ),
            tok if is_term(&tok) => {
                // Term
                let term = parser.push_and_then(tok, Term)?;

                if matches!(parser.peek_token()?, FT::As) {
                    parser.expect_token(FT::As)?;
                    let pattern = Pattern(parser)?;
                    parser.expect_token(FT::Op(Op::Pipe))?;
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
                FT::Op(op) => op,
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
                parser.push_token(FT::Op(op));
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
                Filter::FuncCall(name.into(), vec![left, right])
            }
            fn assign_op(name: &str, left: Filter, right: Filter) -> Filter {
                Filter::VarDef(
                    "_tmp".into(),
                    Box::new(right),
                    Box::new(op_call(
                        "_modify",
                        left,
                        op_call(name, Filter::Identity, Filter::Var("_tmp".into())),
                    )),
                )
            }

            // Shift
            left = match op {
                Op::Opt => Filter::TryCatch(Box::new(left), Box::new(Filter::Empty)),
                Op::Assign => Filter::FuncCall("_assign".into(), vec![left, right]),
                Op::Or => Filter::IfElse(
                    Box::new(left),
                    Box::new(Filter::bool(true)),
                    Box::new(Filter::IfElse(
                        Box::new(right),
                        Box::new(Filter::bool(true)),
                        Box::new(Filter::bool(false)),
                    )),
                ),
                Op::And => Filter::IfElse(
                    Box::new(left),
                    Box::new(Filter::IfElse(
                        Box::new(right),
                        Box::new(Filter::bool(true)),
                        Box::new(Filter::bool(false)),
                    )),
                    Box::new(Filter::bool(false)),
                ),
                Op::Alt => Filter::Alt(Box::new(left), Box::new(right)),
                Op::AltA => Filter::VarDef(
                    "_tmp".into(),
                    Box::new(right),
                    Box::new(op_call(
                        "_modify",
                        left,
                        Filter::Alt(
                            Box::new(Filter::Identity),
                            Box::new(Filter::Var("_tmp".into())),
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
fn Pattern(parser: &mut FParser) -> FResult<Arc<str>> {
    parser.expect_token(FT::Var)?;

    match parser.pop_token()? {
        FT::Id(id) => Ok(id),
        tok => Err(FilterParserError::UnexpectedToken(tok)),
    }
}

/// ElseBody
///   : elif Exp then Exp ElseBody
///   | else Exp end
fn ElseBody(parser: &mut FParser) -> FResult<Filter> {
    let mut elifs = Vec::new();

    while let FT::Elif = parser.peek_token()? {
        parser.expect_token(FT::Elif)?;

        let cond = Exp(parser)?;

        parser.expect_token(FT::Then)?;

        let then = Exp(parser)?;

        elifs.push((Box::new(cond), Box::new(then)));
    }

    parser.expect_token(FT::Else)?;

    let else_ = Exp(parser)?;

    parser.expect_token(FT::End)?;

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
        if let FT::Op(Op::Opt) = parser.peek_token()? {
            parser.expect_token(FT::Op(Op::Opt))?;
            Ok(Filter::TryCatch(Box::new(filter), Box::new(Filter::Empty)))
        } else {
            Ok(filter)
        }
    }

    let mut term = match parser.pop_token()? {
        FT::Dot => {
            if matches!(parser.peek_token()?, FT::Quote | FT::Format(_)) {
                let filter = Filter::Project(Box::new(Filter::Identity), Box::new(String(parser)?));
                try_opt(parser, filter)?
            } else {
                Filter::Identity
            }
        }
        FT::Recr => Filter::FuncCall("recurse".into(), vec![]),
        FT::Quote => parser.push_and_then(FT::Quote, String)?,
        FT::Num(n) => parser.push_and_then(FT::Num(n), Number)?,
        FT::True => Filter::bool(true),
        FT::False => Filter::bool(false),
        FT::Null => Filter::null(),
        FT::Break => {
            parser.expect_token(FT::Var)?;
            let tok = parser.pop_token()?;
            if let FT::Id(id) = tok {
                Filter::Break(id)
            } else {
                return Err(FilterParserError::UnexpectedToken(tok));
            }
        }
        FT::Format(format) => {
            if let FT::Quote = parser.peek_token()? {
                parser.push_and_then(FT::Format(format), String)?
            } else {
                Filter::FuncCall("format".into(), vec![Filter::string(format.to_string())])
            }
        }
        FT::LPar => {
            let exp = Exp(parser)?;
            parser.expect_token(FT::RPar)?;
            exp
        }
        FT::LBrack => {
            if let FT::RBrack = parser.peek_token()? {
                parser.pop_and_produce(Filter::ArrayLit(Box::new(Filter::Empty)))?
            } else {
                let exp = Exp(parser)?;
                parser.expect_token(FT::RBrack)?;
                Filter::ArrayLit(Box::new(exp))
            }
        }
        FT::LBrace => {
            let dict = MkDict(parser)?;
            parser.expect_token(FT::RBrace)?;
            Filter::ObjectLit(dict)
        }
        FT::Var => match parser.pop_token()? {
            FT::Loc => Filter::Loc("<top-level>".into(), parser.pos().line),
            FT::Id(id) => Filter::Var(id),
            tok if is_keyword(&tok) => Filter::Var(tok.to_string().into()),
            tok => return Err(FilterParserError::UnexpectedToken(tok)),
        },
        FT::Id(id) => {
            if let FT::LPar = parser.peek_token()? {
                parser.expect_token(FT::LPar)?;
                let args = Args(parser)?;
                parser.expect_token(FT::RPar)?;
                Filter::FuncCall(id, args)
            } else {
                Filter::FuncCall(id, vec![])
            }
        }
        FT::Field(field) => {
            let filter =
                Filter::Project(Box::new(Filter::Identity), Box::new(Filter::string(field.to_string())));
            try_opt(parser, filter)?
        }
        tok => return Err(FilterParserError::UnexpectedToken(tok)),
    };

    loop {
        term = match parser.pop_token()? {
            FT::Field(field) => Filter::Project(Box::new(term), Box::new(Filter::string(field.to_string()))),
            FT::Dot => Filter::Project(Box::new(term), Box::new(String(parser)?)),
            FT::LBrack => {
                fn parse_slice(parser: &mut FParser, term: Filter) -> FResult<Filter> {
                    let left = match parser.pop_token()? {
                        FT::RBrack => {
                            // Term[]
                            return Ok(Filter::Pipe(Box::new(term), Box::new(Filter::Iter)));
                        }
                        FT::Colon => {
                            if let FT::RBrack = parser.peek_token()? {
                                // Term[:] => Invalid
                                return Err(FilterParserError::UnexpectedToken(
                                    parser.pop_token()?,
                                ));
                            }
                            None // Term[:Exp]
                        }
                        tok => {
                            let left = parser.push_and_then(tok, Exp)?;
                            match parser.pop_token()? {
                                FT::RBrack => {
                                    // Term[Exp]
                                    return Ok(Filter::Project(Box::new(term), Box::new(left)));
                                }
                                FT::Colon => Some(Box::new(left)), // Term[Exp:Exp] | Term[Exp:]
                                tok => return Err(FilterParserError::UnexpectedToken(tok)),
                            }
                        }
                    };

                    let right = match parser.peek_token()? {
                        FT::RBrack => None,                // Term[Exp:]
                        _ => Some(Box::new(Exp(parser)?)), // Term[Exp:Exp] | Term[:Exp]
                    };

                    parser.expect_token(FT::RBrack)?;

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

        term = try_opt(parser, term)?;
    }

    Ok(term)
}

fn is_term(tok: &FilterToken) -> bool {
    matches!(
        tok,
        FT::Dot
            | FT::Recr
            | FT::Quote
            | FT::Num(_)
            | FT::True
            | FT::False
            | FT::Null
            | FT::Break
            | FT::Format(_)
            | FT::LPar
            | FT::LBrack
            | FT::LBrace
            | FT::Var
            | FT::Id(_)
            | FT::Field(_)
    )
}

/// Args
///   : Exp
///   | Args ';' Exp
fn Args(parser: &mut FParser) -> FResult<Vec<Filter>> {
    parser
        .parse_sequence(Exp, FT::Semicolon, FT::RPar)
        .collect::<Result<_, _>>()
}

/// MkDict
///   : {- empty -}
///   | MkDictPair
///   | MkDict ',' MkDictPair
fn MkDict(parser: &mut FParser) -> FResult<Vec<(Filter, Filter)>> {
    parser
        .parse_sequence(MkDictPair, FT::Op(Op::Comma), FT::RBrace)
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
    let tok = parser.pop_token()?;

    if let FT::Var = tok {
        let key = match parser.pop_token()? {
            FT::Id(id) => id,
            tok if is_keyword(&tok) => tok.to_string().into(),
            tok => return Err(FilterParserError::UnexpectedToken(tok)),
        };

        return Ok((Filter::string(key.to_string()), Filter::Var(key)))
    }

    let mut is_exp = false;

    let key = match tok {
        FT::Id(id) => Filter::string(id.to_string()),
        tok if is_keyword(&tok) => Filter::string(tok.to_string()),
        FT::Quote | FT::Format(_) => parser.push_and_then(tok, String)?,
        FT::LPar => {
            is_exp = true;
            let exp = Exp(parser)?;
            parser.expect_token(FT::RPar)?;
            exp
        }
        tok => return Err(FilterParserError::UnexpectedToken(tok)),
    };

    let val = if let FT::Colon = parser.peek_token()? {
        parser.expect_token(FT::Colon)?;
        ExpD(parser)?
    } else if is_exp {
        return Err(FilterParserError::UnmatchedExpectation(
            FT::Colon,
            parser.pop_token()?,
        ));
    } else {
        Filter::Project(Box::new(Filter::Identity), Box::new(key.clone()))
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

        while let FT::Op(Op::Minus) = parser.peek_token()? {
            parser.expect_token(FT::Op(Op::Minus))?;
            minus_count += 1;
        }

        let mut expd = Term(parser)?;

        while minus_count > 0 {
            minus_count -= 1;
            expd = Filter::FuncCall("_negate".into(), vec![expd]);
        }

        Ok(expd)
    }

    let left_expd = unary_expd(parser)?;

    if !matches!(parser.peek_token()?, FT::Op(Op::Pipe)) {
        return Ok(left_expd);
    }

    parser.expect_token(FT::Op(Op::Pipe))?;

    let right_expd = unary_expd(parser)?;

    Ok(Filter::Pipe(Box::new(left_expd), Box::new(right_expd)))
}

/// String
///   : lq InterpString rq
///   | format lq InterpString rq
fn String(parser: &mut FParser) -> FResult<Filter> {
    let format = match parser.pop_token()? {
        FT::Format(format) => format,
        tok => {
            parser.push_token(tok);
            "text".into()
        }
    };

    parser.expect_token(FT::Quote)?;

    let string = InterpString(parser)?;

    parser.expect_token(FT::Quote)?;

    Ok(Filter::VarDef(
        "_fmt".into(),
        Box::new(Filter::string(format.to_string())),
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

        Ok(Filter::string(str))
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
        inner_parser.expect_token(FT::RPar)?;
        inner_parser.morph_into(&mut str_parser);

        // Parse string after interpolated expression
        let string = parse_string(&mut str_parser)?;

        // Concat: previous filter + interpolated expression + string after interpolation
        filter = Filter::FuncCall(
            "_plus".into(),
            vec![
                filter,
                Filter::FuncCall(
                    "_plus".into(),
                    vec![
                        Filter::Pipe(
                            Box::new(exp),
                            Box::new(Filter::FuncCall(
                                "format".into(),
                                vec![Filter::Var("_fmt".into())],
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
    parser.push_token(FT::Quote);

    Ok(filter)
}

/// number
fn Number(parser: &mut FParser) -> FResult<Filter> {
    let tok = parser.pop_token()?;
    if let FT::Num(n) = tok {
        Ok(Filter::number(n))
    } else {
        Err(FilterParserError::ExpectedNumber(tok))
    }
}

fn is_keyword(tok: &FilterToken) -> bool {
    matches!(
        tok,
        FT::Module
            | FT::Import
            | FT::Include
            | FT::Def
            | FT::As
            | FT::If
            | FT::Then
            | FT::Else
            | FT::Elif
            | FT::End
            | FT::Reduce
            | FT::Foreach
            | FT::Try
            | FT::Catch
            | FT::Label
            | FT::Break
            | FT::Loc
    )
}

#[cfg(test)]
mod tests {
    use crate::{lexer::LexSource, parser::ParserPos};

    use super::*;

    const FILTERS: &str = r#"
    .[3:3][1:]
    "#;

    #[test]
    fn test_parsing() {
        let source = LexSource::str(FILTERS);
        let mut parser = FilterParser::new(&source);
        match parser.parse() {
            Ok(filter) => {
                println!("{filter}");
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
            if matches!(tok, Ok(FT::EOF)) {
                break;
            }

            println!("{pos:?}");
            println!("{tok:?}");
            pos = *parser.pos();
        }
    }
}
