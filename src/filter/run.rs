mod builtins;
mod gen;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    iter::zip,
    rc::Rc,
};

use builtins::JqBuiltins;
use either::Either;

use crate::{json::Json, math::Number};

use super::{Filter, FuncParam};

pub use gen::RunGen;
use gen::{yield_, RunOut};

#[derive(Debug, Clone, PartialEq)]
pub enum RunEndValue {
    Error(Json),
    Break(String),
    Halt(String),
}
pub type RunEnd = Option<RunEndValue>;

pub type RunValue = Result<Json, RunEndValue>;

#[derive(Debug, Clone, PartialEq)]
enum RunFile {
    Main,
    Module(String),
}

#[derive(Debug, Clone)]
struct FuncDef {
    state: RunState,
    params: Vec<String>,
    body: Filter,
}

#[derive(Debug, Clone)]
struct CurrentFuncDef {
    state: Rc<RefCell<RunState>>,
    params: Vec<String>,
    body: Filter,
}

#[derive(Debug, Clone, Default)]
struct RunState {
    vars: HashMap<String, Json>,
    funcs: HashMap<(String, usize), FuncDef>,
    labels: HashSet<String>,
    current_func: Option<(String, usize, CurrentFuncDef)>,
}
impl RunState {
    fn is_top_level(&self) -> bool {
        self.current_func.is_none()
    }
}

#[derive(Debug, Clone)]
pub struct RunCtx {
    file: RunFile,
    jq_builtins: JqBuiltins,
    state: RunState,
}
impl RunCtx {
    pub fn new() -> Self {
        let jq_builtins = builtins::jq_builtins();
        Self {
            file: RunFile::Main,
            jq_builtins,
            state: RunState::default(),
        }
    }

    fn is_top_level(&self) -> bool {
        self.state.is_top_level()
    }
}
impl Default for RunCtx {
    fn default() -> Self {
        Self::new()
    }
}

async fn run(out: RunOut<'_>, ctx: &RunCtx, filter: &Filter, json: &Json) -> RunEnd {
    match filter {
        Filter::Identity => {
            yield_!(out, json.clone());
            None
        }
        Filter::Empty => None,
        Filter::Json(json) => {
            yield_!(out, json.clone());
            None
        }
        Filter::Var(name) => run_var(out, ctx, name).await,
        Filter::VarDef(name, body, next) => run_var_def(out, ctx, name, body, next, json).await,
        Filter::ArrayLit(items) => run_array_lit(out, ctx, items, json).await,
        Filter::ObjectLit(items) => run_object_lit(out, ctx, items, json).await,
        Filter::Project(term, key) => run_project(out, ctx, term, key, json).await,
        Filter::Slice(term, left, right) => {
            run_slice(out, ctx, term, left.as_deref(), right.as_deref(), json).await
        }
        Filter::Iter => run_iter(out, json).await,
        Filter::Pipe(left, right) => run_pipe(out, ctx, left, right, json).await,
        Filter::Alt(left, right) => run_alt(out, ctx, left, right, json).await,
        Filter::TryCatch(try_, catch_) => run_try_catch(out, ctx, try_, catch_, json).await,
        Filter::Comma(left, right) => run_comma(out, ctx, left, right, json).await,
        Filter::IfElse(cond, then, else_) => run_if_else(out, ctx, cond, then, else_, json).await,
        Filter::Reduce(exp, name, init, update) => {
            run_reduce(out, ctx, exp, name, init, update, json).await
        }
        Filter::Foreach(exp, name, init, update, extract) => {
            run_foreach(out, ctx, exp, name, init, update, extract, json).await
        }
        Filter::FuncDef(name, params, body, next) => {
            run_func_def(out, ctx, name, params, body, next, json).await
        }
        Filter::FuncCall(name, args) => run_func_call(out, ctx, name, args, json).await,
        Filter::Label(label, then) => run_label(out, ctx, label, then, json).await,
        Filter::Break(label) => run_break(ctx, label).await,
        Filter::Loc(file, line) => run_loc(out, file, *line).await,
    }
}

async fn run_var(out: RunOut<'_>, ctx: &RunCtx, name: &str) -> RunEnd {
    match ctx.state.vars.get(name) {
        Some(val) => yield_!(out, val.clone()),
        None => return Some(str_error(format!("${name} is not defined"))),
    }
    None
}

async fn run_var_def(
    out: RunOut<'_>,
    ctx: &RunCtx,
    name: &str,
    body: &Filter,
    next: &Filter,
    json: &Json,
) -> RunEnd {
    let mut bodies = RunGen::build(ctx, body, json);

    let new_ctx = &mut ctx.clone();

    for body in &mut bodies {
        new_ctx.state.vars.insert(name.to_string(), body);

        let mut nexts = RunGen::build(new_ctx, next, json);
        for next in &mut nexts {
            yield_!(out, next);
        }
        if let Some(end) = nexts.end() {
            return Some(end);
        }
    }
    if let Some(end) = bodies.end() {
        return Some(end);
    }

    None
}

async fn run_array_lit(out: RunOut<'_>, ctx: &RunCtx, items: &Filter, json: &Json) -> RunEnd {
    let mut items_gen = RunGen::build(ctx, items, json);
    let items = items_gen.by_ref().collect::<Vec<_>>();
    if let Some(end) = items_gen.end() {
        return Some(end);
    }

    yield_!(out, Json::Array(items));
    None
}

async fn run_object_lit(
    out: RunOut<'_>,
    ctx: &RunCtx,
    items: &[(Filter, Filter)],
    json: &Json,
) -> RunEnd {
    fn build_pairs(
        ctx: &RunCtx,
        key: &Filter,
        value: &Filter,
        json: &Json,
    ) -> Vec<Result<(String, Json), RunEndValue>> {
        let mut pairs = Vec::new();
        let mut keys = RunGen::build(ctx, key, json);
        for key in &mut keys {
            let mut values = RunGen::build(ctx, value, json);
            for value in &mut values {
                if let Json::String(key) = &key {
                    pairs.push(Ok((key.clone(), value)));
                } else {
                    pairs.push(Err(str_error(format!(
                        "Cannot use {} as object key",
                        json_fmt_error(&key)
                    ))));
                }
            }
            if let Some(end) = values.end() {
                pairs.push(Err(end));
                return pairs;
            }
        }
        if let Some(end) = keys.end() {
            pairs.push(Err(end));
            return pairs;
        }

        pairs
    }

    fn cartesian_product(
        pair_options: &[Vec<Result<(String, Json), RunEndValue>>],
    ) -> (Vec<HashMap<String, Json>>, RunEnd) {
        fn rec(
            pair_options: &[Vec<Result<(String, Json), RunEndValue>>],
            current: HashMap<String, Json>,
            result: &mut Vec<HashMap<String, Json>>,
        ) -> RunEnd {
            if pair_options.is_empty() {
                result.push(current);
                return None;
            }

            for option in &pair_options[0] {
                match option {
                    Ok((key, value)) => {
                        let mut new_current = current.clone();
                        new_current.insert(key.clone(), value.clone());
                        if let Some(error) = rec(&pair_options[1..], new_current, result) {
                            return Some(error);
                        }
                    }
                    Err(s) => {
                        return Some(s.clone());
                    }
                }
            }

            None
        }

        let mut result = Vec::new();
        let err = rec(pair_options, HashMap::new(), &mut result);
        (result, err)
    }

    let pair_options = items
        .iter()
        .map(|(key, value)| build_pairs(ctx, key, value, json))
        .collect::<Vec<_>>();

    let (products, end) = cartesian_product(&pair_options);

    for obj in products {
        yield_!(out, Json::Object(obj));
    }

    end
}

async fn run_project(
    out: RunOut<'_>,
    ctx: &RunCtx,
    term: &Filter,
    exp: &Filter,
    json: &Json,
) -> RunEnd {
    fn project(term: &Json, exp: &Json) -> RunValue {
        match (term, exp) {
            (Json::Object(obj), Json::String(key)) => {
                Ok(obj.get(key).cloned().unwrap_or(Json::Null))
            }
            (Json::Array(arr), Json::Number(Number::Int(n))) => Ok(if n.is_negative() {
                (n.clone() + arr.len()).to_usize()
            } else {
                n.to_usize()
            }
            .and_then(|idx| arr.get(idx))
            .cloned()
            .unwrap_or(Json::Null)),
            (Json::Array(_), Json::Number(Number::Decimal(_))) => Ok(Json::Null),
            (Json::Array(haystack), Json::Array(needle)) => Ok(Json::Array(
                haystack
                    .windows(needle.len())
                    .enumerate()
                    .filter_map(|(i, window)| {
                        if window == needle.as_slice() {
                            Some(i)
                        } else {
                            None
                        }
                    })
                    .map(|i| Json::Number(Number::Int(i.into())))
                    .collect(),
            )),
            (Json::Null, Json::Object(_)) => Ok(Json::Null),
            (Json::Null, Json::String(_)) => Ok(Json::Null),
            (Json::Null, Json::Number(_)) => Ok(Json::Null),
            (term, key) => Err(str_error(format!(
                "Cannot index {} with {} {}",
                json_fmt_type(term),
                json_fmt_type(key),
                json_fmt_bounded(key),
            ))),
        }
    }

    let mut terms = RunGen::build(ctx, term, json);

    for term in &mut terms {
        let mut exps = RunGen::build(ctx, exp, json);

        for exp in &mut exps {
            match project(&term, &exp) {
                Ok(val) => yield_!(out, val),
                Err(end) => return Some(end),
            }
        }
        if let Some(end) = exps.end() {
            return Some(end);
        }
    }
    if let Some(end) = terms.end() {
        return Some(end);
    }

    None
}

async fn run_slice(
    out: RunOut<'_>,
    ctx: &RunCtx,
    term: &Filter,
    left: Option<&Filter>,
    right: Option<&Filter>,
    json: &Json,
) -> RunEnd {
    fn num_to_idx(n: &Number, len: usize, round: rug::float::Round) -> Option<usize> {
        fn cycle_idx(n: &rug::Integer, len: usize) -> usize {
            if n.is_negative() {
                (n.clone() + len).to_usize().unwrap_or(usize::MIN)
            } else {
                n.to_usize().unwrap_or(usize::MAX)
            }
        }

        match n {
            Number::Int(n) => Some(cycle_idx(n, len)),
            Number::Decimal(f) => {
                if f.is_nan() {
                    None
                } else if f.is_infinite() {
                    Some(if f.is_sign_positive() { len } else { 0 })
                } else {
                    Some(cycle_idx(&f.to_integer_round(round).unwrap().0, len))
                }
            }
        }
    }

    fn slice(term: &Json, left: &Json, right: &Json) -> RunValue {
        match (term, left, right) {
            (Json::Array(arr), Json::Number(l), Json::Number(r)) => {
                let len = arr.len();
                let l = match num_to_idx(l, len, rug::float::Round::Down) {
                    Some(l) => l,
                    None => return Ok(Json::Null),
                };
                let r = match num_to_idx(r, len, rug::float::Round::Up) {
                    Some(r) => r,
                    None => return Ok(Json::Null),
                };

                if r < l {
                    return Ok(Json::Array(vec![]));
                }

                Ok(Json::Array(arr[l..r].to_vec()))
            }
            (Json::String(s), Json::Number(l), Json::Number(r)) => {
                let len = s.len();
                let l = match num_to_idx(l, len, rug::float::Round::Down) {
                    Some(l) => l,
                    None => return Ok(Json::Null),
                };
                let r = match num_to_idx(r, len, rug::float::Round::Up) {
                    Some(r) => r,
                    None => return Ok(Json::Null),
                };

                Ok(Json::String(s[l..r].to_string()))
            }
            (Json::Null, Json::Number(_), Json::Number(_)) => Ok(Json::Null),
            (Json::Array(_), anyl, anyr) | (Json::Null, anyl, anyr) => Err(str_error(format!(
                "Start and end indices of an array slice must be numbers, not {} and {}",
                json_fmt_type(anyl),
                json_fmt_type(anyr)
            ))),
            (any, _, _) => Err(str_error(
                json_fmt_error(any) + " cannot be sliced, only arrays or null",
            )),
        }
    }

    let mut terms = RunGen::build(ctx, term, json);
    for term in &mut terms {
        let mut lefts = match left {
            Some(left) => Either::Left(RunGen::build(ctx, left, json)),
            None => Either::Right(std::iter::once(Json::Number(Number::Int(
                rug::Integer::ZERO,
            )))),
        };

        for left in &mut lefts {
            let mut rights = match right {
                Some(right) => Either::Left(RunGen::build(ctx, right, json)),
                None => Either::Right(std::iter::once(Json::Number(Number::Int(match &term {
                    Json::Array(arr) => arr.len().into(),
                    Json::String(s) => s.len().into(),
                    _ => rug::Integer::ZERO,
                })))),
            };

            for right in &mut rights {
                match slice(&term, &left, &right) {
                    Ok(val) => yield_!(out, val),
                    Err(end) => return Some(end),
                }
            }
            if let Some(end) = rights.left().and_then(|mut rights| rights.end()) {
                return Some(end);
            }
        }
        if let Some(end) = lefts.left().and_then(|mut lefts| lefts.end()) {
            return Some(end);
        }
    }
    if let Some(end) = terms.end() {
        return Some(end);
    }

    None
}

async fn run_iter(out: RunOut<'_>, json: &Json) -> RunEnd {
    match json {
        Json::Array(arr) => {
            for json in arr.iter().cloned() {
                yield_!(out, json);
            }
        }
        Json::Object(obj) => {
            for json in obj.values().cloned() {
                yield_!(out, json);
            }
        }
        any => {
            return Some(str_error(format!(
                "Cannot iterate over {}",
                json_fmt_error(any)
            )))
        }
    }

    None
}

async fn run_pipe(
    out: RunOut<'_>,
    ctx: &RunCtx,
    left: &Filter,
    right: &Filter,
    json: &Json,
) -> RunEnd {
    let mut left_jsons = RunGen::build(ctx, left, json);

    for left_json in &mut left_jsons {
        let mut right_jsons = RunGen::build(ctx, right, &left_json);

        for right_json in &mut right_jsons {
            yield_!(out, right_json);
        }
        if let Some(end) = right_jsons.end() {
            return Some(end);
        }
    }
    if let Some(end) = left_jsons.end() {
        return Some(end);
    }

    None
}

async fn run_alt(
    out: RunOut<'_>,
    ctx: &RunCtx,
    left: &Filter,
    right: &Filter,
    json: &Json,
) -> RunEnd {
    let mut lefts = RunGen::build(ctx, left, json);

    let mut has_valid_left = false;
    for left in &mut lefts {
        if json.to_bool() {
            has_valid_left = true;
            yield_!(out, left);
        }
    }
    if let Some(end) = lefts.end() {
        return Some(end);
    }

    if !has_valid_left {
        let mut rights = RunGen::build(ctx, right, json);
        for right in &mut rights {
            yield_!(out, right);
        }
        if let Some(end) = rights.end() {
            return Some(end);
        }
    }

    None
}

async fn run_try_catch(
    out: RunOut<'_>,
    ctx: &RunCtx,
    try_: &Filter,
    catch_: &Filter,
    json: &Json,
) -> RunEnd {
    let mut trys = RunGen::build(ctx, try_, json);

    for json in &mut trys {
        yield_!(out, json);
    }
    match trys.end() {
        Some(RunEndValue::Error(err)) => {
            let mut catches = RunGen::build(ctx, catch_, &err);

            for json in &mut catches {
                yield_!(out, json);
            }
            if let Some(end) = catches.end() {
                return Some(end);
            }
        }
        Some(end) => return Some(end),
        None => {}
    }

    None
}

async fn run_comma(
    out: RunOut<'_>,
    ctx: &RunCtx,
    left: &Filter,
    right: &Filter,
    json: &Json,
) -> RunEnd {
    let mut left_jsons = RunGen::build(ctx, left, json);

    for left_json in &mut left_jsons {
        yield_!(out, left_json);
    }
    if let Some(end) = left_jsons.end() {
        return Some(end);
    }

    let mut right_jsons = RunGen::build(ctx, right, json);

    for right_json in &mut right_jsons {
        yield_!(out, right_json);
    }
    if let Some(end) = right_jsons.end() {
        return Some(end);
    }

    None
}

async fn run_if_else(
    out: RunOut<'_>,
    ctx: &RunCtx,
    cond: &Filter,
    then: &Filter,
    else_: &Filter,
    json: &Json,
) -> RunEnd {
    let mut conds = RunGen::build(ctx, cond, json);

    for cond in &mut conds {
        let filter_branch = if cond.to_bool() { then } else { else_ };
        let mut branches = RunGen::build(ctx, filter_branch, json);

        for branch in &mut branches {
            yield_!(out, branch);
        }
        if let Some(end) = branches.end() {
            return Some(end);
        }
    }
    if let Some(end) = conds.end() {
        return Some(end);
    }

    None
}

async fn run_reduce(
    out: RunOut<'_>,
    ctx: &RunCtx,
    exp: &Filter,
    name: &str,
    init: &Filter,
    update: &Filter,
    json: &Json,
) -> RunEnd {
    // exp_input is json just the first time, then it is null
    let mut exp_input = Some(json);

    let mut bases = RunGen::build(ctx, init, json);
    for base in &mut bases {
        let mut stream = RunGen::build(ctx, exp, exp_input.take().unwrap_or(&Json::Null));

        let new_ctx = &mut ctx.clone();

        let mut acc = base;
        for val in &mut stream {
            new_ctx.state.vars.insert(name.to_string(), val.clone());

            acc = {
                let mut updates = RunGen::build(new_ctx, update, &acc);
                let new_acc = updates.by_ref().fold(Json::Null, |_, j| j);
                if let Some(end) = updates.end() {
                    return Some(end);
                }
                new_acc
            }
        }
        if let Some(end) = stream.end() {
            return Some(end);
        }

        // We only yield the accumulator's last value
        yield_!(out, acc);
    }
    if let Some(end) = bases.end() {
        return Some(end);
    }

    None
}

#[allow(clippy::too_many_arguments)]
async fn run_foreach(
    out: RunOut<'_>,
    ctx: &RunCtx,
    exp: &Filter,
    name: &str,
    init: &Filter,
    update: &Filter,
    extract: &Filter,
    json: &Json,
) -> RunEnd {
    // exp_input is json just the first time, then it is null
    let mut exp_input = Some(json);

    let mut bases = RunGen::build(ctx, init, json);
    for base in &mut bases {
        let mut stream = RunGen::build(ctx, exp, exp_input.take().unwrap_or(&Json::Null));

        let new_ctx = &mut ctx.clone();

        let mut acc = base;
        for val in &mut stream {
            new_ctx.state.vars.insert(name.to_string(), val.clone());

            acc = {
                let mut updates = RunGen::build(new_ctx, update, &acc);

                let mut new_acc = Json::Null;

                for update in &mut updates {
                    new_acc = update;

                    let mut extracts = RunGen::build(new_ctx, extract, &new_acc);
                    for extract in &mut extracts {
                        yield_!(out, extract);
                    }
                    if let Some(end) = extracts.end() {
                        return Some(end);
                    }
                }
                if let Some(end) = updates.end() {
                    return Some(end);
                }

                new_acc
            }
        }
        if let Some(end) = stream.end() {
            return Some(end);
        }
    }
    if let Some(end) = bases.end() {
        return Some(end);
    }

    None
}

async fn run_func_def(
    out: RunOut<'_>,
    ctx: &RunCtx,
    name: &str,
    params: &[FuncParam],
    body: &Filter,
    next: &Filter,
    json: &Json,
) -> RunEnd {
    let mut body = body.clone();
    let mut param_names = Vec::new();

    for param in params {
        match param {
            FuncParam::FilterParam(name) => {
                param_names.push(name.clone());
            }
            FuncParam::VarParam(name) => {
                body = Filter::VarDef(
                    name.clone(),
                    Box::new(Filter::FuncCall(name.clone(), Vec::new())),
                    Box::new(body.clone()),
                );
                param_names.push(name.clone());
            }
        }
    }

    let new_ctx = &mut ctx.clone();
    // let prev_func = ctx.state.funcs.remove(&(name.to_string(), params.len()));

    {
        let func_state = Rc::new(RefCell::new(new_ctx.state.clone()));
        func_state.borrow_mut().current_func = Some((
            name.to_string(),
            params.len(),
            CurrentFuncDef {
                state: func_state.clone(),
                params: param_names.clone(),
                body: body.clone(),
            },
        ));

        new_ctx.state.funcs.insert(
            (name.to_string(), params.len()),
            FuncDef {
                state: func_state.borrow().clone(),
                params: param_names,
                body,
            },
        );

        // drop func_ctx
    }

    let mut nexts = RunGen::build(new_ctx, next, json);
    for next in &mut nexts {
        yield_!(out, next);
    }
    let ret = nexts.end();

    // Restore scope only on the top level of modules
    // let restore_scope = ctx.file == RunFile::Main || ctx.is_top_level();
    // if restore_scope {
    //     if let Some(prev_func) = prev_func {
    //         ctx.state
    //             .funcs
    //             .insert((name.to_string(), params.len()), prev_func);
    //     } else {
    //         ctx.state.funcs.remove(&(name.to_string(), params.len()));
    //     }
    // }

    ret
}

async fn run_func_call(
    out: RunOut<'_>,
    ctx: &RunCtx,
    name: &str,
    args: &[Filter],
    json: &Json,
) -> RunEnd {
    async fn func_call(
        out: RunOut<'_>,
        ctx: &mut RunCtx,
        mut func: FuncDef,
        args: &[Filter],
        json: &Json,
    ) -> RunEnd {
        for (param, arg) in zip(func.params, args) {
            func.state.funcs.insert(
                (param, 0),
                FuncDef {
                    state: ctx.state.clone(),
                    params: vec![],
                    body: arg.clone(),
                },
            );
        }

        std::mem::swap(&mut ctx.state, &mut func.state);

        let ret = {
            let mut results = RunGen::build(ctx, &func.body, json);
            for result in &mut results {
                yield_!(out, result);
            }
            results.end()
        };

        std::mem::swap(&mut ctx.state, &mut func.state);

        ret
    }

    let argc = args.len();

    let new_ctx = &mut ctx.clone();

    if let Some((curr_name, curr_argc, curr_func)) = &new_ctx.state.current_func {
        if curr_name == name && *curr_argc == argc {
            let func_def = FuncDef {
                state: curr_func.state.borrow().clone(),
                params: curr_func.params.clone(),
                body: curr_func.body.clone(),
            };
            return func_call(out, new_ctx, func_def, args, json).await;
        }
    }

    if let Some(func) = new_ctx.state.funcs.get(&(name.to_string(), argc)) {
        return func_call(out, new_ctx, func.clone(), args, json).await;
    }

    if let Some(func) = new_ctx.jq_builtins.get(&(name, argc)) {
        return func_call(out, new_ctx, func.clone(), args, json).await;
    }

    // Only option left is to be an rs builtin
    builtins::run_rs_builtin(name, argc, out, new_ctx, args, json).await
}

async fn run_label(
    out: RunOut<'_>,
    ctx: &RunCtx,
    label: &str,
    then: &Filter,
    json: &Json,
) -> RunEnd {
    let new_ctx = &mut ctx.clone();

    // let inserted = new_ctx.state.labels.insert(label.to_string());
    new_ctx.state.labels.insert(label.to_string());

    let mut results = RunGen::build(ctx, then, json);
    for result in &mut results {
        yield_!(out, result);
    }

    // if inserted {
    //     ctx.state.labels.remove(label);
    // }

    if let Some(RunEndValue::Break(break_label)) = results.end() {
        if break_label != label {
            return Some(RunEndValue::Break(break_label));
        }
    }

    None
}

async fn run_break(ctx: &RunCtx, label: &str) -> RunEnd {
    if ctx.state.labels.contains(label) {
        Some(RunEndValue::Break(label.to_string()))
    } else {
        Some(str_error(format!("$*label-{label} is not defined")))
    }
}

async fn run_loc(out: RunOut<'_>, file: &str, line: usize) -> RunEnd {
    yield_!(
        out,
        Json::Object(HashMap::from([
            ("file".to_string(), Json::String(file.to_string())),
            ("line".to_string(), Json::Number(Number::Int(line.into()))),
        ]))
    );
    None
}

// ------------------- Error Utils --------------------- //

pub(crate) fn str_error(s: String) -> RunEndValue {
    RunEndValue::Error(Json::String(s))
}

pub(crate) fn json_fmt_error(json: &Json) -> String {
    format!("{} ({})", json_fmt_type(json), json_fmt_bounded(json))
}

fn json_fmt_bounded(json: &Json) -> String {
    const MAX_SIZE: usize = 11;

    let fmt = json.format_compact();
    if fmt.len() > MAX_SIZE {
        let fmt = &fmt[..MAX_SIZE];
        format!("{fmt}...")
    } else {
        fmt
    }
}

fn json_fmt_type(json: &Json) -> &'static str {
    match json {
        Json::Object(_) => "object",
        Json::Array(_) => "array",
        Json::String(_) => "string",
        Json::Number(_) => "number",
        Json::Bool(_) => "bool",
        Json::Null => "null",
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_run() {
        let input = r#"
            [1,2,3,4]
        "#;
        let filter = r#"
            foreach (.[],.[]) as $item (0; . + $item)
        "#;

        let input: Json = input.parse().expect("json input parse error");
        println!("{input:?}");

        let filter: Filter = filter.parse().expect("filter parse error");
        println!("{filter:?}");

        let ctx = RunCtx::new();
        let mut run_gen = filter.run(&ctx, &input);
        for json in &mut run_gen {
            println!("{json}");
        }
        if let Some(end) = run_gen.end() {
            match end {
                RunEndValue::Error(err) => println!("error: {err}"),
                RunEndValue::Break(br) => println!("break: {br}"),
                RunEndValue::Halt(hlt) => println!("halt: {hlt}"),
            }
        }
    }
}
