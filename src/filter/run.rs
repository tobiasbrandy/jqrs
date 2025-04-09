use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    iter::zip,
    rc::Rc,
};

use builtins::{JqBuiltins, RsBuiltins};

use crate::{json::Json, math::Number};

use super::{Filter, FuncParam};

mod builtins;

#[derive(Debug, Clone, PartialEq)]
pub enum RunStopValue {
    Error(Json),
    Break(String),
    Halt(String),
}

pub type RunValue = Result<Json, RunStopValue>;

pub type RunResult = Vec<RunValue>;

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
    rs_builtins: RsBuiltins,
    jq_builtins: JqBuiltins,
    state: RunState,
}
impl RunCtx {
    pub fn new() -> Self {
        let (rs_builtins, jq_builtins) = builtins::builtins();
        Self {
            file: RunFile::Main,
            rs_builtins,
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

pub fn run_filter(ctx: &mut RunCtx, filter: &Filter, json: &Json) -> RunResult {
    run(ctx, filter, json)
}

fn run(ctx: &mut RunCtx, filter: &Filter, json: &Json) -> RunResult {
    match filter {
        Filter::Identity => vec![Ok(json.clone())],
        Filter::Empty => vec![],
        Filter::Json(json) => vec![Ok(json.clone())],
        Filter::Var(name) => run_var(ctx, name),
        Filter::VarDef(name, body, next) => run_var_def(ctx, name, body, next, json),
        Filter::ArrayLit(items) => run_array_lit(ctx, items, json),
        Filter::ObjectLit(items) => run_object_lit(ctx, items, json),
        Filter::Project(term, key) => run_project(ctx, term, key, json),
        Filter::Slice(term, left, right) => {
            run_slice(ctx, term, left.as_deref(), right.as_deref(), json)
        }
        Filter::Iter => run_iter(json),
        Filter::Pipe(left, right) => run_pipe(ctx, left, right, json),
        Filter::Alt(left, right) => run_alt(ctx, left, right, json),
        Filter::TryCatch(try_, catch_) => run_try_catch(ctx, try_, catch_, json),
        Filter::Comma(left, right) => run_comma(ctx, left, right, json),
        Filter::IfElse(cond, then, else_) => run_if_else(ctx, cond, then, else_, json),
        Filter::Reduce(exp, name, init, update) => run_reduce(ctx, exp, name, init, update, json),
        Filter::Foreach(exp, name, init, update, extract) => {
            run_foreach(ctx, exp, name, init, update, extract, json)
        }
        Filter::FuncDef(name, params, body, next) => {
            run_func_def(ctx, name, params, body, next, json)
        }
        Filter::FuncCall(name, args) => run_func_call(ctx, name, args, json),
        Filter::Label(label, then) => run_label(ctx, label, then, json),
        Filter::Break(label) => run_break(ctx, label),
        Filter::Loc(file, line) => run_loc(file, *line),
    }
}

fn run_var(ctx: &mut RunCtx, name: &str) -> RunResult {
    match ctx.state.vars.get(name) {
        Some(val) => vec![Ok(val.clone())],
        None => vec![Err(str_error(format!("${name} is not defined")))],
    }
}

fn run_var_def(
    ctx: &mut RunCtx,
    name: &str,
    body: &Filter,
    next: &Filter,
    json: &Json,
) -> RunResult {
    let bodies = run(ctx, body, json);

    let og_var = ctx.state.vars.remove(name);

    let mut ret = Vec::new();

    'stop: for body in bodies {
        let body = match body {
            Ok(body) => body,
            stop => {
                ret.push(stop);
                break 'stop;
            }
        };

        ctx.state.vars.insert(name.to_string(), body);

        for next in run(ctx, next, json) {
            match next {
                Ok(next) => {
                    ret.push(Ok(next));
                }
                stop => {
                    ret.push(stop);
                    break 'stop;
                }
            }
        }
    }

    if let Some(og_var) = og_var {
        ctx.state.vars.insert(name.to_string(), og_var);
    }

    ret
}

fn run_array_lit(ctx: &mut RunCtx, items: &Filter, json: &Json) -> RunResult {
    let items = run(ctx, items, json);

    let mut arr = Vec::with_capacity(items.len());

    for item in items {
        match item {
            Ok(item) => arr.push(item),
            other => return vec![other],
        }
    }

    vec![Ok(Json::Array(arr))]
}

fn run_object_lit(ctx: &mut RunCtx, items: &[(Filter, Filter)], json: &Json) -> RunResult {
    fn build_pairs(
        ctx: &mut RunCtx,
        key: &Filter,
        value: &Filter,
        json: &Json,
    ) -> Vec<Result<(String, Json), RunStopValue>> {
        let mut pairs = Vec::new();
        for key in run(ctx, key, json) {
            for value in run(ctx, value, json) {
                match (key.clone(), value) {
                    (Ok(Json::String(key)), Ok(value)) => {
                        pairs.push(Ok((key, value)));
                    }
                    (Ok(any), Ok(_)) => {
                        pairs.push(Err(str_error(format!(
                            "Cannot use {} as object key",
                            json_fmt_error(&any)
                        ))));
                        return pairs;
                    }
                    (Err(s), _) => {
                        pairs.push(Err(s));
                        return pairs;
                    }
                    (_, Err(s)) => {
                        pairs.push(Err(s));
                        return pairs;
                    }
                }
            }
        }

        pairs
    }

    fn cartesian_product(
        pair_options: &[Vec<Result<(String, Json), RunStopValue>>],
    ) -> (Vec<HashMap<String, Json>>, Option<RunStopValue>) {
        fn rec(
            pair_options: &[Vec<Result<(String, Json), RunStopValue>>],
            current: HashMap<String, Json>,
            result: &mut Vec<HashMap<String, Json>>,
        ) -> Option<RunStopValue> {
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

    let (products, error) = cartesian_product(&pair_options);

    let mut ret = products
        .into_iter()
        .map(|obj| Ok(Json::Object(obj)))
        .collect::<Vec<_>>();

    if let Some(error) = error {
        ret.push(Err(error));
    }

    ret
}

fn run_project(ctx: &mut RunCtx, term: &Filter, exp: &Filter, json: &Json) -> RunResult {
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

    let mut ret = Vec::new();
    for term in run(ctx, term, json) {
        let term = match term {
            Ok(term) => term,
            stop => {
                ret.push(stop);
                return ret;
            }
        };

        for exp in run(ctx, exp, json) {
            let exp = match exp {
                Ok(exp) => exp,
                stop => {
                    ret.push(stop);
                    return ret;
                }
            };

            ret.push(project(&term, &exp));
        }
    }

    ret
}

fn run_slice(
    ctx: &mut RunCtx,
    term: &Filter,
    left: Option<&Filter>,
    right: Option<&Filter>,
    json: &Json,
) -> RunResult {
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

    let mut ret = Vec::new();

    for term in run(ctx, term, json) {
        let term = match term {
            Ok(term) => term,
            stop => {
                ret.push(stop.clone());
                return ret;
            }
        };

        let lefts = match left {
            Some(left) => run(ctx, left, json),
            None => vec![Ok(Json::Number(Number::Int(rug::Integer::ZERO)))],
        };

        for left in lefts {
            let left = match left {
                Ok(left) => left,
                stop => {
                    ret.push(stop.clone());
                    return ret;
                }
            };

            let rights = match right {
                Some(right) => run(ctx, right, json),
                None => vec![Ok(Json::Number(Number::Int(match &term {
                    Json::Array(arr) => arr.len().into(),
                    Json::String(s) => s.len().into(),
                    _ => rug::Integer::ZERO,
                })))],
            };

            for right in rights {
                let right = match right {
                    Ok(right) => right,
                    stop => {
                        ret.push(stop.clone());
                        return ret;
                    }
                };

                ret.push(slice(&term, &left, &right));
            }
        }
    }

    ret
}

fn run_iter(json: &Json) -> RunResult {
    match json {
        Json::Array(arr) => arr.iter().map(|json| Ok(json.clone())).collect(),
        Json::Object(obj) => obj.values().map(|json| Ok(json.clone())).collect(),
        any => vec![Err(str_error(format!(
            "Cannot iterate over {}",
            json_fmt_error(any)
        )))],
    }
}

fn run_pipe(ctx: &mut RunCtx, left: &Filter, right: &Filter, json: &Json) -> RunResult {
    let mut ret = Vec::new();

    for left_json in run(ctx, left, json) {
        match left_json {
            Ok(left_json) => {
                ret.extend(run(ctx, right, &left_json));
            }
            stop => {
                ret.push(stop);
                break;
            }
        }
    }

    ret
}

fn run_alt(ctx: &mut RunCtx, left: &Filter, right: &Filter, json: &Json) -> RunResult {
    let mut valid_left = Vec::new();

    for json in run(ctx, left, json) {
        match json {
            Ok(json) => {
                if json.to_bool() {
                    valid_left.push(Ok(json));
                }
            }
            stop => {
                valid_left.push(stop);
                break;
            }
        }
    }

    if valid_left.is_empty() {
        run(ctx, right, json)
    } else {
        valid_left
    }
}

fn run_try_catch(ctx: &mut RunCtx, try_: &Filter, catch_: &Filter, json: &Json) -> RunResult {
    let mut ret = Vec::new();

    for json in run(ctx, try_, json) {
        match json {
            Ok(json) => {
                ret.push(Ok(json));
            }
            Err(RunStopValue::Error(err)) => {
                for json in run(ctx, catch_, &err) {
                    match json {
                        Ok(json) => {
                            ret.push(Ok(json));
                        }
                        stop => {
                            ret.push(stop);
                            return ret;
                        }
                    }
                }
            }
            stop => {
                ret.push(stop);
                return ret;
            }
        }
    }

    ret
}

fn run_comma(ctx: &mut RunCtx, left: &Filter, right: &Filter, json: &Json) -> RunResult {
    let mut ret = Vec::new();

    for val in run(ctx, left, json) {
        match val {
            Ok(val) => {
                ret.push(Ok(val));
            }
            stop => {
                ret.push(stop);
                return ret;
            }
        }
    }

    for val in run(ctx, right, json) {
        match val {
            Ok(val) => {
                ret.push(Ok(val));
            }
            stop => {
                ret.push(stop);
                return ret;
            }
        }
    }

    ret
}

fn run_if_else(
    ctx: &mut RunCtx,
    cond: &Filter,
    then: &Filter,
    else_: &Filter,
    json: &Json,
) -> RunResult {
    let mut ret = Vec::new();

    for cond in run(ctx, cond, json) {
        match cond {
            Ok(cond) => {
                let branch = if cond.to_bool() { then } else { else_ };
                for branch in run(ctx, branch, json) {
                    match branch {
                        Ok(branch) => {
                            ret.push(Ok(branch));
                        }
                        stop => {
                            ret.push(stop);
                            break;
                        }
                    }
                }
            }
            stop => {
                ret.push(stop);
                break;
            }
        };
    }

    ret
}

fn run_reduce(
    ctx: &mut RunCtx,
    exp: &Filter,
    name: &str,
    init: &Filter,
    update: &Filter,
    json: &Json,
) -> RunResult {
    fn reset_var(ctx: &mut RunCtx, name: &str, var: Option<Json>) {
        if let Some(var) = var {
            ctx.state.vars.insert(name.to_string(), var);
        }
    }

    // exp_input is json just the first time, then it is null
    let mut exp_input = Some(json);

    let mut ret = Vec::new();

    for base in run(ctx, init, json) {
        let base = match base {
            Ok(base) => base,
            stop => {
                ret.push(stop);
                return ret;
            }
        };

        let stream = run(ctx, exp, exp_input.take().unwrap_or(&Json::Null));

        let og_var = ctx.state.vars.remove(name);

        let mut acc = base;
        for val in stream {
            let val = match val {
                Ok(val) => val,
                stop => {
                    ret.push(stop);
                    reset_var(ctx, name, og_var);
                    return ret;
                }
            };

            ctx.state.vars.insert(name.to_string(), val.clone());

            let updates = run(ctx, update, &acc);
            acc = match updates.into_iter().try_fold(None, |_, item| item.map(Some)) {
                Ok(Some(last_update)) => last_update,
                Ok(None) => Json::Null,
                Err(stop) => {
                    ret.push(Err(stop));
                    reset_var(ctx, name, og_var);
                    return ret;
                }
            };
        }

        reset_var(ctx, name, og_var);
    }

    ret
}

fn run_foreach(
    ctx: &mut RunCtx,
    exp: &Filter,
    name: &str,
    init: &Filter,
    update: &Filter,
    extract: &Filter,
    json: &Json,
) -> RunResult {
    fn reset_var(ctx: &mut RunCtx, name: &str, var: Option<Json>) {
        if let Some(var) = var {
            ctx.state.vars.insert(name.to_string(), var);
        }
    }

    // exp_input is json just the first time, then it is null
    let mut exp_input = Some(json);

    let mut ret = Vec::new();

    for base in run(ctx, init, json) {
        let base = match base {
            Ok(base) => base,
            stop => {
                ret.push(stop);
                return ret;
            }
        };

        let stream = run(ctx, exp, exp_input.take().unwrap_or(&Json::Null));

        let og_var = ctx.state.vars.remove(name);

        let mut acc = base;
        for val in stream {
            let val = match val {
                Ok(val) => val,
                stop => {
                    ret.push(stop);
                    reset_var(ctx, name, og_var);
                    return ret;
                }
            };

            ctx.state.vars.insert(name.to_string(), val.clone());

            let updates = run(ctx, update, &acc);
            if updates.is_empty() {
                acc = Json::Null;
            }
            for update in updates {
                let update = match update {
                    Ok(update) => update,
                    stop => {
                        ret.push(stop);
                        reset_var(ctx, name, og_var);
                        return ret;
                    }
                };

                for extract in run(ctx, extract, &update) {
                    match extract {
                        val @ Ok(_) => ret.push(val),
                        stop => {
                            ret.push(stop);
                            reset_var(ctx, name, og_var);
                            return ret;
                        }
                    }
                }

                acc = update;
            }
        }

        reset_var(ctx, name, og_var);
    }

    ret
}

fn run_func_def(
    ctx: &mut RunCtx,
    name: &str,
    params: &[FuncParam],
    body: &Filter,
    next: &Filter,
    json: &Json,
) -> RunResult {
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

    let prev_func = ctx.state.funcs.remove(&(name.to_string(), params.len()));

    {
        let func_state = Rc::new(RefCell::new(ctx.state.clone()));
        func_state.borrow_mut().current_func = Some((
            name.to_string(),
            params.len(),
            CurrentFuncDef {
                state: func_state.clone(),
                params: param_names.clone(),
                body: body.clone(),
            },
        ));

        ctx.state.funcs.insert(
            (name.to_string(), params.len()),
            FuncDef {
                state: func_state.borrow().clone(),
                params: param_names,
                body,
            },
        );

        // drop func_ctx
    }

    let ret = run(ctx, next, json);

    // Restore scope only on the top level of modules
    let restore_scope = ctx.file == RunFile::Main || ctx.is_top_level();
    if restore_scope {
        if let Some(prev_func) = prev_func {
            ctx.state
                .funcs
                .insert((name.to_string(), params.len()), prev_func);
        } else {
            ctx.state.funcs.remove(&(name.to_string(), params.len()));
        }
    }

    ret
}

fn run_func_call(ctx: &mut RunCtx, name: &str, args: &[Filter], json: &Json) -> RunResult {
    fn func_call(
        ctx: &mut RunCtx,
        mut func: FuncDef,
        args: &[Filter],
        json: &Json,
    ) -> RunResult {
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

        let ret = run(ctx, &func.body, json);

        std::mem::swap(&mut ctx.state, &mut func.state);

        ret
    }

    let argc = args.len();

    if let Some((curr_name, curr_argc, curr_func)) = &ctx.state.current_func {
        if curr_name == name && *curr_argc == argc {
            let func_def = FuncDef {
                state: curr_func.state.borrow().clone(),
                params: curr_func.params.clone(),
                body: curr_func.body.clone(),
            };
            return func_call(ctx, func_def, args, json);
        }
    }

    if let Some(func) = ctx.state.funcs.get(&(name.to_string(), argc)) {
        return func_call(ctx, func.clone(), args, json);
    }

    if let Some(func) = ctx.jq_builtins.get(&(name, argc)) {
        return func_call(ctx, func.clone(), args, json);
    }

    if let Some(func) = ctx.rs_builtins.get(&(name, argc)) {
        return (func)(ctx, args, json);
    }

    vec![Err(str_error(format!("{name}/{argc} is not defined")))]
}

fn run_label(ctx: &mut RunCtx, label: &str, then: &Filter, json: &Json) -> RunResult {
    let inserted = ctx.state.labels.insert(label.to_string());
    let breaked_then = run(ctx, then, json);
    if inserted {
        ctx.state.labels.remove(label);
    }

    breaked_then
        .into_iter()
        .filter(|ret| match ret {
            Err(RunStopValue::Break(break_label)) => label != break_label,
            _ => true,
        })
        .collect()
}

fn run_break(ctx: &mut RunCtx, label: &str) -> RunResult {
    if ctx.state.labels.contains(label) {
        vec![Err(RunStopValue::Break(label.to_string()))]
    } else {
        vec![Err(str_error(format!("$*label-{label} is not defined")))]
    }
}

fn run_loc(file: &str, line: usize) -> RunResult {
    vec![Ok(Json::Object(HashMap::from([
        ("file".to_string(), Json::String(file.to_string())),
        ("line".to_string(), Json::Number(Number::Int(line.into()))),
    ])))]
}

// ------------------- Error Utils --------------------- //

pub(crate) fn str_error(s: String) -> RunStopValue {
    RunStopValue::Error(Json::String(s))
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
            {"a": [1,2], "b": [3,4]}
        "#;
        let filter = r#"
            .a + .b
        "#;

        let input: Json = input.parse().expect("json input parse error");
        println!("{input:?}");

        let filter: Filter = filter.parse().expect("filter parse error");
        println!("{filter:?}");

        filter
            .run(&mut RunCtx::new(), &input)
            .into_iter()
            .map(|r| r.map(|json| json.format_compact()))
            .for_each(|r| match r {
                Ok(json) => println!("{json}"),
                Err(RunStopValue::Error(err)) => println!("error: {err}"),
                Err(s) => println!("{s:?}"),
            });
    }
}
