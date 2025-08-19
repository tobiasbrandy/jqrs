mod builtins;

use std::{
    cell::RefCell,
    sync::{Arc, Mutex},
};

use either::Either;

use crate::{json::Json, math::Number};

use super::Filter;

// ------------------- API types --------------------- //

#[derive(Debug, Clone, PartialEq)]
pub enum RunEndValue {
    Error(Arc<Json>),
    Break(Arc<str>),
    Halt(Arc<str>),
}
pub type RunEnd = Option<RunEndValue>;

pub type RunValue = Result<Arc<Json>, RunEndValue>;

pub type FuncId = (Arc<str>, usize);

// ------------------- Generator API & Polyfills --------------------- //

type RunOut = genawaiter::rc::Co<Arc<Json>>;

/// Generator with iterator semantics and an end value
pub struct RunGen<F: Future<Output = RunEnd>> {
    run_gen: genawaiter::rc::Gen<Arc<Json>, (), F>,
    end: RunEnd,
}
impl<F: Future<Output = RunEnd>> RunGen<F> {
    fn new(producer: impl FnOnce(RunOut) -> F) -> Self {
        Self {
            run_gen: genawaiter::rc::Gen::new(producer),
            end: None,
        }
    }

    pub fn resume(&mut self) -> genawaiter::GeneratorState<Arc<Json>, RunEnd> {
        self.run_gen.resume()
    }

    pub fn end(&mut self) -> RunEnd {
        self.end.take()
    }
}
impl<F: Future<Output = RunEnd>> genawaiter::Coroutine for RunGen<F> {
    type Yield = Arc<Json>;
    type Resume = ();
    type Return = F::Output;

    fn resume_with(
        mut self: std::pin::Pin<&mut Self>,
        arg: Self::Resume,
    ) -> genawaiter::GeneratorState<Self::Yield, Self::Return> {
        self.run_gen.resume_with(arg)
    }
}
impl<F: Future<Output = RunEnd>> Iterator for RunGen<F> {
    type Item = Arc<Json>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.resume() {
            genawaiter::GeneratorState::Yielded(val) => Some(val),
            genawaiter::GeneratorState::Complete(end) => {
                self.end = end;
                None
            }
        }
    }
}

macro_rules! yield_ {
    ($out:ident, $val:expr) => {
        $out.yield_($val).await
    };
}
use yield_;

// ------------------- Internal types - Context, FuncDef & Scope --------------------- //

#[derive(Debug, Clone)]
struct FuncDef {
    scope: Arc<Mutex<Scope>>,
    params: Arc<[Arc<str>]>,
    body: Arc<Filter>,
}

#[derive(Debug, Clone, Default)]
struct Scope {
    vars: im::HashMap<Arc<str>, Arc<Json>>,
    funcs: im::HashMap<FuncId, Arc<FuncDef>>,
    labels: im::HashSet<Arc<str>>,
    is_top_level: bool,
}

#[derive(Debug, Clone, PartialEq, Default)]
enum RunFile {
    #[default]
    Main,
    Module(String),
}

#[derive(Debug, Clone)]
pub struct RunCtx {
    file: RunFile,
    scope: RefCell<Arc<Mutex<Scope>>>,
}
impl Default for RunCtx {
    fn default() -> Self {
        Self {
            file: RunFile::Main,
            scope: RefCell::new(Arc::new(Mutex::new(Scope {
                vars: im::HashMap::new(),
                funcs: builtins::JQ_BUILTINS.clone(),
                labels: im::HashSet::new(),
                is_top_level: true,
            }))),
        }
    }
}
impl RunCtx {
    pub fn new(vars: im::HashMap<Arc<str>, Arc<Json>>) -> Self {
        Self {
            file: RunFile::Main,
            scope: RefCell::new(Arc::new(Mutex::new(Scope {
                vars,
                funcs: builtins::JQ_BUILTINS.clone(),
                labels: im::HashSet::new(),
                is_top_level: true,
            }))),
        }
    }

    fn start_new_scope(&self) -> Arc<Mutex<Scope>> {
        self.scope
            .replace_with(|og_frame| Arc::new(Mutex::new(og_frame.lock().unwrap().clone())))
    }

    fn restore_scope(&self, frame: Arc<Mutex<Scope>>) -> Arc<Mutex<Scope>> {
        self.scope.replace(frame)
    }

    fn is_top_level(&self) -> bool {
        self.scope.borrow().lock().unwrap().is_top_level
    }

    fn get_var(&self, name: &str) -> Option<Arc<Json>> {
        self.scope.borrow().lock().unwrap().vars.get(name).cloned()
    }

    fn insert_var(&self, name: Arc<str>, val: Arc<Json>) -> Option<Arc<Json>> {
        self.scope.borrow().lock().unwrap().vars.insert(name, val)
    }

    fn get_func(&self, name: Arc<str>, argc: usize) -> Option<Arc<FuncDef>> {
        self.scope
            .borrow()
            .lock()
            .unwrap()
            .funcs
            .get(&(name, argc))
            .cloned()
    }

    fn insert_func(&self, name: Arc<str>, argc: usize, func: Arc<FuncDef>) -> Option<Arc<FuncDef>> {
        self.scope
            .borrow()
            .lock()
            .unwrap()
            .funcs
            .insert((name, argc), func)
    }

    fn has_label(&self, label: &str) -> bool {
        self.scope.borrow().lock().unwrap().labels.contains(label)
    }

    fn insert_label(&self, label: Arc<str>) -> bool {
        self.scope
            .borrow()
            .lock()
            .unwrap()
            .labels
            .insert(label)
            .is_some()
    }
}

// ------------------- Implementation --------------------- //

pub(crate) fn run(
    ctx: &RunCtx,
    filter: &Arc<Filter>,
    json: &Arc<Json>,
) -> RunGen<impl Future<Output = RunEnd>> {
    async fn inner_run(
        out: RunOut,
        ctx: &RunCtx,
        filter: &Arc<Filter>,
        json: &Arc<Json>,
    ) -> RunEnd {
        match filter.as_ref() {
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
                run_slice(out, ctx, term, left.as_ref(), right.as_ref(), json).await
            }
            Filter::Iter => run_iter(out, json).await,
            Filter::Pipe(left, right) => run_pipe(out, ctx, left, right, json).await,
            Filter::Alt(left, right) => run_alt(out, ctx, left, right, json).await,
            Filter::TryCatch(try_, catch_) => run_try_catch(out, ctx, try_, catch_, json).await,
            Filter::Comma(left, right) => run_comma(out, ctx, left, right, json).await,
            Filter::IfElse(cond, then, else_) => {
                run_if_else(out, ctx, cond, then, else_, json).await
            }
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

    RunGen::new(|co| inner_run(co, ctx, filter, json))
}

async fn run_var(out: RunOut, ctx: &RunCtx, name: &Arc<str>) -> RunEnd {
    match ctx.get_var(name) {
        Some(val) => yield_!(out, val.clone()),
        None => return Some(str_error(format!("${name} is not defined"))),
    }
    None
}

async fn run_var_def(
    out: RunOut,
    ctx: &RunCtx,
    name: &Arc<str>,
    body: &Arc<Filter>,
    next: &Arc<Filter>,
    json: &Arc<Json>,
) -> RunEnd {
    let mut bodies = run(ctx, body, json);
    for body in &mut bodies {
        let og_scope = ctx.start_new_scope();
        ctx.insert_var(name.clone(), body.clone());

        let mut nexts = run(ctx, next, json);
        for next in &mut nexts {
            let new_scope = ctx.restore_scope(og_scope.clone());
            yield_!(out, next);
            ctx.restore_scope(new_scope);
        }

        ctx.restore_scope(og_scope);

        if let Some(end) = nexts.end() {
            return Some(end);
        }
    }
    bodies.end()
}

async fn run_array_lit(out: RunOut, ctx: &RunCtx, items: &Arc<Filter>, json: &Arc<Json>) -> RunEnd {
    let mut items_gen = run(ctx, items, json);
    let items = items_gen.by_ref().collect::<im::Vector<_>>();
    if let Some(end) = items_gen.end() {
        return Some(end);
    }

    yield_!(out, Json::Array(items).into());
    None
}

async fn run_object_lit(
    out: RunOut,
    ctx: &RunCtx,
    items: &[(Arc<Filter>, Arc<Filter>)],
    json: &Arc<Json>,
) -> RunEnd {
    #[allow(clippy::type_complexity)]
    fn build_pairs(
        ctx: &RunCtx,
        key: &Arc<Filter>,
        value: &Arc<Filter>,
        json: &Arc<Json>,
    ) -> Vec<Result<(Arc<str>, Arc<Json>), RunEndValue>> {
        let mut pairs = Vec::new();
        let mut keys = run(ctx, key, json);
        for key in &mut keys {
            let mut values = run(ctx, value, json);
            for value in &mut values {
                if let Json::String(key) = key.as_ref() {
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

    #[allow(clippy::type_complexity)]
    fn cartesian_product(
        pair_options: &[Vec<Result<(Arc<str>, Arc<Json>), RunEndValue>>],
    ) -> (Vec<im::HashMap<Arc<str>, Arc<Json>>>, RunEnd) {
        fn rec(
            pair_options: &[Vec<Result<(Arc<str>, Arc<Json>), RunEndValue>>],
            current: im::HashMap<Arc<str>, Arc<Json>>,
            result: &mut Vec<im::HashMap<Arc<str>, Arc<Json>>>,
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
        let err = rec(pair_options, im::HashMap::new(), &mut result);
        (result, err)
    }

    let pair_options = items
        .iter()
        .map(|(key, value)| build_pairs(ctx, key, value, json))
        .collect::<Vec<_>>();

    let (products, end) = cartesian_product(&pair_options);

    for obj in products {
        yield_!(out, Json::Object(obj).into());
    }

    end
}

async fn run_project(
    out: RunOut,
    ctx: &RunCtx,
    term: &Arc<Filter>,
    exp: &Arc<Filter>,
    json: &Arc<Json>,
) -> RunEnd {
    fn project(term: &Json, exp: &Json) -> RunValue {
        match (term, exp) {
            (Json::Object(obj), Json::String(key)) => {
                Ok(obj.get(key).cloned().unwrap_or(Json::arc_null()))
            }
            (Json::Array(arr), Json::Number(Number::Int(n))) => Ok(if n.is_negative() {
                (n.clone() + arr.len()).to_usize()
            } else {
                n.to_usize()
            }
            .and_then(|idx| arr.get(idx))
            .cloned()
            .unwrap_or(Json::arc_null())),
            (Json::Array(_), Json::Number(Number::Decimal(_))) => Ok(Json::arc_null()),
            (Json::Array(haystack), Json::Array(needle)) => {
                let needle = needle.iter().collect::<Vec<&_>>();
                Ok(Json::Array(
                    haystack
                        .iter()
                        .collect::<Vec<&_>>()
                        .windows(needle.len())
                        .enumerate()
                        .filter_map(|(i, window)| if window == needle { Some(i) } else { None })
                        .map(|i| Json::Number(Number::Int(i.into())).into())
                        .collect(),
                )
                .into())
            }
            (Json::Null, Json::Object(_)) => Ok(Json::arc_null()),
            (Json::Null, Json::String(_)) => Ok(Json::arc_null()),
            (Json::Null, Json::Number(_)) => Ok(Json::arc_null()),
            (term, key) => Err(str_error(format!(
                "Cannot index {} with {} {}",
                json_fmt_type(term),
                json_fmt_type(key),
                json_fmt_bounded(key),
            ))),
        }
    }

    let mut terms = run(ctx, term, json);

    for term in &mut terms {
        let mut exps = run(ctx, exp, json);

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
    out: RunOut,
    ctx: &RunCtx,
    term: &Arc<Filter>,
    left: Option<&Arc<Filter>>,
    right: Option<&Arc<Filter>>,
    json: &Arc<Json>,
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
                    None => return Ok(Json::arc_null()),
                };
                let r = match num_to_idx(r, len, rug::float::Round::Up) {
                    Some(r) => r,
                    None => return Ok(Json::arc_null()),
                };

                if r < l {
                    return Ok(Json::arc_empty_array());
                }

                Ok(Json::Array(arr.clone().slice(l..r)).into())
            }
            (Json::String(s), Json::Number(l), Json::Number(r)) => {
                let len = s.len();
                let l = match num_to_idx(l, len, rug::float::Round::Down) {
                    Some(l) => l,
                    None => return Ok(Json::arc_null()),
                };
                let r = match num_to_idx(r, len, rug::float::Round::Up) {
                    Some(r) => r,
                    None => return Ok(Json::arc_null()),
                };

                Ok(Json::String(s[l..r].into()).into())
            }
            (Json::Null, Json::Number(_), Json::Number(_)) => Ok(Json::arc_null()),
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

    let mut terms = run(ctx, term, json);
    for term in &mut terms {
        let mut lefts = match left {
            Some(left) => Either::Left(run(ctx, left, json)),
            None => Either::Right(std::iter::once(
                Json::Number(Number::Int(rug::Integer::ZERO)).into(),
            )),
        };

        for left in &mut lefts {
            let mut rights = match right {
                Some(right) => Either::Left(run(ctx, right, json)),
                None => Either::Right(std::iter::once(
                    Json::Number(Number::Int(match term.as_ref() {
                        Json::Array(arr) => arr.len().into(),
                        Json::String(s) => s.len().into(),
                        _ => rug::Integer::ZERO,
                    }))
                    .into(),
                )),
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

async fn run_iter(out: RunOut, json: &Json) -> RunEnd {
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
            )));
        }
    }

    None
}

async fn run_pipe(out: RunOut, ctx: &RunCtx, left: &Arc<Filter>, right: &Arc<Filter>, json: &Arc<Json>) -> RunEnd {
    let mut left_jsons = run(ctx, left, json);

    for left_json in &mut left_jsons {
        let mut right_jsons = run(ctx, right, &left_json);

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

async fn run_alt(out: RunOut, ctx: &RunCtx, left: &Arc<Filter>, right: &Arc<Filter>, json: &Arc<Json>) -> RunEnd {
    let mut lefts = run(ctx, left, json);

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
        let mut rights = run(ctx, right, json);
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
    out: RunOut,
    ctx: &RunCtx,
    try_: &Arc<Filter>,
    catch_: &Arc<Filter>,
    json: &Arc<Json>,
) -> RunEnd {
    let mut trys = run(ctx, try_, json);

    for json in &mut trys {
        yield_!(out, json);
    }
    match trys.end() {
        Some(RunEndValue::Error(err)) => {
            let mut catches = run(ctx, catch_, &err);

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
    out: RunOut,
    ctx: &RunCtx,
    left: &Arc<Filter>,
    right: &Arc<Filter>,
    json: &Arc<Json>,
) -> RunEnd {
    let mut left_jsons = run(ctx, left, json);

    for left_json in &mut left_jsons {
        yield_!(out, left_json);
    }
    if let Some(end) = left_jsons.end() {
        return Some(end);
    }

    let mut right_jsons = run(ctx, right, json);

    for right_json in &mut right_jsons {
        yield_!(out, right_json);
    }
    if let Some(end) = right_jsons.end() {
        return Some(end);
    }

    None
}

async fn run_if_else(
    out: RunOut,
    ctx: &RunCtx,
    cond: &Arc<Filter>,
    then: &Arc<Filter>,
    else_: &Arc<Filter>,
    json: &Arc<Json>,
) -> RunEnd {
    let mut conds = run(ctx, cond, json);

    for cond in &mut conds {
        let filter_branch = if cond.to_bool() { then } else { else_ };
        let mut branches = run(ctx, filter_branch, json);

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
    out: RunOut,
    ctx: &RunCtx,
    exp: &Arc<Filter>,
    name: &Arc<str>,
    init: &Arc<Filter>,
    update: &Arc<Filter>,
    json: &Arc<Json>,
) -> RunEnd {
    // exp_input is json just the first time, then it is null
    let mut exp_input = Some(json);

    let mut bases = run(ctx, init, json);
    for base in &mut bases {
        let mut acc = base;

        let default_exp_input = Json::arc_null();
        let mut stream = run(ctx, exp, exp_input.take().unwrap_or(&default_exp_input));
        for val in &mut stream {
            acc = {
                let og_scope = ctx.start_new_scope();
                ctx.insert_var(name.clone(), val);

                let mut updates = run(ctx, update, &acc);
                let new_acc = updates.by_ref().fold(Json::arc_null(), |_, j| j);

                ctx.restore_scope(og_scope);

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
    bases.end()
}

#[allow(clippy::too_many_arguments)]
async fn run_foreach(
    out: RunOut,
    ctx: &RunCtx,
    exp: &Arc<Filter>,
    name: &Arc<str>,
    init: &Arc<Filter>,
    update: &Arc<Filter>,
    extract: &Arc<Filter>,
    json: &Arc<Json>,
) -> RunEnd {
    // exp_input is json just the first time, then it is null
    let mut exp_input = Some(json);

    let mut bases = run(ctx, init, json);
    for base in &mut bases {
        let mut acc = base;

        let default_exp_input = Json::arc_null();
        let mut stream = run(ctx, exp, exp_input.take().unwrap_or(&default_exp_input));
        for val in &mut stream {
            acc = {
                let og_scope = ctx.start_new_scope();
                ctx.insert_var(name.clone(), val.clone());

                let mut new_acc = Json::arc_null();

                let mut updates = run(ctx, update, &acc);
                for update in &mut updates {
                    new_acc = update;

                    let mut extracts = run(ctx, extract, &new_acc);
                    for extract in &mut extracts {
                        let new_scope = ctx.restore_scope(og_scope.clone());
                        yield_!(out, extract);
                        ctx.restore_scope(new_scope);
                    }
                    if let Some(end) = extracts.end() {
                        ctx.restore_scope(og_scope);
                        return Some(end);
                    }
                }

                ctx.restore_scope(og_scope);

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
    bases.end()
}

async fn run_func_def(
    out: RunOut,
    ctx: &RunCtx,
    name: &Arc<str>,
    params: &Arc<[Arc<str>]>,
    body: &Arc<Filter>,
    next: &Arc<Filter>,
    json: &Arc<Json>,
) -> RunEnd {
    let og_scope = ctx.start_new_scope();
    ctx.insert_func(
        name.clone(),
        params.len(),
        Arc::new(FuncDef {
            scope: ctx.scope.borrow().clone(),
            params: params.clone(),
            body: body.clone(),
        }),
    );

    let mut nexts = run(ctx, next, json);
    for next in &mut nexts {
        let new_scope = ctx.restore_scope(og_scope.clone());
        yield_!(out, next);
        ctx.restore_scope(new_scope);
    }
    let ret = nexts.end();

    // Restore scope only on the top level of modules
    if ctx.file == RunFile::Main || !ctx.is_top_level() {
        ctx.restore_scope(og_scope);
    }

    ret
}

async fn run_func_call(
    out: RunOut,
    ctx: &RunCtx,
    name: &Arc<str>,
    args: &[Arc<Filter>],
    json: &Arc<Json>,
) -> RunEnd {
    static EMPTY_PARAMS: std::sync::LazyLock<Arc<[Arc<str>]>> = std::sync::LazyLock::new(|| Arc::new([]));

    let argc = args.len();

    if let Some(func) = ctx.get_func(name.clone(), argc) {
        // User defined or builtin function call
        let og_scope = ctx.restore_scope(func.scope.clone());
        ctx.start_new_scope();
        ctx.scope.borrow().lock().unwrap().is_top_level = false;

        for (param, arg) in std::iter::zip(func.params.iter(), args) {
            ctx.insert_func(
                param.clone(),
                0,
                Arc::new(FuncDef {
                    scope: og_scope.clone(),
                    params: EMPTY_PARAMS.clone(),
                    body: arg.clone(),
                }),
            );
        }

        let ret = {
            let mut results = run(ctx, &func.body, json);
            for result in &mut results {
                let new_scope = ctx.restore_scope(og_scope.clone());
                yield_!(out, result);
                ctx.restore_scope(new_scope);
            }
            results.end()
        };

        ctx.restore_scope(og_scope);

        ret
    } else {
        // Rust builtin function call
        builtins::run_rs_builtin(name, argc, out, ctx, args, json).await
    }
}

async fn run_label(
    out: RunOut,
    ctx: &RunCtx,
    label: &Arc<str>,
    then: &Arc<Filter>,
    json: &Arc<Json>,
) -> RunEnd {
    let og_scope = ctx.start_new_scope();
    ctx.insert_label(label.clone());

    let mut results = run(ctx, then, json);
    for result in &mut results {
        let new_scope = ctx.restore_scope(og_scope.clone());
        yield_!(out, result);
        ctx.restore_scope(new_scope);
    }

    ctx.restore_scope(og_scope);

    if let Some(RunEndValue::Break(break_label)) = results.end()
        && break_label != *label
    {
        return Some(RunEndValue::Break(break_label));
    }

    None
}

async fn run_break(ctx: &RunCtx, label: &Arc<str>) -> RunEnd {
    if ctx.has_label(label) {
        Some(RunEndValue::Break(label.clone()))
    } else {
        Some(str_error(format!("$*label-{label} is not defined")))
    }
}

async fn run_loc(out: RunOut, file: &str, line: usize) -> RunEnd {
    yield_!(
        out,
        Json::Object(im::HashMap::from(&[
            ("file".into(), Json::String(file.into()).into()),
            ("line".into(), Json::Number(Number::Int(line.into())).into()),
        ][..])).into()
    );
    None
}

// ------------------- Error Utils --------------------- //

fn str_error(s: String) -> RunEndValue {
    RunEndValue::Error(Json::String(s.into()).into())
}

fn json_fmt_error(json: &Json) -> String {
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
            def square: . * .;
            def is_even: [true, false][. % 2];
            def square_evens: [.[] | select(is_even)] | map(. * .);
            square_evens
        "#;

        let input: Arc<_> = input.parse::<Json>().expect("json input parse error").into();
        println!("{input:?}");

        let filter: Arc<_> = filter.parse::<Filter>().expect("filter parse error").into();
        println!("{filter:?}");

        let ctx = RunCtx::default();
        let mut run_gen = filter.run(&ctx, &input);
        for json in &mut run_gen {
            print!("{json:?}");
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
