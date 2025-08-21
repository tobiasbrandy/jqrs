#![allow(clippy::too_many_arguments)]

use std::sync::{Arc, Weak};

use crate::filter::Filter;
use crate::json::Json;
use crate::math::Number;

#[derive(Debug, Clone, PartialEq)]
pub enum RunEndValue {
    Error(Arc<Json>),
    Break(Arc<str>),
    Halt { code: usize, err: Option<Arc<Json>> },
}

#[derive(Debug)]
pub struct FilterRunner {
    stack: Vec<Frame>,
}
impl FilterRunner {
    pub fn new(filter: Arc<Filter>, json: Arc<Json>) -> Self {
        Self {
            stack: vec![Frame {
                filter,
                scope: Arc::new(Scope::default()),
                state: FrameState::default(),
                dot: json,
                parent: 0,
                closed: false,
            }],
        }
    }
}
impl Iterator for FilterRunner {
    type Item = Result<Arc<Json>, RunEndValue>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stack.is_empty() {
            return None;
        }

        match run(self) {
            RunVal::Value(json) => Some(Ok(json)),
            RunVal::EndOk => None,
            RunVal::EndErr(err) => Some(Err(err)),
        }
    }
}

#[derive(Debug, Clone)]
struct FuncDef {
    scope: Weak<Scope>,
    params: Arc<[Arc<str>]>,
    body: Arc<Filter>,
}

#[derive(Debug, Clone, Default)]
struct Scope {
    vars: im::HashMap<Arc<str>, Arc<Json>>,
    funcs: im::HashMap<(Arc<str>, usize), FuncDef>,
    labels: im::HashSet<Arc<str>>,
}

#[derive(Debug)]
struct Frame {
    filter: Arc<Filter>,
    scope: Arc<Scope>,
    state: FrameState,
    dot: Arc<Json>,
    parent: usize,
    closed: bool,
}

#[derive(Debug, Clone, Default)]
enum RunVal {
    Value(Arc<Json>),
    #[default]
    EndOk,
    EndErr(RunEndValue),
}
impl From<Result<Arc<Json>, RunEndValue>> for RunVal {
    fn from(val: Result<Arc<Json>, RunEndValue>) -> Self {
        match val {
            Ok(json) => RunVal::Value(json),
            Err(end) => RunVal::EndErr(end),
        }
    }
}

#[derive(Debug, Clone)]
enum StepOut {
    Run(Arc<Filter>, Arc<Json>),
    RunScoped(Arc<Filter>, Arc<Json>, Arc<Scope>),
    Yield(Arc<Json>),
    Return(Arc<Json>),
    Continue,
    EndOk,
    EndErr(RunEndValue),
}
impl StepOut {
    fn run(filter: &Arc<Filter>, dot: &Arc<Json>) -> Self {
        Self::Run(filter.clone(), dot.clone())
    }

    fn run_scoped(filter: &Arc<Filter>, dot: &Arc<Json>, scope: Arc<Scope>) -> Self {
        Self::RunScoped(filter.clone(), dot.clone(), scope)
    }
}
impl From<RunVal> for StepOut {
    fn from(val: RunVal) -> Self {
        match val {
            RunVal::Value(json) => StepOut::Yield(json),
            RunVal::EndOk => StepOut::EndOk,
            RunVal::EndErr(err) => StepOut::EndErr(err),
        }
    }
}
impl From<Result<Arc<Json>, RunEndValue>> for StepOut {
    fn from(val: Result<Arc<Json>, RunEndValue>) -> Self {
        RunVal::from(val).into()
    }
}

#[derive(Debug, Default)]
enum FrameState {
    #[default]
    Start,
    VarDef(VarDefState),
    ArrayLit(ArrayLitState),
    ObjectLit(ObjectLitState),
    Project(ProjectState),
    Iter(IterState),
    Slice(SliceState),
    Pipe(PipeState),
    Alt(AltState),
    TryCatch(TryCatchState),
    Comma(CommaState),
    IfElse(IfElseState),
    FuncDef(FuncDefState),
    FuncCall(FuncCallState),
    Label(LabelState),
}

fn run(runner: &mut FilterRunner) -> RunVal {
    if runner.stack.is_empty() {
        return RunVal::EndOk;
    }

    let mut frame_idx = runner.stack.len() - 1;
    let mut yielded: Option<RunVal> = None;

    loop {
        let frame = &mut runner.stack[frame_idx];
        let parent = frame.parent;

        let step_output = if frame.closed {
            StepOut::EndOk
        } else {
            frame_run(frame, yielded.unwrap_or_default())
        };

        yielded = match step_output {
            StepOut::Run(filter, dot) => {
                let scope = frame.scope.clone();
                runner.stack.push(Frame {
                    filter,
                    scope,
                    state: FrameState::default(),
                    dot,
                    parent: frame_idx,
                    closed: false,
                });
                None
            }
            StepOut::RunScoped(filter, dot, scope) => {
                runner.stack.push(Frame {
                    filter,
                    scope,
                    state: FrameState::default(),
                    dot,
                    parent: frame_idx,
                    closed: false,
                });
                None
            }
            StepOut::Yield(json) => Some(RunVal::Value(json)),
            StepOut::Return(json) => {
                frame.closed = true;
                Some(RunVal::Value(json))
            }
            StepOut::Continue => None,
            StepOut::EndOk => {
                while runner.stack.len() > frame_idx {
                    runner.stack.pop();
                }
                Some(RunVal::EndOk)
            }
            StepOut::EndErr(err) => {
                while runner.stack.len() > frame_idx {
                    runner.stack.pop();
                }
                Some(RunVal::EndErr(err))
            }
        };

        if frame_idx == 0
            && let Some(run_val) = yielded
        {
            return run_val;
        }

        frame_idx = match yielded {
            Some(_) => parent,
            None => runner.stack.len() - 1,
        }
    }
}

#[rustfmt::skip]
fn frame_run(frame: &mut Frame, yielded: RunVal) -> StepOut {
    let state = &mut frame.state;
    let dot = &frame.dot;
    let scope = &frame.scope;

    macro_rules! state {
        ($variant:ident) => {{
            if matches!(state, FrameState::Start) {
                *state = FrameState::$variant(Default::default());
            }
            match state {
                FrameState::$variant(s) => s,
                _ => unreachable!(),
            }
        }};
    }

    match frame.filter.as_ref() {
        Filter::Identity => match state {
            FrameState::Start => StepOut::Return(dot.clone()),
            _ => unreachable!(),
        },
        Filter::Empty => {
            StepOut::EndOk
        }
        Filter::Json(json) => match state {
            FrameState::Start => StepOut::Return(json.clone()),
            _ => unreachable!(),
        },
        Filter::Var(name) => match state {
            FrameState::Start => match scope.vars.get(name) {
                Some(json) => StepOut::Return(json.clone()),
                None => StepOut::EndErr(str_error(format!("${name} is not defined"))),
            },
            _ => unreachable!(),
        }
        Filter::VarDef(name, body, next) => {
            run_var_def(state!(VarDef), scope, dot, yielded, name, body, next)
        }
        Filter::ArrayLit(items) => {
            run_array_lit(state!(ArrayLit), dot, yielded, items)
        }
        Filter::ObjectLit(items) => {
            run_object_lit(state!(ObjectLit), dot, yielded, items)
        }
        Filter::Project(term, exp) => {
            run_project(state!(Project), dot, yielded, term, exp)
        }
        Filter::Slice(term, left, right) => {
            run_slice(state!(Slice), dot, yielded, term, left, right)
        }
        Filter::Iter => {
            run_iter(state!(Iter), dot)
        }
        Filter::Pipe(left, right) => {
            run_pipe(state!(Pipe), dot, yielded, left, right)
        }
        Filter::Alt(left, right) => {
            run_alt(state!(Alt), dot, yielded, left, right)
        }
        Filter::TryCatch(try_, catch_) => {
            run_try_catch(state!(TryCatch), dot, yielded, try_, catch_)
        }
        Filter::Comma(left, right) => {
            run_comma(state!(Comma), dot, yielded, left, right)
        }
        Filter::IfElse(cond, then, else_) => {
            run_if_else(state!(IfElse), dot, yielded, cond, then, else_)
        }
        Filter::FuncDef(name, params, body, next) => {
            run_func_def(state!(FuncDef), scope, dot, yielded, name, params, body, next)
        }
        Filter::FuncCall(name, args) => {
            run_func_call(state!(FuncCall), scope, dot, yielded, name, args)
        }
        Filter::Label(label, then) => {
            run_label(state!(Label), scope, dot, yielded, label, then)
        }
        Filter::Break(label) => match state {
            FrameState::Start => if scope.labels.contains(label) {
                StepOut::EndErr(RunEndValue::Break(label.clone()))
            } else {
                StepOut::EndErr(str_error(format!("$*label-{label} is not defined")))
            }
            _ => unreachable!(),
        }
        Filter::Loc(file, line) => match state {
            FrameState::Start => StepOut::Return(Json::Object(im::hashmap! {
                "file".into() => Json::String(file.clone()).into(),
                "line".into() => Json::Number(Number::Int((*line).into())).into(),
            }).into()),
            _ => unreachable!(),
        }
        _ => todo!(),
    }
}

#[derive(Debug, Clone, Default)]
enum VarDefState {
    #[default]
    Start,
    Body,
    Next,
}
fn run_var_def(
    state: &mut VarDefState,
    scope: &Scope,
    dot: &Arc<Json>,
    yielded: RunVal,
    name: &Arc<str>,
    body: &Arc<Filter>,
    next: &Arc<Filter>,
) -> StepOut {
    match state {
        VarDefState::Start => {
            *state = VarDefState::Body;
            StepOut::run(body, dot)
        }
        VarDefState::Body => match yielded {
            RunVal::Value(body) => {
                let mut new_scope = scope.clone();
                new_scope.vars.insert(name.clone(), body);

                *state = VarDefState::Next;
                StepOut::run_scoped(next, dot, Arc::new(new_scope))
            }
            val => val.into(),
        },
        VarDefState::Next => yielded.into(),
    }
}

#[derive(Debug, Clone, Default)]
enum ArrayLitState {
    #[default]
    Start,
    Items(im::Vector<Arc<Json>>),
}
fn run_array_lit(
    state: &mut ArrayLitState,
    dot: &Arc<Json>,
    yielded: RunVal,
    items: &Arc<Filter>,
) -> StepOut {
    match state {
        ArrayLitState::Start => {
            *state = ArrayLitState::Items(im::Vector::new());
            StepOut::run(items, dot)
        }
        ArrayLitState::Items(items) => match yielded {
            RunVal::Value(item) => {
                items.push_back(item);
                StepOut::Continue
            }
            RunVal::EndOk => StepOut::Return(Json::Array(items.clone()).into()),
            val => val.into(),
        },
    }
}

#[derive(Debug, Clone)]
struct ObjectLitStateFrame {
    curr: im::HashMap<Arc<str>, Arc<Json>>,
    key_gen: Arc<Filter>,
    value_gen: Arc<Filter>,
    key: Option<Arc<str>>,
}
#[derive(Debug, Clone, Default)]
enum ObjectLitState {
    #[default]
    Start,
    Running {
        curr_idx: usize,
        frames: Vec<ObjectLitStateFrame>,
    },
}
fn run_object_lit(
    state: &mut ObjectLitState,
    dot: &Arc<Json>,
    yielded: RunVal,
    items: &[(Arc<Filter>, Arc<Filter>)],
) -> StepOut {
    match state {
        ObjectLitState::Start => {
            if items.is_empty() {
                return StepOut::EndOk;
            }

            // Create frames
            let curr_idx = 0;
            let mut frames = items
                .iter()
                .cloned()
                .map(|(key_gen, value_gen)| ObjectLitStateFrame {
                    curr: im::HashMap::new(),
                    key_gen,
                    value_gen,
                    key: None,
                })
                .collect::<Vec<_>>();

            // Start the first frame
            let next_frame = &mut frames[curr_idx];
            let step_out = StepOut::run(&next_frame.key_gen, dot);

            // Set the state
            *state = ObjectLitState::Running { curr_idx, frames };
            step_out
        }
        ObjectLitState::Running { curr_idx, frames } => {
            let frame = &mut frames[*curr_idx];

            match &frame.key {
                // Consuming keys
                None => match yielded {
                    RunVal::Value(key) => match key.as_ref() {
                        Json::String(key) => {
                            frame.key = Some(key.clone());
                            StepOut::run(&frame.value_gen, dot)
                        }
                        _ => StepOut::EndErr(str_error(format!(
                            "Cannot use {} as object key",
                            json_fmt_error(&key)
                        ))),
                    },
                    RunVal::EndOk => {
                        if *curr_idx == 0 {
                            // We finished
                            StepOut::EndOk
                        } else {
                            // We finished this frame, go back to the previous one
                            *curr_idx -= 1;
                            StepOut::Continue
                        }
                    }
                    val => val.into(),
                },
                // Consuming values
                Some(key) => match yielded {
                    RunVal::Value(value) => {
                        let new_curr = frame.curr.update(key.clone(), value);

                        if *curr_idx == frames.len() - 1 {
                            // Last frame, yield the object
                            StepOut::Yield(Arc::new(Json::Object(new_curr)))
                        } else {
                            // Pass value to the next frame
                            *curr_idx += 1;
                            let next_frame = &mut frames[*curr_idx];
                            next_frame.curr = new_curr;
                            StepOut::run(&next_frame.key_gen, dot)
                        }
                    }
                    RunVal::EndOk => {
                        // Finished consuming values, continue consuming keys
                        frame.key = None;
                        StepOut::Continue
                    }
                    val => val.into(),
                },
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
enum ProjectState {
    #[default]
    Start,
    Term,
    Exp {
        term: Arc<Json>,
    },
}
fn run_project(
    state: &mut ProjectState,
    dot: &Arc<Json>,
    yielded: RunVal,
    term: &Arc<Filter>,
    exp: &Arc<Filter>,
) -> StepOut {
    fn project(term: &Json, exp: &Json) -> Result<Arc<Json>, RunEndValue> {
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

    match state {
        ProjectState::Start => {
            *state = ProjectState::Term;
            StepOut::run(term, dot)
        }
        ProjectState::Term => match yielded {
            RunVal::Value(term) => {
                *state = ProjectState::Exp { term };
                StepOut::run(exp, dot)
            }
            val => val.into(),
        },
        ProjectState::Exp { term } => match yielded {
            RunVal::Value(exp) => project(term, &exp).into(),
            RunVal::EndOk => {
                *state = ProjectState::Term;
                StepOut::Continue
            }
            val => val.into(),
        },
    }
}

#[derive(Debug, Clone, Default)]
enum SliceState {
    #[default]
    Start,
    Term,
    Left {
        term: Arc<Json>,
    },
    Right {
        term: Arc<Json>,
        left: Arc<Json>,
    },
}
fn run_slice(
    state: &mut SliceState,
    dot: &Arc<Json>,
    yielded: RunVal,
    term: &Arc<Filter>,
    left: &Option<Arc<Filter>>,
    right: &Option<Arc<Filter>>,
) -> StepOut {
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

    fn slice(term: &Json, left: &Json, right: &Json) -> Result<Arc<Json>, RunEndValue> {
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

    match state {
        SliceState::Start => {
            *state = SliceState::Term;
            StepOut::run(term, dot)
        }
        SliceState::Term => match yielded {
            RunVal::Value(term) => match left {
                Some(left) => {
                    *state = SliceState::Left { term };
                    StepOut::run(left, dot)
                }
                None => {
                    *state = SliceState::Right {
                        term,
                        left: Json::arc_zero(),
                    };
                    StepOut::Continue
                }
            },
            val => val.into(),
        },
        SliceState::Left { term } => match yielded {
            RunVal::Value(left) => match right {
                Some(right) => {
                    *state = SliceState::Right {
                        term: term.clone(),
                        left,
                    };
                    StepOut::run(right, dot)
                }
                None => {
                    let right = Arc::new(Json::Number(Number::Int(match term.as_ref() {
                        Json::Array(arr) => arr.len().into(),
                        Json::String(s) => s.len().into(),
                        _ => rug::Integer::ZERO,
                    })));
                    slice(term, &left, &right).into()
                }
            },
            RunVal::EndOk => {
                *state = SliceState::Term;
                StepOut::Continue
            }
            val => val.into(),
        },
        SliceState::Right { term, left } => match yielded {
            RunVal::Value(right) => slice(term, left, &right).into(),
            RunVal::EndOk => {
                *state = SliceState::Left { term: term.clone() };
                StepOut::Continue
            }
            val => val.into(),
        },
    }
}

#[derive(Default)]
enum IterState {
    #[default]
    Start,
    IterArray(im::vector::ConsumingIter<Arc<Json>>),
    IterObject(im::hashmap::ConsumingIter<(Arc<str>, Arc<Json>)>),
}
impl std::fmt::Debug for IterState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IterState::Start => write!(f, "Start"),
            IterState::IterArray(_) => write!(f, "IterArray"),
            IterState::IterObject(_) => write!(f, "IterObject"),
        }
    }
}
fn run_iter(state: &mut IterState, dot: &Arc<Json>) -> StepOut {
    match state {
        IterState::Start => match dot.as_ref() {
            Json::Array(arr) => {
                *state = IterState::IterArray(arr.clone().into_iter());
                run_iter(state, dot)
            }
            Json::Object(obj) => {
                *state = IterState::IterObject(obj.clone().into_iter());
                run_iter(state, dot)
            }
            any => StepOut::EndErr(str_error(format!(
                "Cannot iterate over {}",
                json_fmt_error(any)
            ))),
        },
        IterState::IterArray(iter) => match iter.next() {
            Some(val) => StepOut::Yield(val),
            None => StepOut::EndOk,
        },
        IterState::IterObject(iter) => match iter.next() {
            Some((_, val)) => StepOut::Yield(val),
            None => StepOut::EndOk,
        },
    }
}

#[derive(Debug, Clone, Default)]
enum PipeState {
    #[default]
    Start,
    Left,
    Right,
}
fn run_pipe(
    state: &mut PipeState,
    dot: &Arc<Json>,
    yielded: RunVal,
    left: &Arc<Filter>,
    right: &Arc<Filter>,
) -> StepOut {
    match state {
        PipeState::Start => {
            *state = PipeState::Left;
            StepOut::run(left, dot)
        }
        PipeState::Left => match yielded {
            RunVal::Value(json) => {
                *state = PipeState::Right;
                StepOut::Run(right.clone(), json)
            }
            val => val.into(),
        },
        PipeState::Right => match yielded {
            RunVal::EndOk => {
                *state = PipeState::Left;
                StepOut::Continue
            }
            val => val.into(),
        },
    }
}

#[derive(Debug, Clone, Default)]
enum AltState {
    #[default]
    Start,
    Left {
        has_valid_left: bool,
    },
    Right,
}
fn run_alt(
    state: &mut AltState,
    dot: &Arc<Json>,
    yielded: RunVal,
    left: &Arc<Filter>,
    right: &Arc<Filter>,
) -> StepOut {
    match state {
        AltState::Start => {
            *state = AltState::Left {
                has_valid_left: false,
            };
            StepOut::run(left, dot)
        }
        AltState::Left { has_valid_left } => match yielded {
            RunVal::Value(json) => {
                if json.to_bool() {
                    *state = AltState::Left {
                        has_valid_left: true,
                    };
                    StepOut::Yield(json)
                } else {
                    StepOut::Continue
                }
            }
            RunVal::EndOk => {
                if !*has_valid_left {
                    *state = AltState::Right;
                    StepOut::run(right, dot)
                } else {
                    StepOut::EndOk
                }
            }
            val => val.into(),
        },
        AltState::Right => yielded.into(),
    }
}

#[derive(Debug, Clone, Default)]
enum TryCatchState {
    #[default]
    Start,
    Try,
    Catch,
}
fn run_try_catch(
    state: &mut TryCatchState,
    dot: &Arc<Json>,
    yielded: RunVal,
    try_: &Arc<Filter>,
    catch_: &Arc<Filter>,
) -> StepOut {
    match state {
        TryCatchState::Start => {
            *state = TryCatchState::Try;
            StepOut::run(try_, dot)
        }
        TryCatchState::Try => match yielded {
            RunVal::EndErr(RunEndValue::Error(err)) => {
                *state = TryCatchState::Catch;
                StepOut::Run(catch_.clone(), err)
            }
            val => val.into(),
        },
        TryCatchState::Catch => yielded.into(),
    }
}

#[derive(Debug, Clone, Default)]
enum CommaState {
    #[default]
    Start,
    Left,
    Right,
}
fn run_comma(
    state: &mut CommaState,
    dot: &Arc<Json>,
    yielded: RunVal,
    left: &Arc<Filter>,
    right: &Arc<Filter>,
) -> StepOut {
    match state {
        CommaState::Start => {
            *state = CommaState::Left;
            StepOut::run(left, dot)
        }
        CommaState::Left => match yielded {
            RunVal::EndOk => {
                *state = CommaState::Right;
                StepOut::run(right, dot)
            }
            val => val.into(),
        },
        CommaState::Right => yielded.into(),
    }
}

#[derive(Debug, Clone, Default)]
enum IfElseState {
    #[default]
    Start,
    Cond,
    Branch,
}
fn run_if_else(
    state: &mut IfElseState,
    dot: &Arc<Json>,
    yielded: RunVal,
    cond: &Arc<Filter>,
    then: &Arc<Filter>,
    else_: &Arc<Filter>,
) -> StepOut {
    match state {
        IfElseState::Start => {
            *state = IfElseState::Cond;
            StepOut::run(cond, dot)
        }
        IfElseState::Cond => match yielded {
            RunVal::Value(cond) => {
                *state = IfElseState::Branch;
                if cond.to_bool() {
                    StepOut::run(then, dot)
                } else {
                    StepOut::run(else_, dot)
                }
            }
            val => val.into(),
        },
        IfElseState::Branch => match yielded {
            RunVal::EndOk => {
                *state = IfElseState::Cond;
                StepOut::Continue
            }
            val => val.into(),
        },
    }
}

#[derive(Debug, Clone, Default)]
enum FuncDefState {
    #[default]
    Start,
    Next,
}
fn run_func_def(
    state: &mut FuncDefState,
    scope: &Scope,
    dot: &Arc<Json>,
    yielded: RunVal,
    name: &Arc<str>,
    params: &Arc<[Arc<str>]>,
    body: &Arc<Filter>,
    next: &Arc<Filter>,
) -> StepOut {
    match state {
        FuncDefState::Start => {
            let new_scope = Arc::new_cyclic(|weak| {
                let mut scope = scope.clone();
                scope.funcs.insert(
                    (name.clone(), params.len()),
                    FuncDef {
                        scope: weak.clone(),
                        params: params.clone(),
                        body: body.clone(),
                    },
                );
                scope
            });

            *state = FuncDefState::Next;
            StepOut::run_scoped(next, dot, new_scope)
        }
        FuncDefState::Next => yielded.into(),
    }
}

#[derive(Debug, Clone, Default)]
enum FuncCallState {
    #[default]
    Start,
    Body,
}
fn run_func_call(
    state: &mut FuncCallState,
    scope: &Arc<Scope>,
    dot: &Arc<Json>,
    yielded: RunVal,
    name: &Arc<str>,
    args: &[Arc<Filter>],
) -> StepOut {
    static EMPTY_PARAMS: std::sync::LazyLock<Arc<[Arc<str>]>> =
        std::sync::LazyLock::new(|| Arc::new([]));

    match state {
        FuncCallState::Start => {
            let Some(func) = scope.funcs.get(&(name.clone(), args.len())) else {
                // StepOut::Return(builtins::run_rs_builtin(name, argc, out, ctx, args, json))
                todo!()
            };

            let param_scope = Arc::downgrade(scope);

            let mut new_scope = func.scope.upgrade().unwrap().as_ref().clone();
            for (param, arg) in std::iter::zip(func.params.iter(), args) {
                new_scope.funcs.insert(
                    (param.clone(), 0),
                    FuncDef {
                        scope: param_scope.clone(),
                        params: EMPTY_PARAMS.clone(),
                        body: arg.clone(),
                    },
                );
            }

            *state = FuncCallState::Body;
            StepOut::run_scoped(&func.body, dot, Arc::new(new_scope))
        }
        FuncCallState::Body => yielded.into(),
    }
}

#[derive(Debug, Clone, Default)]
enum LabelState {
    #[default]
    Start,
    Next,
}
fn run_label(
    state: &mut LabelState,
    scope: &Scope,
    dot: &Arc<Json>,
    yielded: RunVal,
    label: &Arc<str>,
    then: &Arc<Filter>,
) -> StepOut {
    match state {
        LabelState::Start => {
            let mut new_scope = scope.clone();
            new_scope.labels.insert(label.clone());

            *state = LabelState::Next;
            StepOut::run_scoped(then, dot, Arc::new(new_scope))
        }
        LabelState::Next => match yielded {
            RunVal::EndErr(RunEndValue::Break(break_label)) if break_label == *label => {
                StepOut::EndOk
            }
            val => val.into(),
        },
    }
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
            [1,2,3]
        "#;
        let filter = r#"
            1 | label $out | 1, 2, 3, ., break $out, 5, 4, 6, 7
        "#;

        let input: Arc<_> = input
            .parse::<Json>()
            .expect("json input parse error")
            .into();
        println!("{input:?}");

        let filter: Arc<_> = filter.parse::<Filter>().expect("filter parse error").into();
        println!("{filter:?}");

        println!();

        for result in FilterRunner::new(filter, input) {
            match result {
                Ok(json) => print!("{json:?}"),
                Err(end) => match end {
                    RunEndValue::Error(err) => println!("error: {err}"),
                    RunEndValue::Break(br) => println!("break: {br}"),
                    RunEndValue::Halt { code, err } => println!("halt: {code} {err:?}"),
                },
            }
        }
    }
}
