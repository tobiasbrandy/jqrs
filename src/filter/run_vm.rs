use std::sync::Arc;

use crate::filter::Filter;
use crate::filter::run::RunEndValue;
use crate::json::Json;
use crate::math::Number;

#[derive(Debug)]
pub struct FilterRunner {
    stack: Vec<Frame>,
}
impl FilterRunner {
    pub fn new(filter: Arc<Filter>, json: Arc<Json>) -> Self {
        Self {
            stack: vec![Frame {
                filter,
                scope: Scope::default(),
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

#[allow(dead_code)]
#[derive(Debug, Clone, Default)]
struct Scope {
    // vars: im::HashMap<Arc<str>, Json>,
    // funcs: im::HashMap<(Arc<str>, usize), VmFuncDef>,
    // labels: im::HashSet<Arc<str>>,
}

#[derive(Debug)]
struct Frame {
    filter: Arc<Filter>,
    #[allow(dead_code)]
    scope: Scope,
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
    Yield(Arc<Json>),
    Return(Arc<Json>),
    Continue,
    EndOk,
    EndErr(RunEndValue),
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
    Project(ProjectState),
    Iter(IterState),
    Slice(SliceState),
    Pipe(PipeState),
    TryCatch(TryCatchState),
    Comma(CommaState),
}

fn run(runner: &mut FilterRunner) -> RunVal {
    let mut frame_idx = runner.stack.len() - 1;
    let mut yielded: Option<RunVal> = None;

    loop {
        let frame = runner.stack.get_mut(frame_idx).unwrap();
        let parent = frame.parent;

        let step_output = if frame.closed {
            StepOut::EndOk
        } else {
            frame_run(frame, yielded.unwrap_or_default())
        };

        yielded = match step_output {
            StepOut::Run(filter, dot) => {
                runner.stack.push(Frame {
                    filter,
                    scope: Scope::default(),
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
                // TODO: Popea hasta frame_idx inclusive
                runner.stack.pop();
                Some(RunVal::EndOk)
            }
            StepOut::EndErr(err) => {
                // TODO: Popea hasta frame_idx inclusive
                runner.stack.pop();
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
    let dot = frame.dot.clone();

    macro_rules! state {
        ($variant:ident($ty:ty)) => {{
            if matches!(state, FrameState::Start) {
                *state = FrameState::$variant(<$ty>::default());
            }
            match state {
                FrameState::$variant(s) => s,
                _ => unreachable!(),
            }
        }};
    }

    match frame.filter.as_ref() {
        Filter::Identity => match state {
            FrameState::Start => StepOut::Return(dot),
            _ => unreachable!(),
        },
        Filter::Empty => {
            StepOut::EndOk
        }
        Filter::Json(json) => match state {
            FrameState::Start => StepOut::Return(json.clone()),
            _ => unreachable!(),
        },
        Filter::Project(term, exp) => {
            run_project(state!(Project(ProjectState)), dot, yielded, term.clone(), exp.clone())
        }
        Filter::Slice(term, left, right) => {
            run_slice(state!(Slice(SliceState)), dot, yielded, term.clone(), left.clone(), right.clone())
        }
        Filter::Iter => {
            run_iter(state!(Iter(IterState)), dot)
        }
        Filter::Pipe(left, right) => {
            run_pipe(state!(Pipe(PipeState)), dot, yielded, left.clone(), right.clone())
        }
        Filter::TryCatch(try_, catch_) => {
            run_try_catch(state!(TryCatch(TryCatchState)), dot, yielded, try_.clone(), catch_.clone())
        }
        Filter::Comma(left, right) => {
            run_comma(state!(Comma(CommaState)), dot, yielded, left.clone(), right.clone())
        }
        _ => todo!(),
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
    dot: Arc<Json>,
    yielded: RunVal,
    term: Arc<Filter>,
    exp: Arc<Filter>,
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
            StepOut::Run(term, dot)
        }
        ProjectState::Term => match yielded {
            RunVal::Value(term) => {
                *state = ProjectState::Exp { term };
                StepOut::Run(exp, dot)
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
    dot: Arc<Json>,
    yielded: RunVal,
    term: Arc<Filter>,
    left: Option<Arc<Filter>>,
    right: Option<Arc<Filter>>,
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
            StepOut::Run(term, dot)
        }
        SliceState::Term => match yielded {
            RunVal::Value(term) => match left {
                Some(left) => {
                    *state = SliceState::Left { term };
                    StepOut::Run(left, dot)
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
                    StepOut::Run(right, dot)
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
fn run_iter(state: &mut IterState, dot: Arc<Json>) -> StepOut {
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
    dot: Arc<Json>,
    yielded: RunVal,
    left: Arc<Filter>,
    right: Arc<Filter>,
) -> StepOut {
    match state {
        PipeState::Start => {
            *state = PipeState::Left;
            StepOut::Run(left, dot)
        }
        PipeState::Left => match yielded {
            RunVal::Value(json) => {
                *state = PipeState::Right;
                StepOut::Run(right, json)
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
enum TryCatchState {
    #[default]
    Start,
    Try,
    Catch,
}
fn run_try_catch(
    state: &mut TryCatchState,
    dot: Arc<Json>,
    yielded: RunVal,
    try_: Arc<Filter>,
    catch_: Arc<Filter>,
) -> StepOut {
    match state {
        TryCatchState::Start => {
            *state = TryCatchState::Try;
            StepOut::Run(try_, dot)
        }
        TryCatchState::Try => match yielded {
            RunVal::EndErr(RunEndValue::Error(err)) => {
                *state = TryCatchState::Catch;
                StepOut::Run(catch_, err)
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
    dot: Arc<Json>,
    yielded: RunVal,
    left: Arc<Filter>,
    right: Arc<Filter>,
) -> StepOut {
    match state {
        CommaState::Start => {
            *state = CommaState::Left;
            StepOut::Run(left, dot)
        }
        CommaState::Left => match yielded {
            RunVal::EndOk => {
                *state = CommaState::Right;
                StepOut::Run(right, dot)
            }
            val => val.into(),
        },
        CommaState::Right => yielded.into(),
    }
}

// ------------------- Error Utils --------------------- //

fn str_error(s: String) -> RunEndValue {
    RunEndValue::Error(Json::String(s.into()).into())
}

#[allow(dead_code)]
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
            .[]
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
                    RunEndValue::Halt(hlt) => println!("halt: {hlt}"),
                },
            }
        }
    }
}
