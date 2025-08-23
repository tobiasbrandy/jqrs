use std::sync::{Arc, LazyLock};

use rug::{Integer, float::Round};

use crate::{
    filter::{
        Filter,
        run::{
            FuncDef, RunEndValue, RunVal, Scope, StepOut, json_fmt_error, run_module, str_error,
        },
    },
    json::Json,
    math::Number,
};

pub(super) static JQ_BUILTINS: LazyLock<im::HashMap<(Arc<str>, usize), FuncDef>> =
    LazyLock::new(|| {
        let builtins_module: Arc<_> = include_str!("builtins.jq")
            .parse::<Filter>()
            .expect("Error parsing jq builtins")
            .into();

        let module_scopes = run_module(&Scope::default(), &builtins_module);

        let jq_builtins = module_scopes
            .last()
            .expect("No builtins found")
            .funcs
            .clone();

        // Leak all scopes to make sure they are never deallocated
        for scope in &module_scopes {
            // We create a strong ref count and immediately forget it
            std::mem::forget(scope.clone());
        }

        jq_builtins
    });

#[derive(Debug, Clone, Default)]
pub(super) enum RsBuiltinState {
    #[default]
    Start,
    Unary(UnaryState),
    Binary(BinaryState),
    Range(RangeState),
}

#[rustfmt::skip]
pub(super) fn run_rs_builtin(
    state: &mut RsBuiltinState,
    _scope: &Scope,
    dot: &Arc<Json>,
    yielded: RunVal,
    name: &str,
    args: &[Arc<Filter>],
) -> StepOut {
    macro_rules! state {
        ($variant:ident) => {{
            if matches!(state, RsBuiltinState::Start) {
                *state = RsBuiltinState::$variant(Default::default());
            }
            match state {
                RsBuiltinState::$variant(s) => s,
                _ => unreachable!(),
            }
        }};
    }

    macro_rules! nullary {
        ($func:ident) => {
            nullary(dot, $func)
        };
    }

    macro_rules! unary {
        ($func:ident) => {
            unary(state!(Unary), dot, yielded, args, $func)
        };
    }

    macro_rules! binary {
        ($func:ident) => {
            binary(state!(Binary), dot, yielded, args, $func)
        };
    }

    match (name, args.len()) {
        ("empty",           0) => empty(),
        ("not",             0) => nullary!(not),
        ("range",           2) => range(state!(Range), dot, yielded, args),
        // ("path", 1) => unary(dot, yielded, args, path),
        ("_plus",           2) => binary!(plus),
        ("_negate",         1) => unary!(negate),
        ("_minus",          2) => binary!(minus),
        ("_multiply",       2) => binary!(multiply),
        ("_divide",         2) => binary!(divide),
        ("_mod",            2) => binary!(modulus),
        ("_equal",          2) => binary!(equal),
        ("_not_equal",      2) => binary!(not_equal),
        ("_less",           2) => binary!(less),
        ("_greater",        2) => binary!(greater),
        ("_lesseq",         2) => binary!(less_equal),
        ("_greatereq",      2) => binary!(greater_equal),
        ("sort",           0) => nullary!(sort),
        ("infinite",       0) => nullary!(infinite),
        ("nan",            0) => nullary!(nan),
        (_, argc) => StepOut::EndErr(str_error(format!("{name}/{argc} is not defined"))),
    }
}

// -------------- Utils -------------- //

type NullaryFn = fn(&Arc<Json>) -> Result<Arc<Json>, RunEndValue>;
fn nullary(dot: &Arc<Json>, f: NullaryFn) -> StepOut {
    match f(dot) {
        Ok(val) => StepOut::Return(val),
        Err(end) => StepOut::EndErr(end),
    }
}

type UnaryFn = fn(&Arc<Json>, &Arc<Json>) -> Result<Arc<Json>, RunEndValue>;
#[derive(Debug, Clone, Default)]
pub(super) enum UnaryState {
    #[default]
    Start,
    RunA,
}
fn unary(
    state: &mut UnaryState,
    dot: &Arc<Json>,
    yielded: RunVal,
    args: &[Arc<Filter>],
    f: UnaryFn,
) -> StepOut {
    match state {
        UnaryState::Start => {
            *state = UnaryState::RunA;
            StepOut::run(&args[0], dot)
        }
        UnaryState::RunA => match yielded {
            RunVal::Value(a) => f(&a, dot).into(),
            val => val.into(),
        },
    }
}

type BinaryFn = fn(&Arc<Json>, &Arc<Json>, &Arc<Json>) -> Result<Arc<Json>, RunEndValue>;
#[derive(Debug, Clone, Default)]
pub(super) enum BinaryState {
    #[default]
    Start,
    RunB,
    RunA(Arc<Json>),
}
fn binary(
    state: &mut BinaryState,
    dot: &Arc<Json>,
    yielded: RunVal,
    args: &[Arc<Filter>],
    f: BinaryFn,
) -> StepOut {
    match state {
        BinaryState::Start => {
            *state = BinaryState::RunB;
            StepOut::run(&args[1], dot)
        }
        BinaryState::RunB => match yielded {
            RunVal::Value(b) => {
                *state = BinaryState::RunA(b);
                StepOut::run(&args[0], dot)
            }
            val => val.into(),
        },
        BinaryState::RunA(b) => match yielded {
            RunVal::Value(a) => f(&a, b, dot).into(),
            RunVal::EndOk => {
                *state = BinaryState::RunB;
                StepOut::Continue
            }
            val => val.into(),
        },
    }
}

// -------------- Builtins -------------- //

fn empty() -> StepOut {
    StepOut::EndOk
}

fn not(dot: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    Ok(Json::arc_bool(!dot.to_bool()))
}

#[derive(Debug, Clone, Default)]
pub(super) enum RangeState {
    #[default]
    Start,
    GetStart,
    GetEnd {
        start: Arc<Json>,
    },
    InfiniteNaN,
    InfiniteInc {
        curr: Number,
    },
    InfiniteNegInfinite,
    Range {
        len: Integer,
        curr: Number,
    },
}
fn range(
    state: &mut RangeState,
    dot: &Arc<Json>,
    yielded: RunVal,
    args: &[Arc<Filter>],
) -> StepOut {
    // Range's argument cross product order is backwards (don't know why, ask jq)
    match state {
        RangeState::Start => {
            *state = RangeState::GetStart;
            StepOut::run(&args[0], dot)
        }
        RangeState::GetStart => match yielded {
            RunVal::Value(start) => {
                *state = RangeState::GetEnd { start };
                StepOut::run(&args[1], dot)
            }
            val => val.into(),
        },
        RangeState::GetEnd { start } => match yielded {
            RunVal::Value(end) => {
                *state = match (start.as_ref(), end.as_ref()) {
                    (Json::Number(start), Json::Number(end)) => {
                        if start.is_nan() {
                            RangeState::InfiniteNaN
                        } else if end.is_nan() || end.is_pos_infinite() {
                            RangeState::InfiniteInc {
                                curr: start.clone(),
                            }
                        } else if start.is_neg_infinite() {
                            RangeState::InfiniteNegInfinite
                        } else if start.is_pos_infinite() || end.is_neg_infinite() {
                            return StepOut::EndOk;
                        } else {
                            let len = (end - start).to_integer(Round::Up).unwrap();
                            RangeState::Range {
                                len,
                                curr: start.clone(),
                            }
                        }
                    }
                    (_, _) => {
                        return StepOut::EndErr(str_error(
                            "Range bounds must be numeric".to_string(),
                        ));
                    }
                };
                range(state, dot, RunVal::default(), args)
            }
            RunVal::EndOk => {
                *state = RangeState::Start;
                StepOut::Continue
            }
            val => val.into(),
        },
        RangeState::InfiniteNaN => StepOut::Yield(Json::arc_nan()),
        RangeState::InfiniteInc { curr } => {
            let ret = Json::Number(curr.clone()).into();
            curr.inc_mut();
            StepOut::Yield(ret)
        }
        RangeState::InfiniteNegInfinite => StepOut::Yield(Json::arc_neg_infinity()),
        RangeState::Range { len, curr } => {
            if *len > 0 {
                let ret = Json::Number(curr.clone()).into();
                curr.inc_mut();
                *len -= 1;
                StepOut::Yield(ret)
            } else {
                StepOut::EndOk
            }
        }
    }
}

fn plus(left: &Arc<Json>, right: &Arc<Json>, _: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    match (left.as_ref(), right.as_ref()) {
        (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l + r).into()),
        (Json::Array(l), Json::Array(r)) => Ok(Json::Array(r + l).into()),
        (Json::String(l), Json::String(r)) => Ok(Json::String(
            (String::with_capacity(l.len() + r.len()) + l.as_ref() + r.as_ref()).into(),
        )
        .into()),
        (Json::Object(l), Json::Object(r)) => Ok(Json::Object(r + l).into()),
        (Json::Null, _) => Ok(right.clone()),
        (_, Json::Null) => Ok(left.clone()),
        (l, r) => Err(str_error(format!(
            "{} and {} cannot be added",
            json_fmt_error(l),
            json_fmt_error(r)
        ))),
    }
}

fn negate(val: &Arc<Json>, _: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    match val.as_ref() {
        Json::Number(n) => Ok(Json::Number(-n).into()),
        any => Err(str_error(format!(
            "{} cannot be negated",
            json_fmt_error(any)
        ))),
    }
}

fn minus(left: &Arc<Json>, right: &Arc<Json>, _: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    match (left.as_ref(), right.as_ref()) {
        (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l - r).into()),
        (Json::Array(l), Json::Array(r)) => {
            Ok(Json::Array(l.iter().filter(|&x| !r.contains(x)).cloned().collect()).into())
        }
        (l, r) => Err(str_error(format!(
            "{} and {} cannot be substracted",
            json_fmt_error(l),
            json_fmt_error(r)
        ))),
    }
}

fn multiply(left: &Arc<Json>, right: &Arc<Json>, _: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    match (left.as_ref(), right.as_ref()) {
        (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l * r).into()),
        (Json::String(s), Json::Number(n)) => Ok(n
            .to_usize(Round::Down)
            .map(|n| Json::String(s.clone().repeat(n).into()).into())
            .unwrap_or(Json::arc_null())),
        (Json::Object(_), Json::Object(_)) => {
            fn merge_rec_arc(l: Arc<Json>, r: Arc<Json>) -> Arc<Json> {
                match (l.as_ref(), r.as_ref()) {
                    (Json::Object(l), Json::Object(r)) => {
                        Json::Object(l.clone().union_with(r.clone(), merge_rec_arc)).into()
                    }
                    _ => r,
                }
            }
            Ok(merge_rec_arc(left.clone(), right.clone()))
        }
        (l, r) => Err(str_error(format!(
            "{} and {} cannot be multiplied",
            json_fmt_error(l),
            json_fmt_error(r)
        ))),
    }
}

fn divide(left: &Arc<Json>, right: &Arc<Json>, _: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    match (left.as_ref(), right.as_ref()) {
        (jl @ Json::Number(l), jr @ Json::Number(r)) => {
            if r.is_zero() {
                Err(str_error(format!(
                    "{} and {} cannot be divided because the divisor is zero",
                    json_fmt_error(jl),
                    json_fmt_error(jr)
                )))
            } else {
                Ok(Json::Number(l / r).into())
            }
        }
        (Json::String(l), Json::String(r)) => Ok(Json::Array({
            let mut ret = l
                .split(r.as_ref())
                .map(|s| Json::String(s.into()).into())
                .collect::<im::Vector<_>>();
            if r.is_empty() {
                ret.pop_front();
                ret.pop_back();
            }
            ret
        })
        .into()),
        (l, r) => Err(str_error(format!(
            "{} and {} cannot be divided",
            json_fmt_error(l),
            json_fmt_error(r)
        ))),
    }
}

fn modulus(left: &Arc<Json>, right: &Arc<Json>, _: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    match (left.as_ref(), right.as_ref()) {
        (jl @ Json::Number(l), jr @ Json::Number(r)) => {
            if r.is_zero() {
                Err(str_error(format!(
                    "{} and {} cannot be divided (remainder) because the divisor is zero",
                    json_fmt_error(jl),
                    json_fmt_error(jr)
                )))
            } else {
                let left = l.to_integer(Round::Zero);
                let right = r.to_integer(Round::Zero);
                Ok(Json::Number(Number::Int(match (left, right) {
                    (Some(left), Some(right)) => {
                        let ret = left.clone().abs() % right.abs();
                        if left.is_negative() { -ret } else { ret }
                    }
                    (None, Some(right)) => right,
                    (Some(left), None) => left,
                    (None, None) => Integer::ZERO,
                }))
                .into())
            }
        }
        (l, r) => Err(str_error(format!(
            "{} and {} cannot be divided (remainder)",
            json_fmt_error(l),
            json_fmt_error(r)
        ))),
    }
}

fn equal(left: &Arc<Json>, right: &Arc<Json>, _: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    Ok(Json::arc_bool(
        left.is_nan() && right.is_nan() && left != right,
    ))
}

fn not_equal(left: &Arc<Json>, right: &Arc<Json>, _: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    Ok(Json::arc_bool(
        left.is_nan() || right.is_nan() || left != right,
    ))
}

fn less(left: &Arc<Json>, right: &Arc<Json>, _: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    Ok(Json::arc_bool(left.is_nan() || left < right))
}

fn less_equal(
    left: &Arc<Json>,
    right: &Arc<Json>,
    _: &Arc<Json>,
) -> Result<Arc<Json>, RunEndValue> {
    Ok(Json::arc_bool(left.is_nan() || left <= right))
}

fn greater(left: &Arc<Json>, right: &Arc<Json>, _: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    Ok(Json::arc_bool(!left.is_nan() && left > right))
}

fn greater_equal(
    left: &Arc<Json>,
    right: &Arc<Json>,
    _: &Arc<Json>,
) -> Result<Arc<Json>, RunEndValue> {
    Ok(Json::arc_bool(!left.is_nan() && left >= right))
}

fn sort(dot: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    match dot.as_ref() {
        Json::Array(arr) => {
            let mut new_arr = arr.clone();
            new_arr.sort();
            Ok(Json::Array(new_arr).into())
        }
        _ => Err(str_error(format!(
            "{} cannot be sorted, as it is not an array",
            json_fmt_error(dot)
        ))),
    }
}

fn infinite(_: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    Ok(Json::arc_infinity())
}

fn nan(_: &Arc<Json>) -> Result<Arc<Json>, RunEndValue> {
    Ok(Json::arc_nan())
}
