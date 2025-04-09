use std::{collections::HashMap, sync::LazyLock};

use rug::{float::Round, Integer};

use crate::{
    filter::{
        run::{json_fmt_error, str_error},
        Filter,
    },
    json::Json,
    math::Number,
};

use super::{run, FuncDef, RunCtx, RunResult, RunValue};

pub type RsFuncDef = fn(&mut RunCtx, &[Filter], &Json) -> RunResult;

pub type RsBuiltins = &'static HashMap<(&'static str, usize), RsFuncDef>;
pub type JqBuiltins = HashMap<(&'static str, usize), FuncDef>;

pub fn builtins() -> (RsBuiltins, JqBuiltins) {
    let rs_builtins = &*RS_BUILTINS;

    let jq_builtins = HashMap::new(); // TODO

    (rs_builtins, jq_builtins)
}

static RS_BUILTINS: LazyLock<HashMap<(&str, usize), RsFuncDef>> = LazyLock::new(|| {
    HashMap::from_iter([
        (("empty", 0), empty as RsFuncDef),
        (("not", 0), not),
        // (("path",           1),   func1     path)
        // (("range",          2),   func2     range)
        (("_plus", 2), plus),
        (("_negate", 1), negate),
        (("_minus", 2), minus),
        (("_multiply", 2), multiply),
        (("_divide", 2), divide),
        (("_mod", 2), modulus),
    ])
});

// -------------- Utils -------------- //

fn nullary(
    _: &mut RunCtx,
    args: &[Filter],
    json: &Json,
    f: fn(&Json) -> RunResult,
) -> RunResult {
    assert!(args.is_empty(), "Nullary functions take no arguments");
    f(json)
}

fn unary(
    ctx: &mut RunCtx,
    args: &[Filter],
    json: &Json,
    f: fn(&Json, &Json) -> RunValue,
) -> RunResult {
    let a = match args {
        [a] => a,
        _ => panic!("Unary functions take one argument"),
    };

    let mut ret = Vec::new();
    for a in run(ctx, a, json) {
        let a = match a {
            Ok(a) => a,
            stop => {
                ret.push(stop);
                return ret;
            }
        };

        match f(&a, json) {
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

fn binary(
    ctx: &mut RunCtx,
    args: &[Filter],
    json: &Json,
    f: fn(&Json, &Json, &Json) -> RunValue,
) -> RunResult {
    let (a, b) = match args {
        [a, b] => (a, b),
        _ => panic!("Binary functions takes two arguments"),
    };

    let mut ret = Vec::new();
    for a in run(ctx, a, json) {
        let a = match a {
            Ok(a) => a,
            stop => {
                ret.push(stop);
                return ret;
            }
        };

        for b in run(ctx, b, json) {
            let b = match b {
                Ok(b) => b,
                stop => {
                    ret.push(stop);
                    return ret;
                }
            };

            match f(&a, &b, json) {
                Ok(val) => {
                    ret.push(Ok(val));
                }
                stop => {
                    ret.push(stop);
                    return ret;
                }
            }
        }
    }

    ret
}

// -------------- Builtins -------------- //

fn empty(ctx: &mut RunCtx, args: &[Filter], json: &Json) -> RunResult {
    nullary(ctx, args, json, |_| vec![])
}

fn not(ctx: &mut RunCtx, args: &[Filter], json: &Json) -> RunResult {
    nullary(ctx, args, json, |json| {
        vec![Ok(Json::Bool(!json.to_bool()))]
    })
}

fn plus(ctx: &mut RunCtx, args: &[Filter], json: &Json) -> RunResult {
    binary(ctx, args, json, |l, r, _| match (l, r) {
        (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l + r)),
        (Json::Array(l), Json::Array(r)) => Ok(Json::Array([l.as_slice(), r.as_slice()].concat())),
        (Json::String(l), Json::String(r)) => Ok(Json::String(String::new() + l + r)),
        (Json::Object(l), Json::Object(r)) => {
            let mut ret = l.clone();
            ret.extend(r.clone());
            Ok(Json::Object(ret))
        }
        (Json::Null, r) => Ok(r.clone()),
        (l, Json::Null) => Ok(l.clone()),
        (l, r) => Err(str_error(format!(
            "{} and {} cannot be added",
            json_fmt_error(l),
            json_fmt_error(r)
        ))),
    })
}

fn negate(ctx: &mut RunCtx, args: &[Filter], json: &Json) -> RunResult {
    unary(ctx, args, json, |val, _| match val {
        Json::Number(n) => Ok(Json::Number(-n)),
        any => Err(str_error(format!(
            "{} cannot be negated",
            json_fmt_error(any)
        ))),
    })
}

fn minus(ctx: &mut RunCtx, args: &[Filter], json: &Json) -> RunResult {
    binary(ctx, args, json, |l, r, _| match (l, r) {
        (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l - r)),
        (Json::Array(l), Json::Array(r)) => Ok(Json::Array(
            l.iter().filter(|&x| !r.contains(x)).cloned().collect(),
        )),
        (l, r) => Err(str_error(format!(
            "{} and {} cannot be substracted",
            json_fmt_error(l),
            json_fmt_error(r)
        ))),
    })
}

fn multiply(ctx: &mut RunCtx, args: &[Filter], json: &Json) -> RunResult {
    binary(ctx, args, json, |l, r, _| match (l, r) {
        (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l * r)),
        (Json::String(s), Json::Number(n)) => Ok(n
            .to_usize(Round::Down)
            .map(|n| Json::String(s.clone().repeat(n)))
            .unwrap_or(Json::Null)),
        (Json::Object(l), Json::Object(r)) => {
            fn merge_rec(l: &mut HashMap<String, Json>, r: &HashMap<String, Json>) {
                for (key, r_val) in r {
                    match (l.get_mut(key), r_val) {
                        (Some(Json::Object(l_val)), Json::Object(r_val)) => {
                            merge_rec(l_val, r_val);
                        }
                        _ => {
                            l.insert(key.clone(), r_val.clone());
                        }
                    }
                }
            }
            let mut ret = l.clone();
            merge_rec(&mut ret, r);
            Ok(Json::Object(ret))
        }
        (l, r) => Err(str_error(format!(
            "{} and {} cannot be multiplied",
            json_fmt_error(l),
            json_fmt_error(r)
        ))),
    })
}

fn divide(ctx: &mut RunCtx, args: &[Filter], json: &Json) -> RunResult {
    binary(ctx, args, json, |l, r, _| match (l, r) {
        (jl @ Json::Number(l), jr @ Json::Number(r)) => {
            if r.is_zero() {
                Err(str_error(format!(
                    "{} and {} cannot be divided because the divisor is zero",
                    json_fmt_error(jl),
                    json_fmt_error(jr)
                )))
            } else {
                Ok(Json::Number(l / r))
            }
        }
        (Json::String(l), Json::String(r)) => Ok(Json::Array({
            let mut ret = l
                .split(r)
                .map(|s| Json::String(s.to_string()))
                .collect::<Vec<_>>();
            if r.is_empty() {
                ret.pop();
                ret.remove(0);
            }
            ret
        })),
        (l, r) => Err(str_error(format!(
            "{} and {} cannot be divided",
            json_fmt_error(l),
            json_fmt_error(r)
        ))),
    })
}

fn modulus(ctx: &mut RunCtx, args: &[Filter], json: &Json) -> RunResult {
    binary(ctx, args, json, |l, r, _| match (l, r) {
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
                        if left.is_negative() {
                            -ret
                        } else {
                            ret
                        }
                    }
                    (None, Some(right)) => right,
                    (Some(left), None) => left,
                    (None, None) => Integer::ZERO,
                })))
            }
        }
        (l, r) => Err(str_error(format!(
            "{} and {} cannot be divided (remainder)",
            json_fmt_error(l),
            json_fmt_error(r)
        ))),
    })
}
