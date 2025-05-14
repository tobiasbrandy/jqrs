use std::{
    cell::RefCell,
    collections::HashMap,
    sync::{Arc, LazyLock},
};

use rug::{float::Round, Integer};

use crate::{
    filter::{
        run::{json_fmt_error, str_error},
        Filter,
    },
    json::Json,
    math::Number,
};

use super::{yield_, FuncDef, RunCtx, RunEnd, RunFile, RunGen, RunOut, RunState, RunValue};

pub static JQ_BUILTINS: LazyLock<HashMap<(Arc<str>, usize), FuncDef>> = LazyLock::new(|| {
    let ctx = RunCtx {
        file: RunFile::Module("builtins.jq".to_string()),
        custom_builtins: Some(HashMap::new()),
        state: RefCell::new(RunState::default()),
    };

    include_str!("builtins.jq")
        .parse::<Filter>()
        .expect("Error parsing jq builtins")
        .run(&ctx, &Json::Null)
        .last();

    ctx.state.into_inner().funcs
});

pub async fn run_rs_builtin(
    name: &str,
    argc: usize,
    out: RunOut<'_>,
    ctx: &RunCtx,
    args: &[Filter],
    json: &Json,
) -> RunEnd {
    macro_rules! dispatch {
        ($( ($name:literal, $argc:literal) => $func:ident ),+ $(,)? ) => {{
            match (name, argc) {
                $( ($name, $argc) => $func(out, ctx, args, json).await, )+
                _ => Some(str_error(format!("{name}/{argc} is not defined"))),
            }
        }};
    }

    dispatch!(
        ("empty",       0) => empty,
        ("not",         0) => not,
    //  ("path",        1) => path,
    //  ("range",       2) => range,
        ("_plus",       2) => plus,
        ("_negate",     1) => negate,
        ("_minus",      2) => minus,
        ("_multiply",   2) => multiply,
        ("_divide",     2) => divide,
        ("_mod",        2) => modulus,
        ("_equal",      2) => equal,
        ("_notequal",   2) => not_equal,
        ("_less",       2) => less,
        ("_greater",    2) => greater,
        ("_lesseq",     2) => less_equal,
        ("_greatereq",  2) => greater_equal,
        ("sort",        0) => sort,
    )
}

// -------------- Utils -------------- //

async fn nullary(
    out: RunOut<'_>,
    _ctx: &RunCtx,
    args: &[Filter],
    json: &Json,
    f: fn(&Json) -> RunValue,
) -> RunEnd {
    assert!(args.is_empty(), "Nullary functions take no arguments");

    match f(json) {
        Ok(val) => yield_!(out, val),
        Err(end) => return Some(end),
    }

    None
}

async fn unary(
    out: RunOut<'_>,
    ctx: &RunCtx,
    args: &[Filter],
    json: &Json,
    f: fn(&Json, &Json) -> RunValue,
) -> RunEnd {
    let a = match args {
        [a] => a,
        _ => panic!("Unary functions take one argument"),
    };

    let mut a_gen = RunGen::build(ctx, a, json);
    for a in &mut a_gen {
        match f(&a, json) {
            Ok(val) => yield_!(out, val),
            Err(end) => return Some(end),
        }
    }
    if let Some(end) = a_gen.end() {
        return Some(end);
    }

    None
}

async fn binary(
    out: RunOut<'_>,
    ctx: &RunCtx,
    args: &[Filter],
    json: &Json,
    f: fn(&Json, &Json, &Json) -> RunValue,
) -> RunEnd {
    let (a, b) = match args {
        [a, b] => (a, b),
        _ => panic!("Binary functions takes two arguments"),
    };

    let mut a_gen = RunGen::build(ctx, a, json);
    for a in &mut a_gen {
        let mut b_gen = RunGen::build(ctx, b, json);
        for b in &mut b_gen {
            match f(&a, &b, json) {
                Ok(val) => yield_!(out, val),
                Err(end) => return Some(end),
            }
        }
        if let Some(end) = b_gen.end() {
            return Some(end);
        }
    }
    if let Some(end) = a_gen.end() {
        return Some(end);
    }

    None
}

// -------------- Builtins -------------- //

async fn empty(_: RunOut<'_>, _: &RunCtx, args: &[Filter], _: &Json) -> RunEnd {
    assert!(args.is_empty(), "Nullary functions take no arguments");
    None
}

async fn not(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    nullary(out, ctx, args, json, |json| Ok(Json::Bool(!json.to_bool()))).await
}

async fn plus(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| match (l, r) {
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
    .await
}

async fn negate(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    unary(out, ctx, args, json, |val, _| match val {
        Json::Number(n) => Ok(Json::Number(-n)),
        any => Err(str_error(format!(
            "{} cannot be negated",
            json_fmt_error(any)
        ))),
    })
    .await
}

async fn minus(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| match (l, r) {
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
    .await
}

async fn multiply(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| match (l, r) {
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
    .await
}

async fn divide(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| match (l, r) {
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
    .await
}

async fn modulus(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| match (l, r) {
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
    .await
}

async fn equal(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::Bool({
            if l.is_nan() || r.is_nan() {
                false
            } else {
                l == r
            }
        }))
    })
    .await
}

async fn not_equal(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::Bool({
            if l.is_nan() || r.is_nan() {
                true
            } else {
                l != r
            }
        }))
    })
    .await
}

async fn less(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::Bool({
            if l.is_nan() {
                true
            } else {
                l < r
            }
        }))
    })
    .await
}

async fn less_equal(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::Bool({
            if l.is_nan() {
                true
            } else {
                l <= r
            }
        }))
    })
    .await
}

async fn greater(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::Bool({
            if l.is_nan() {
                false
            } else {
                l > r
            }
        }))
    })
    .await
}

async fn greater_equal(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::Bool({
            if l.is_nan() {
                false
            } else {
                l >= r
            }
        }))
    })
    .await
}

async fn sort(out: RunOut<'_>, ctx: &RunCtx, args: &[Filter], json: &Json) -> RunEnd {
    nullary(out, ctx, args, json, |json| match json {
        Json::Array(arr) => {
            let mut new_arr = arr.clone();
            new_arr.sort();
            Ok(Json::Array(new_arr))
        }
        _ => Err(str_error(format!(
            "{} cannot be sorted, as it is not an array",
            json_fmt_error(json)
        ))),
    })
    .await
}
