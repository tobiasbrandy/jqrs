use std::{
    cell::RefCell,
    sync::{Arc, LazyLock, Mutex},
};

use rug::{Integer, float::Round};

use crate::{
    filter::{
        Filter,
        run::{FuncId, Scope, json_fmt_error, run, str_error, yield_},
    },
    json::Json,
    math::Number,
};

use super::{FuncDef, RunCtx, RunEnd, RunFile, RunOut, RunValue};

pub(super) static JQ_BUILTINS: LazyLock<im::HashMap<FuncId, Arc<FuncDef>>> = LazyLock::new(|| {
    let ctx = RunCtx {
        file: RunFile::Module("builtins.jq".to_string()),
        scope: RefCell::new(Arc::new(Mutex::new(Scope {
            vars: im::HashMap::new(),
            funcs: im::HashMap::new(),
            labels: im::HashSet::new(),
            is_top_level: true,
        }))),
    };

    let filter: Arc<_> = include_str!("builtins.jq")
        .parse::<Filter>()
        .expect("Error parsing jq builtins")
        .into();

    filter.run(&ctx, &Json::arc_null()).last();

    ctx.scope.into_inner().lock().unwrap().funcs.clone()
});

pub(super) async fn run_rs_builtin(
    name: &str,
    argc: usize,
    out: RunOut,
    ctx: &RunCtx,
    args: &[Arc<Filter>],
    json: &Arc<Json>,
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
        ("range",       2) => range,
    //  ("path",        1) => path,
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
        ("infinite",    0) => infinite,
        ("nan",         0) => nan,
    )
}

// -------------- Utils -------------- //

async fn nullary(
    out: RunOut,
    _ctx: &RunCtx,
    args: &[Arc<Filter>],
    json: &Arc<Json>,
    f: fn(&Arc<Json>) -> RunValue,
) -> RunEnd {
    assert!(args.is_empty(), "Nullary functions take no arguments");

    match f(json) {
        Ok(val) => yield_!(out, val),
        Err(end) => return Some(end),
    }

    None
}

async fn unary(
    out: RunOut,
    ctx: &RunCtx,
    args: &[Arc<Filter>],
    json: &Arc<Json>,
    f: fn(&Arc<Json>, &Arc<Json>) -> RunValue,
) -> RunEnd {
    let a = match args {
        [a] => a,
        _ => panic!("Unary functions take one argument"),
    };

    let mut a_gen = run(ctx, a, json);
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
    out: RunOut,
    ctx: &RunCtx,
    args: &[Arc<Filter>],
    json: &Arc<Json>,
    f: fn(&Arc<Json>, &Arc<Json>, &Arc<Json>) -> RunValue,
) -> RunEnd {
    let (a, b) = match args {
        [a, b] => (a, b),
        _ => panic!("Binary functions take two arguments"),
    };

    let mut b_gen = run(ctx, b, json);
    for b in &mut b_gen {
        let mut a_gen = run(ctx, a, json);
        for a in &mut a_gen {
            match f(&a, &b, json) {
                Ok(val) => yield_!(out, val),
                Err(end) => return Some(end),
            }
        }
        if let Some(end) = a_gen.end() {
            return Some(end);
        }
    }
    if let Some(end) = b_gen.end() {
        return Some(end);
    }

    None
}

// -------------- Builtins -------------- //

async fn empty(_: RunOut, _: &RunCtx, args: &[Arc<Filter>], _: &Arc<Json>) -> RunEnd {
    assert!(args.is_empty(), "Nullary functions take no arguments");
    None
}

async fn not(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    nullary(out, ctx, args, json, |json| {
        Ok(Json::arc_bool(!json.to_bool()))
    })
    .await
}

async fn range(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    async fn range(out: &RunOut, start: &Arc<Json>, end: &Arc<Json>) -> RunEnd {
        match (start.as_ref(), end.as_ref()) {
            (Json::Number(start), Json::Number(end)) => {
                if start.is_nan() {
                    // Infinite null
                    loop {
                        yield_!(out, Json::arc_nan());
                    }
                }

                let mut curr = start.clone();

                if end.is_nan() || end.is_pos_infinite() {
                    // Infinite
                    loop {
                        yield_!(out, Json::Number(curr.clone()).into());
                        curr.inc_mut();
                    }
                }

                if start.is_neg_infinite() {
                    // Infinite neg_infinite
                    loop {
                        yield_!(out, Json::Number(start.clone()).into());
                    }
                }

                if start.is_pos_infinite() || end.is_neg_infinite() {
                    return None;
                }

                let mut len = (end - start).to_integer(Round::Up).unwrap();
                while len > 0 {
                    yield_!(out, Json::Number(curr.clone()).into());
                    curr.inc_mut();
                    len -= 1;
                }

                None
            }
            (_, _) => Some(str_error("Range bounds must be numeric".to_string())),
        }
    }

    // Argument cross product order is backwards (don't know why, ask jq)
    let (a, b) = match args {
        [a, b] => (a, b),
        _ => panic!("Binary functions take two arguments"),
    };

    let mut a_gen = run(ctx, a, json);
    for a in &mut a_gen {
        let mut b_gen = run(ctx, b, json);
        for b in &mut b_gen {
            if let Some(end) = range(&out, &a, &b).await {
                return Some(end);
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

async fn plus(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        match (l.as_ref(), r.as_ref()) {
            (Json::Number(l), Json::Number(r)) => Ok(Json::Number(l + r).into()),
            (Json::Array(l), Json::Array(r)) => Ok(Json::Array(r + l).into()),
            (Json::String(l), Json::String(r)) => Ok(Json::String(
                (String::with_capacity(l.len() + r.len()) + l.as_ref() + r.as_ref()).into(),
            )
            .into()),
            (Json::Object(l), Json::Object(r)) => Ok(Json::Object(r + l).into()),
            (Json::Null, _) => Ok(r.clone()),
            (_, Json::Null) => Ok(l.clone()),
            (l, r) => Err(str_error(format!(
                "{} and {} cannot be added",
                json_fmt_error(l),
                json_fmt_error(r)
            ))),
        }
    })
    .await
}

async fn negate(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    unary(out, ctx, args, json, |val, _| match val.as_ref() {
        Json::Number(n) => Ok(Json::Number(-n).into()),
        any => Err(str_error(format!(
            "{} cannot be negated",
            json_fmt_error(any)
        ))),
    })
    .await
}

async fn minus(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        match (l.as_ref(), r.as_ref()) {
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
    })
    .await
}

async fn multiply(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        match (l.as_ref(), r.as_ref()) {
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
                Ok(merge_rec_arc(l.clone(), r.clone()))
            }
            (l, r) => Err(str_error(format!(
                "{} and {} cannot be multiplied",
                json_fmt_error(l),
                json_fmt_error(r)
            ))),
        }
    })
    .await
}

async fn divide(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        match (l.as_ref(), r.as_ref()) {
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
    })
    .await
}

async fn modulus(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        match (l.as_ref(), r.as_ref()) {
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
    })
    .await
}

async fn equal(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::arc_bool(l.is_nan() && r.is_nan() && l != r))
    })
    .await
}

async fn not_equal(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::arc_bool(l.is_nan() || r.is_nan() || l != r))
    })
    .await
}

async fn less(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::arc_bool(l.is_nan() || l < r))
    })
    .await
}

async fn less_equal(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::arc_bool(l.is_nan() || l <= r))
    })
    .await
}

async fn greater(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::arc_bool(!l.is_nan() && l > r))
    })
    .await
}

async fn greater_equal(
    out: RunOut,
    ctx: &RunCtx,
    args: &[Arc<Filter>],
    json: &Arc<Json>,
) -> RunEnd {
    binary(out, ctx, args, json, |l, r, _| {
        Ok(Json::arc_bool(!l.is_nan() && l >= r))
    })
    .await
}

async fn sort(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    nullary(out, ctx, args, json, |json| match json.as_ref() {
        Json::Array(arr) => {
            let mut new_arr = arr.clone();
            new_arr.sort();
            Ok(Json::Array(new_arr).into())
        }
        _ => Err(str_error(format!(
            "{} cannot be sorted, as it is not an array",
            json_fmt_error(json)
        ))),
    })
    .await
}

async fn infinite(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    nullary(out, ctx, args, json, |_| Ok(Json::arc_infinity())).await
}

async fn nan(out: RunOut, ctx: &RunCtx, args: &[Arc<Filter>], json: &Arc<Json>) -> RunEnd {
    nullary(out, ctx, args, json, |_| Ok(Json::arc_nan())).await
}
