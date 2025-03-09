use core::f32;

use rug::{float::ParseFloatError, ops::CompleteRound, Float, Integer};

pub const PRECISION: u32 = 53; // f64 (double) precision

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(Integer),
    Decimal(Float),
}
impl Number {
    pub fn nan() -> Number {
        Number::Decimal(Float::with_val(PRECISION, f32::NAN))
    }
    pub fn infinity() -> Number {
        Number::Decimal(Float::with_val(PRECISION, f32::INFINITY))
    }
    pub fn neg_infinity() -> Number {
        Number::Decimal(Float::with_val(PRECISION, f32::NEG_INFINITY))
    }
}
impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Int(i) => write!(f, "{i}"),
            Number::Decimal(ff) => write!(f, "{ff}"),
        }
    }
}

pub fn parse_number(s: &str) -> Result<Number, ParseFloatError> {
    let f = Float::parse(s)?.complete(PRECISION);

    if f.is_integer() {
        Ok(Number::Int(f.to_integer().unwrap()))
    } else {
        Ok(Number::Decimal(f))
    }
}
