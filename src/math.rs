use core::f32;
use std::{
    ops::{Add, Div, Mul, Neg, Sub},
    str::FromStr,
};

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

    pub fn to_integer(&self, round: rug::float::Round) -> Option<Integer> {
        match self {
            Number::Int(i) => Some(i.clone()),
            Number::Decimal(f) => f.to_integer_round(round).map(|(i, _)| i),
        }
    }

    pub fn to_usize(&self, round: rug::float::Round) -> Option<usize> {
        match self {
            Number::Int(i) => i.to_usize(),
            Number::Decimal(f) => f
                .to_integer_round(round)
                .and_then(|(i, _)| i.to_usize()),
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Number::Int(i) => i.is_zero(),
            Number::Decimal(f) => f.is_zero(),
        }
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
impl FromStr for Number {
    type Err = ParseFloatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let f = Float::parse(s)?.complete(PRECISION);

        if f.is_integer() {
            Ok(Number::Int(f.to_integer().unwrap()))
        } else {
            Ok(Number::Decimal(f))
        }
    }
}
impl Add for &Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(a), Number::Int(b)) => Number::Int(a.clone() + b),
            (Number::Decimal(a), Number::Decimal(b)) => {
                Number::Decimal(a.clone() + b)
            }
            (Number::Int(a), Number::Decimal(b)) => {
                Number::Decimal(a + b.clone())
            }
            (Number::Decimal(a), Number::Int(b)) => {
                Number::Decimal(a.clone() + b)
            }
        }
    }
}
impl Sub for &Number {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(a), Number::Int(b)) => Number::Int(a.clone() - b),
            (Number::Decimal(a), Number::Decimal(b)) => {
                Number::Decimal(a.clone() - b)
            }
            (Number::Int(a), Number::Decimal(b)) => {
                Number::Decimal(a - b.clone())
            }
            (Number::Decimal(a), Number::Int(b)) => {
                Number::Decimal(a.clone() - b)
            }
        }
    }
}
impl Mul for &Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(a), Number::Int(b)) => Number::Int(a.clone() * b),
            (Number::Decimal(a), Number::Decimal(b)) => {
                Number::Decimal(a.clone() * b)
            }
            (Number::Int(a), Number::Decimal(b)) => {
                Number::Decimal(a * b.clone())
            }
            (Number::Decimal(a), Number::Int(b)) => {
                Number::Decimal(a.clone() * b)
            }
        }
    }
}
impl Div for &Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(a), Number::Int(b)) => {
                if a.is_divisible(b) {
                    Number::Int(a.clone() / b)
                } else {
                    Number::Decimal(Float::with_val(PRECISION, a) / b)
                }
            }
            (Number::Decimal(a), Number::Decimal(b)) => {
                Number::Decimal(a.clone() / b)
            }
            (Number::Int(a), Number::Decimal(b)) => {
                Number::Decimal(a / b.clone())
            }
            (Number::Decimal(a), Number::Int(b)) => {
                Number::Decimal(a.clone() / b)
            }
        }
    }
}
impl Neg for &Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Number::Int(a) => Number::Int(a.clone().neg()),
            Number::Decimal(a) => Number::Decimal(a.clone().neg()),
        }
    }
}
