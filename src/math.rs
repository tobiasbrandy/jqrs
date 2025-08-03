use core::f32;
use std::{
    cmp::Ordering,
    ops::{Add, Div, Mul, Neg, Sub},
    str::FromStr,
};

use rug::{float::ParseFloatError, ops::CompleteRound, Float, Integer};

pub const PRECISION: u32 = 53; // f64 (double) precision

#[derive(Debug, Clone)]
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
            Number::Decimal(f) => f.to_integer_round(round).and_then(|(i, _)| i.to_usize()),
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            Number::Int(i) => i.is_positive(),
            Number::Decimal(f) => f.is_sign_positive(),
        }
    }
    pub fn is_negative(&self) -> bool {
        match self {
            Number::Int(i) => i.is_negative(),
            Number::Decimal(f) => f.is_sign_negative(),
        }
    }
    pub fn is_nan(&self) -> bool {
        match self {
            Number::Int(_) => false,
            Number::Decimal(f) => f.is_nan(),
        }
    }
    pub fn is_infinite(&self) -> bool {
        match self {
            Number::Int(_) => false,
            Number::Decimal(f) => f.is_infinite(),
        }
    }
    pub fn is_pos_infinite(&self) -> bool {
        self.is_infinite() && self.is_positive()
    }
    pub fn is_neg_infinite(&self) -> bool {
        self.is_infinite() && self.is_negative()
    }
    pub fn is_zero(&self) -> bool {
        match self {
            Number::Int(i) => i.is_zero(),
            Number::Decimal(f) => f.is_zero(),
        }
    }

    pub fn inc(mut self) -> Self {
        self.inc_mut();
        self
    }
    pub fn inc_mut(&mut self) {
        match self {
            Number::Int(i) => *i += Integer::ONE,
            Number::Decimal(f) => *f += Integer::ONE,
        }
    }

    pub fn dec(mut self) -> Self {
        self.dec_mut();
        self
    }
    pub fn dec_mut(&mut self) {
        match self {
            Number::Int(i) => *i -= Integer::ONE,
            Number::Decimal(f) => *f -= Integer::ONE,
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
impl Ord for Number {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.is_nan(), other.is_nan()) {
            (true, true) => return Ordering::Equal,
            (true, false) => return Ordering::Less,
            (false, true) => return Ordering::Greater,
            (false, false) => {}
        }

        let mret = match (self, other) {
            (Number::Int(a), Number::Int(b)) => Some(a.cmp(b)),
            (Number::Decimal(a), Number::Decimal(b)) => a.partial_cmp(b),
            (Number::Int(a), Number::Decimal(b)) => a.partial_cmp(b),
            (Number::Decimal(a), Number::Int(b)) => a.partial_cmp(b),
        };

        match mret {
            Some(ret) => ret,
            None => unreachable!(
                "NaN values were previously handled, so partial_cmp should never return None"
            ),
        }
    }
}
impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}
impl Eq for Number {}
impl Add for &Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(a), Number::Int(b)) => Number::Int(a.clone() + b),
            (Number::Decimal(a), Number::Decimal(b)) => Number::Decimal(a.clone() + b),
            (Number::Int(a), Number::Decimal(b)) => Number::Decimal(a + b.clone()),
            (Number::Decimal(a), Number::Int(b)) => Number::Decimal(a.clone() + b),
        }
    }
}
impl Sub for &Number {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(a), Number::Int(b)) => Number::Int(a.clone() - b),
            (Number::Decimal(a), Number::Decimal(b)) => Number::Decimal(a.clone() - b),
            (Number::Int(a), Number::Decimal(b)) => Number::Decimal(a - b.clone()),
            (Number::Decimal(a), Number::Int(b)) => Number::Decimal(a.clone() - b),
        }
    }
}
impl Mul for &Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(a), Number::Int(b)) => Number::Int(a.clone() * b),
            (Number::Decimal(a), Number::Decimal(b)) => Number::Decimal(a.clone() * b),
            (Number::Int(a), Number::Decimal(b)) => Number::Decimal(a * b.clone()),
            (Number::Decimal(a), Number::Int(b)) => Number::Decimal(a.clone() * b),
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
            (Number::Decimal(a), Number::Decimal(b)) => Number::Decimal(a.clone() / b),
            (Number::Int(a), Number::Decimal(b)) => Number::Decimal(a / b.clone()),
            (Number::Decimal(a), Number::Int(b)) => Number::Decimal(a.clone() / b),
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
