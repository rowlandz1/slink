use core::ops;
use std::fmt;
use std::cmp;
use crate::error::{EvalError, EvalResult};
use Number::*;

#[derive(Debug, Clone, Copy)]
pub enum Number {
    Int(i32),
    Float(f64),
    IntCmplx(i32, i32),
    FloatCmplx(f64, f64),
}

impl Number {
    pub fn recip(self) -> EvalResult<Number> {
        match self {
            Int(0) => Err(EvalError::DivideByZero),
            Int(n) => Ok(Float(1f64 / n as f64)),
            Float(n) => {
                if n == 0f64 { Err(EvalError::DivideByZero) }
                else { Ok(Float(1f64 / n)) }
            }
            IntCmplx(0, 0) => Err(EvalError::DivideByZero),
            IntCmplx(a, b) => {
                let (a, b) = (a as f64, b as f64);
                let mag = a*a + b*b;
                Ok(FloatCmplx(a/mag, -b/mag))
            }
            FloatCmplx(a, b) => {
                if a == 0f64 && b == 0f64 { return Err(EvalError::DivideByZero); }
                let mag = a*a + b* b;
                Ok(FloatCmplx(a/mag, -b/mag))
            }
        }
    }

    pub fn conjugate(self) -> Number {
        match self {
            IntCmplx(a, b) => IntCmplx(a, -b),
            FloatCmplx(a, b) => FloatCmplx(a, -b),
            realvalue => realvalue
        }
    }

    pub fn abs(self) -> Number {
        match self {
            Int(n) => Int(n.abs()),
            Float(n) => Float(n.abs()),
            IntCmplx(a, b) => {
                let mag2 = (a*a + b*b) as f64;
                Float(mag2.sqrt())
            }
            FloatCmplx(a, b) => Float((a*a + b*b).sqrt())
        }
    }

    pub fn sqrt(self) -> EvalResult<Number> {
        match self {
            Int(n) => {
                if n >= 0 { Ok(Float((n as f64).sqrt())) }
                else { Err(EvalError::OutOfRange) }
            }
            Float(n) => {
                if n >= 0f64 { Ok(Float((n as f64).sqrt())) }
                else { Err(EvalError::OutOfRange) }
            }
            _ => Err(EvalError::TypeMismatch)
        }
    }
}

impl ops::Add<Number> for Number {
    type Output = Number;
    fn add(self, rhs: Number) -> Number {
        match (self, rhs) {
            (Int(n1), Int(n2))   => Int(n1 + n2),
            (Int(n1), Float(n2)) => Float(n1 as f64 + n2),
            (Int(n1), IntCmplx(a2, b2)) => IntCmplx(a2 + n1, b2),
            (Int(n1), FloatCmplx(a2, b2)) => FloatCmplx(n1 as f64 + a2, b2),
            (Float(n1), Float(n2)) => Float(n1 + n2),
            (Float(n1), IntCmplx(a2, b2)) => FloatCmplx(n1 + a2 as f64, b2 as f64),
            (Float(n1), FloatCmplx(a2, b2)) => FloatCmplx(n1 + a2, b2),
            (IntCmplx(a1, b1), IntCmplx(a2, b2)) => IntCmplx(a1 + a2, b1 + b2),
            (IntCmplx(a1, b1), FloatCmplx(a2, b2)) => FloatCmplx(a1 as f64 + a2, b1 as f64 + b2),
            (FloatCmplx(a1, b1), FloatCmplx(a2, b2)) => FloatCmplx(a1 + a2, b1 + b2),
            (lhs, rhs) => rhs + lhs,
        }
    }
}

impl ops::Mul<Number> for Number {
    type Output = Number;
    fn mul(self, rhs: Number) -> Number {
        match (self, rhs) {
            (Int(n1), Int(n2))   => Int(n1 * n2),
            (Int(n1), Float(n2)) => Float(n1 as f64 * n2),
            (Int(n1), IntCmplx(a2, b2)) => IntCmplx(n1*a2, n1*b2),
            (Int(n1), FloatCmplx(a2, b2)) => FloatCmplx(n1 as f64 * a2, n1 as f64 * b2),
            (Float(n1), Float(n2)) => Float(n1 * n2),
            (Float(n1), IntCmplx(a2, b2)) => FloatCmplx(n1 * a2 as f64, n1 * b2 as f64),
            (Float(n1), FloatCmplx(a2, b2)) => FloatCmplx(n1*a2, n1*b2),
            (IntCmplx(a1, b1), IntCmplx(a2, b2)) => IntCmplx(a1*a2 - b1*b2, a1*b2 + a2*b1),
            (IntCmplx(a1, b1), FloatCmplx(a2, b2)) => {
                let (a1, b1) = (a1 as f64, b1 as f64);
                FloatCmplx(a1*a2 - b1*b2, a1*b2 + a2*b1)
            }
            (FloatCmplx(a1, b1), FloatCmplx(a2, b2)) => FloatCmplx(a1*a2 - b1*b2, a1*b2 + a2*b1),
            (lhs, rhs) => rhs * lhs,
        }
    }
}

impl ops::Neg for Number {
    type Output = Number;
    fn neg(self) -> Number {
        match self {
            Int(n) => Int(-n),
            Float(n) => Float(-n),
            IntCmplx(a, b) => IntCmplx(-a, -b),
            FloatCmplx(a, b) => FloatCmplx(-a, -b)
        }
    }
}

impl ops::Sub<Number> for Number {
    type Output = Number;
    fn sub(self, rhs: Number) -> Number {
        self + -rhs
    }
}

impl ops::Div<Number> for Number {
    type Output = EvalResult<Number>;
    fn div(self, rhs: Number) -> EvalResult<Number> {
        Ok(self * rhs.recip()?)
    }
}

impl cmp::PartialEq<Number> for Number {
    fn eq(&self, other: &Number) -> bool {
        let (a1, b1) = match *self {
            Int(n) => (n as f64, 0f64),
            Float(n) => (n, 0f64),
            IntCmplx(a, b) => (a as f64, b as f64),
            FloatCmplx(a, b) => (a, b)
        };
        let (a2, b2) = match *other {
            Int(n) => (n as f64, 0f64),
            Float(n) => (n, 0f64),
            IntCmplx(a, b) => (a as f64, b as f64),
            FloatCmplx(a, b) => (a, b)
        };
        a1 == a2 && b1 == b2
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let p = f.precision().unwrap_or(2);
        match self {
            Int(n) => { f.write_fmt(format_args!("{}", n)) }
            Float(n) => { f.write_fmt(format_args!("{:.p$}", n, p=p)) }
            IntCmplx(a, b) => { f.write_fmt(format_args!("{}+{}i", a, b)) }
            FloatCmplx(a, b) => { f.write_fmt(format_args!("{:.p$}+{:.p$}i", a, b, p=p)) }
        }
    }
}
