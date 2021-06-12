/* value.rs
*
* Defines basic operations on SciVal objects. These operations should apply_to
* more than one of the SciVal variants. Otherwise it should probably be in
* internals.rs
*/

use core::ops;
use core::cmp;
use std::collections::HashMap;
use crate::callable::Callable;
use crate::error::{EvalError, EvalResult};
use crate::matrix;
use crate::number::Number;
use Number::*;
use SciVal as V;

#[derive(Debug, Clone)]
pub enum SciVal {
    Number(Number),
    Bool(bool),
    Matrix(usize, usize, Vec<Number>),  // numrows, numcols, index = row*numcols + col
    List(Vec<SciVal>),
    Tuple(Vec<SciVal>),
    Str(String),
    Callable(Callable)
}

pub enum Arg {
    Question,
    Val(Box<SciVal>),
}

#[derive(Debug, Clone)]
pub enum Slice<F, S> {
    Single(F),
    Range(F, S),
}

impl SciVal {
    // Function application. Defers to Callable::fun_app
    pub fn fun_app(self, args: Vec<Arg>) -> EvalResult<SciVal> {
        if let V::Callable(f) = self {
            f.fun_app(args)
        } else { Err(EvalError::TypeMismatch) }
    }

    // Function application with a keyword list. Defers to Callable::fun_app
    pub fn fun_kw_app(self, args: HashMap<String, SciVal>) -> EvalResult<SciVal> {
         if let V::Callable(f) = self {
             f.fun_kw_app(args)
         } else { Err(EvalError::TypeMismatch) }
    }

    // Function composition. Defers to Callable::fun_comp if self is callable,
    // otherwise function application is performed.
    pub fn fun_comp(self, other: SciVal) -> EvalResult<SciVal> {
        match (self, other) {
            (V::Callable(f1), V::Callable(f2)) => Ok(V::Callable(f1.fun_comp(f2)?)),
            (v1, V::Callable(f2)) => f2.fun_app(vec![Arg::Val(Box::new(v1))]),
            _ => Err(EvalError::TypeMismatch)
        }
    }

    /// Same as fun_comp except tuples are unpacked into the arguments of
    /// the second function.
    pub fn fun_comp_unpack(self, other: SciVal) -> EvalResult<SciVal> {
        match (self, other) {
            (V::Callable(f1), V::Callable(f2)) => Ok(V::Callable(f1.fun_comp_unpack(f2)?)),
            (V::Tuple(args), V::Callable(f2)) => {
                let args: Vec<Arg> = args.into_iter().map(|arg|{Arg::Val(Box::new(arg))}).collect();
                f2.fun_app(args)
            },
            _ => Err(EvalError::TypeMismatch)
        }
    }

    // Slicing can be applied to lists, strings, or callable values. Slicing
    // matrices is handled differently.
    pub fn list_slice(self, slice: Slice<i32, Option<i32>>) -> EvalResult<SciVal> {
       match self {
            V::List(mut v) => {
                match slice.resolve_negative_indices(v.len())? {
                    Slice::Single(i) => Ok(v.remove(i)),
                    Slice::Range(i, j) => {
                        v.truncate(j);
                        Ok(V::List(v.split_off(i)))
                    }
                }
            }
            V::Str(mut s) => {
                match slice.resolve_negative_indices(s.len())? {
                    Slice::Single(i) => Ok(V::Str(s.chars().nth(i as usize).unwrap().to_string())),
                    Slice::Range(i, j) => {
                        s.truncate(j);
                        Ok(V::Str(s.split_off(i)))
                    }
                }
            }
            V::Callable(f) => {
                Ok(V::Callable(f.fun_comp(Callable::mk_list_slice(slice))?))
            }
            _ => Err(EvalError::TypeMismatch)
        }
    }

    // Matrix slicing can be applied to matrices or callable objects.
    pub fn matrix_slice(self, slice1: Slice<i32, Option<i32>>, slice2: Slice<i32, Option<i32>>) -> EvalResult<SciVal> {
        match self {
            V::Matrix(r, c, v) => {
                let slice1 = slice1.resolve_negative_indices(r)?;
                let slice2 = slice2.resolve_negative_indices(c)?;
                if let (Slice::Single(i), Slice::Single(j)) = (&slice1, &slice2) {
                    return Ok(V::Number(v[(*i)*r + (*j)]));
                }
                let (r, c, v) = matrix::matrix_slice(r, c, &v, slice1, slice2);
                Ok(V::Matrix(r, c, v))
            }
            V::Callable(f) => {
                Ok( V::Callable(f.fun_comp(Callable::mk_matrix_slice(slice1, slice2))?))
            }
             _ => Err(EvalError::TypeMismatch)
         }
    }

    pub fn inv(self) -> EvalResult<SciVal> {
        match self {
            V::Number(n) => Ok(V::Number(n.recip()?)),
            V::Matrix(r, c, mut v) => {
                if r != c { return Err(EvalError::InvalidMatrixShape); }
                if matrix::matrix_det(r, &v) == Float(0f64) { return Err(EvalError::NoninvertableMatrix); }
                let ret = matrix::matrix_inv(r, &mut v)?;
                Ok(V::Matrix(r, c, ret))
            }
            _ => Err(EvalError::TypeMismatch),
        }
    }

    pub fn pow(self, rhs: SciVal) -> EvalResult<SciVal> {
        match (self, rhs) {
            (V::Number(n1), V::Number(n2)) => Ok(V::Number(n1.pow(n2)?)),
            _ => Err(EvalError::TypeMismatch),
        }
    }

    pub fn abs(self) -> EvalResult<SciVal> {
        match self {
            V::Number(n) => Ok(V::Number(n.abs())),
            V::Matrix(r, c, v) => {
                let v = v.into_iter().map(|x| x.abs()).collect();
                Ok(V::Matrix(r, c, v))
            }
            _ => Err(EvalError::TypeMismatch)
        }
    }

    pub fn conjugate(self) -> EvalResult<SciVal> {
        match self {
            V::Number(n) => Ok(V::Number(n.conjugate())),
            V::Matrix(r, c, v) => {
                let v = v.into_iter().map(|x| x.conjugate()).collect();
                Ok(V::Matrix(r, c, v))
            }
            _ => Err(EvalError::TypeMismatch)
        }
    }

    pub fn equals(&self, rhs: &SciVal) -> EvalResult<SciVal> {
        match (self, rhs) {
            (V::Bool(b1), V::Bool(b2)) => Ok(V::Bool(b1 == b2)),
            (V::Number(n1), V::Number(n2)) => Ok(V::Bool(n1 == n2)),
            (V::Matrix(r1, c1, m1), V::Matrix(r2, c2, m2)) => {
                if r1 != r2 || c1 != c2 { return Ok(V::Bool(false)); }
                Ok(V::Bool(m1.iter().zip(m2.iter()).all(|p|{ p.0 == p.1 })))
            }
            (V::List(v1), V::List(v2)) |
            (V::Tuple(v1), V::Tuple(v2)) => {
                for (a, b) in v1.iter().zip(v2.iter()) {
                    if let V::Bool(false) = a.equals(b)? { return Ok(V::Bool(false)); }
                }
                Ok(V::Bool(true))
            }
            _ => Err(EvalError::TypeMismatch),
        }
    }

    pub fn not_equals(&self, rhs: &SciVal) -> EvalResult<SciVal> {
        self.equals(rhs)?.logical_not()
    }

    pub fn compare(&self, rhs: &SciVal) -> EvalResult<cmp::Ordering> {
        if let (V::Number(n1), V::Number(n2)) = (self, rhs) { n1.compare(n2) }
        else { Err(EvalError::TypeMismatch) }
    }

    pub fn lt(&self, rhs: &SciVal) -> EvalResult<SciVal> {
        match self.compare(rhs)? {
            cmp::Ordering::Less => Ok(V::Bool(true)),
            cmp::Ordering::Equal |
            cmp::Ordering::Greater => Ok(V::Bool(false))
        }
    }

    pub fn gt(&self, rhs: &SciVal) -> EvalResult<SciVal> {
        match self.compare(rhs)? {
            cmp::Ordering::Less |
            cmp::Ordering::Equal => Ok(V::Bool(false)),
            cmp::Ordering::Greater => Ok(V::Bool(true))
        }
    }

    pub fn ge(&self, rhs: &SciVal) -> EvalResult<SciVal> {
        match self.compare(rhs)? {
            cmp::Ordering::Less => Ok(V::Bool(false)),
            cmp::Ordering::Equal |
            cmp::Ordering::Greater => Ok(V::Bool(true))
        }
    }

    pub fn le(&self, rhs: &SciVal) -> EvalResult<SciVal> {
        match self.compare(rhs)? {
            cmp::Ordering::Less |
            cmp::Ordering::Equal => Ok(V::Bool(true)),
            cmp::Ordering::Greater => Ok(V::Bool(false))
        }
    }

    pub fn logical_and(self, rhs: SciVal) -> EvalResult<SciVal> {
        if let (V::Bool(b1), V::Bool(b2)) = (self, rhs) { Ok(V::Bool(b1 && b2)) }
        else { Err(EvalError::TypeMismatch) }
    }

    pub fn logical_or(self, rhs: SciVal) -> EvalResult<SciVal> {
        if let (V::Bool(b1), V::Bool(b2)) = (self, rhs) { Ok(V::Bool(b1 || b2)) }
        else { Err(EvalError::TypeMismatch) }
    }

    pub fn logical_not(self) -> EvalResult<SciVal> {
        if let V::Bool(b) = self { Ok(V::Bool(!b)) }
        else { Err(EvalError::TypeMismatch) }
    }
}

impl ops::Add<SciVal> for SciVal {
    type Output = EvalResult<SciVal>;
    fn add(self, rhs: SciVal) -> EvalResult<SciVal> {
        match (self, rhs) {
            (V::Matrix(r1, c1, m1), V::Matrix(r2, c2, m2)) => {
                if c1 != c2 || r1 != r2 {
                    return Err(EvalError::IncompatibleMatrixShapes);
                }
                let mut ret = vec![Int(0); m1.len()];
                for i in 0..ret.len() {
                    ret[i] = m1[i] + m2[i];
                }
                Ok(V::Matrix(r1, c1, ret))
            }
            (V::Number(n), V::Matrix(r, c, m)) |
            (V::Matrix(r, c, m), V::Number(n)) => {
                let mut ret = vec![Int(0); m.len()];
                for i in 0..ret.len() {
                    ret[i] = m[i] + n;
                }
                Ok(V::Matrix(r, c, ret))
            }
            (V::Number(n1), V::Number(n2)) => Ok(V::Number(n1 + n2)),
            (V::List(mut v1), V::List(mut v2)) => {
                v1.append(&mut v2);
                Ok(V::List(v1))
            }
            (V::Str(s1), V::Str(s2)) => {
                Ok(V::Str(format!("{}{}", s1, s2)))
            }
            _ => Err(EvalError::TypeMismatch)
        }
    }
}

impl ops::Mul<SciVal> for SciVal {
    type Output = EvalResult<SciVal>;

    fn mul(self, rhs: SciVal) -> EvalResult<SciVal> {
        match (self, rhs) {
            (V::Matrix(r1, c1, m1), V::Matrix(r2, c2, m2)) => {
                if c1 != r2 {
                    return Err(EvalError::IncompatibleMatrixShapes);
                }
                let mut ret = vec![Int(0); r1*c2];
                for r in 0..r1 {
                    for c in 0..c2 {
                        let mut sum = Int(0);
                        for k in 0..c1 {
                            sum = sum + m1[r*c1 + k] * m2[k*c2 + c];
                        }
                        ret[r*c2 + c] = sum;
                    }
                }
                Ok(V::Matrix(r1, c2, ret))
            }
            (V::Number(n), V::Matrix(r, c, m)) |
            (V::Matrix(r, c, m), V::Number(n)) => {
                let mut ret = vec![Int(0); m.len()];
                for i in 0..ret.len() {
                    ret[i] = m[i] * n;
                }
                Ok(V::Matrix(r, c, ret))
            }
            (V::Number(n1), V::Number(n2)) => Ok(V::Number(n1 * n2)),
            (V::List(v), V::Number(Int(n))) => {
                if n < 0 { return Err(EvalError::OutOfRange); }
                let n = n as usize;
                let mut vret: Vec<SciVal> = Vec::new();
                for _ in 0..n {
                    vret.append(&mut v.to_vec());
                }
                Ok(V::List(vret))
            }
            (V::Str(s), V::Number(Int(n))) => {
                if n < 0 { return Err(EvalError::OutOfRange); }
                let n = n as usize;
                let mut sret = String::new();
                for _ in 0..n {
                    sret.push_str(&s);
                }
                Ok(V::Str(sret))
            }
            _ => Err(EvalError::TypeMismatch)
        }
    }
}

impl ops::Rem for SciVal {
   type Output = EvalResult<SciVal>;
   fn rem(self, rhs: SciVal) -> EvalResult<SciVal> {
       if let (V::Number(n1), V::Number(n2)) = (self, rhs) {
           Ok(V::Number((n1 % n2)?))
       } else { Err(EvalError::TypeMismatch) }
   }
}

impl ops::Sub<SciVal> for SciVal {
    type Output = EvalResult<SciVal>;
    fn sub(self, rhs: SciVal) -> EvalResult<SciVal> {
        self + (V::Number(Int(-1))*rhs)?
    }
}

impl ops::Div<SciVal> for SciVal {
    type Output = EvalResult<SciVal>;
    fn div(self, rhs: SciVal) -> EvalResult<SciVal> {
        self * rhs.inv()?
    }
}

impl ops::Neg for SciVal {
    type Output = EvalResult<SciVal>;
    fn neg(self) -> EvalResult<SciVal> {
        V::Number(Int(0)) - self
    }
}
