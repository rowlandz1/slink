/* value.rs
 *
 * Defines basic operations on SciVal objects. These operations should apply_to
 * more than one of the SciVal variants. Otherwise it should probably be in
 * internals.rs
 */

 use core::ops;
 use crate::ast::*;
 use SciVal::*;
 use crate::internals;
 use crate::number;
 use crate::error::*;
 use crate::matrix;
 use crate::exec::Environ;

 impl SciVal {
     fn flatten_arg_list(args: Vec<Arg>) -> Vec<Arg> {
         let mut newargs: Vec<Arg> = Vec::new();
         for arg in args {
             match arg {
                 Arg::Question => { newargs.push(Arg::Question); }
                 Arg::Val(v) => if let Tuple(v) = *v {
                     for va in v { newargs.push(Arg::Val(Box::new(va))); }
                 } else { newargs.push(Arg::Val(v)); }
             }
         }
         newargs
     }

     pub fn fun_app(self, args: Vec<Arg>) -> EvalResult<SciVal> {
         let args = Self::flatten_arg_list(args);
         if let Closure{mut env, name, mut params, expr, next} = self {
             if params.len() < args.len() {
                 return Err(EvalError::ArityMismatch);
             }
             let appliedparams = params.split_off(params.len() - args.len());
             for (param, arg) in appliedparams.iter().zip(args) {
                 match arg {
                     Arg::Question => { params.push(param.clone()); }
                     Arg::Val(arg) => { env.insert(param.clone(), *arg.clone()); }
                 }
             }
             if params.len() == 0 {
                 let result = match expr {
                     Ok(expr) => Environ::from_map(env).evaluate(*expr),
                     Err(name) => internals::apply_to_internal(name, env),
                 };
                 let result = match result {
                     Ok(v) => v,
                     Err(e) => {
                         if let Some(name) = name {
                             return Err(EvalError::InResolvedExpr(Box::new(e), name));
                         } else { return Err(e); }
                     }
                 };
                 match next {
                     Some(next) => next.fun_app(vec![Arg::Val(Box::new(result))]),
                     None => Ok(result),
                 }
             } else {
                 Ok(Closure{env, name, params, expr, next})
             }
         } else { Err(EvalError::TypeMismatch) }
     }

     pub fn fun_comp(self, other: SciVal) -> EvalResult<SciVal> {
         if let Closure{env, name, params, expr, next} = self {
             let newnext = match next {
                 Some(next) => next.fun_comp(other)?,
                 None => other,
             };
             Ok(Closure{env, name, params, expr, next: Some(Box::new(newnext))})
         } else {
             other.fun_app(vec![Arg::Val(Box::new(self))])
         }
     }

     pub fn inv(self) -> EvalResult<SciVal> {
         match self {
             Number(n) => Ok(Number(n.recip()?)),
             Matrix(r, c, mut v) => {
                 if r != c { return Err(EvalError::InvalidMatrixShape); }
                 if matrix::matrix_det(r, &v) == number::Number::Float(0f64) { return Err(EvalError::NoninvertableMatrix); }
                 let ret = matrix::matrix_inv(r, &mut v)?;
                 Ok(Matrix(r, c, ret))
             }
             _ => Err(EvalError::TypeMismatch),
         }
     }

     pub fn abs(self) -> EvalResult<SciVal> {
         match self {
             Number(n) => Ok(Number(n.abs())),
             Matrix(r, c, v) => {
                 let v = v.into_iter().map(|x| x.abs()).collect();
                 Ok(Matrix(r, c, v))
             }
             _ => Err(EvalError::TypeMismatch)
         }
     }

     pub fn conjugate(self) -> EvalResult<SciVal> {
         match self {
             Number(n) => Ok(Number(n.conjugate())),
             Matrix(r, c, v) => {
                 let v = v.into_iter().map(|x| x.conjugate()).collect();
                 Ok(Matrix(r, c, v))
             }
             _ => Err(EvalError::TypeMismatch)
         }
     }
 }

 impl ops::Add<SciVal> for SciVal {
     type Output = EvalResult<SciVal>;
     fn add(self, rhs: SciVal) -> EvalResult<SciVal> {
         match (self, rhs) {
             (Matrix(r1, c1, m1), Matrix(r2, c2, m2)) => {
                 if c1 != c2 || r1 != r2 {
                     return Err(EvalError::IncompatibleMatrixShapes);
                 }
                 let mut ret = vec![number::Number::Int(0); m1.len()];
                 for i in 0..ret.len() {
                     ret[i] = m1[i] + m2[i];
                 }
                 Ok(Matrix(r1, c1, ret))
             }
             (Number(n), Matrix(r, c, m)) |
             (Matrix(r, c, m), Number(n)) => {
                 let mut ret = vec![number::Number::Int(0); m.len()];
                 for i in 0..ret.len() {
                     ret[i] = m[i] + n;
                 }
                 Ok(Matrix(r, c, ret))
             }
             (Number(n1), Number(n2)) => Ok(Number(n1 + n2)),
             (List(mut v1), List(mut v2)) => {
                 v1.append(&mut v2);
                 Ok(List(v1))
             }
             _ => Err(EvalError::TypeMismatch)
         }
     }
 }

 impl ops::Mul<SciVal> for SciVal {
     type Output = EvalResult<SciVal>;

     fn mul(self, rhs: SciVal) -> EvalResult<SciVal> {
         match (self, rhs) {
             (Matrix(r1, c1, m1), Matrix(r2, c2, m2)) => {
                 if c1 != r2 {
                     return Err(EvalError::IncompatibleMatrixShapes);
                 }
                 let mut ret = vec![number::Number::Int(0); r1*c2];
                 for r in 0..r1 {
                     for c in 0..c2 {
                         let mut sum = number::Number::Int(0);
                         for k in 0..c1 {
                             sum = sum + m1[r*c1 + k] * m2[k*c2 + c];
                         }
                         ret[r*c2 + c] = sum;
                     }
                 }
                 Ok(Matrix(r1, c2, ret))
             }
             (Number(n), Matrix(r, c, m)) |
             (Matrix(r, c, m), Number(n)) => {
                 let mut ret = vec![number::Number::Int(0); m.len()];
                 for i in 0..ret.len() {
                     ret[i] = m[i] * n;
                 }
                 Ok(Matrix(r, c, ret))
             }
             (Number(n1), Number(n2)) => Ok(Number(n1 * n2)),
             (List(v), Number(number::Number::Int(n))) => {
                 if n < 0 { return Err(EvalError::OutOfRange); }
                 let n = n as usize;
                 let mut vret: Vec<SciVal> = Vec::new();
                 for _ in 0..n {
                     vret.append(&mut v.to_vec());
                 }
                 Ok(List(vret))
             }
             _ => Err(EvalError::TypeMismatch)
         }
     }
 }

 impl ops::Sub<SciVal> for SciVal {
     type Output = EvalResult<SciVal>;
     fn sub(self, rhs: SciVal) -> EvalResult<SciVal> {
         self + (Number(number::Number::Int(-1))*rhs)?
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
         Number(number::Number::Int(0)) - self
     }
 }
