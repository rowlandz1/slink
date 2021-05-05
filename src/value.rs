/* value.rs
 *
 * Defines basic operations on SciVal objects. These operations should apply_to
 * more than one of the SciVal variants. Otherwise it should probably be in
 * internals.rs
 */

 use core::ops;
 use core::cmp;
 use std::collections::HashMap;
 use crate::ast::*;
 use SciVal::*;
 use crate::internals;
 use crate::number;
 use crate::error::*;
 use crate::matrix;
 use crate::exec::Environ;
 use crate::macros;

 impl SciVal {
     // Unpacks top-level tuple values from an argument list.
     // Used by fun_app
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

     // Function application with a normal argument list. "self" should be a Closure
     // a Macro, or a slice, otherwise a a type error will be returned.
     pub fn fun_app(self, args: Vec<Arg>) -> EvalResult<SciVal> {
         let args = Self::flatten_arg_list(args);
         if let Closure{mut env, name, mut params, mut app, expr, next} = self {
             if params.len() < args.len() {
                 return Err(EvalError::ArityMismatch);
             }
             let appliedparams = params.split_off(params.len() - args.len());
             for (param, arg) in appliedparams.iter().zip(args) {
                 match arg {
                     Arg::Question => { params.push(param.clone()); }
                     Arg::Val(arg) => { app.insert(param.clone(), *arg.clone()); }
                 }
             }
             if params.len() == 0 {
                 env.extend(app.into_iter());
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
                 Ok(Closure{env, name, params, app, expr, next})
             }
         }
         else if let Macro(name, next) = self {
             let mut newargs: Vec<SciVal> = Vec::new();
             for arg in args.into_iter() {
                 match arg {
                     Arg::Question => { return Err(EvalError::QuestionMarkMacroArg); }
                     Arg::Val(v) => { newargs.push(*v); }
                 }
             }
             let result = macros::apply_to_macro(name, newargs)?;
             match next {
                 Some(next) => next.fun_app(vec![Arg::Val(Box::new(result))]),
                 None => Ok(result),
             }
         }
         else if let ListSlice(slice, next) = self {
             if args.len() != 1 { panic!("Error, ListSlice was applied to wrong number of arguments"); }
             let v = match args.into_iter().next().unwrap() {
                 Arg::Question => panic!("Error, ListSlice cannot accept question mark arguments"),
                 Arg::Val(v) => v,
             };
             let result = v.list_slice(slice)?;
             match next {
                 Some(next) => next.fun_app(vec![Arg::Val(Box::new(result))]),
                 None => Ok(result),
             }
         }
         else if let MatrixSlice(slice1, slice2, next) = self {
             if args.len() != 1 { panic!("Error, MatrixSlice was applied to wrong number of arguments"); }
             let v = match args.into_iter().next().unwrap() {
                 Arg::Question => panic!("Error, MatrixSlice cannot accept question mark arguments"),
                 Arg::Val(v) => v,
             };
             let result = v.matrix_slice(slice1, slice2)?;
             match next {
                 Some(next) => next.fun_app(vec![Arg::Val(Box::new(result))]),
                 None => Ok(result),
             }
         }
         else { Err(EvalError::TypeMismatch) }
     }

     // Function application with a keyword list. "self" should be a Closure.
     pub fn fun_kw_app(self, mut args: HashMap<String, SciVal>) -> EvalResult<SciVal> {
          if let Closure{mut env, name, params, mut app, expr, next} = self {
              let mut newparams: Vec<String> = Vec::new();
              for param in params {
                  if let Some(v) = args.remove(&param) { app.insert(param, v); }
                  else { newparams.push(param); }
              }
              let params = newparams;
              for (key, val) in args {
                  if !app.contains_key(&key) { return Err(EvalError::InvalidKeywordArgument); }
                  app.insert(key, val);
              }

              if params.len() == 0 {
                  env.extend(app.into_iter());
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
                  Ok(Closure{env, name, params, app, expr, next})
              }

          } else { Err(EvalError::TypeMismatch) }
     }

     // function composition. "self" should be a callable value,
     // i.e. a Closure, Macro, or ListSlice
     pub fn fun_comp(self, other: SciVal) -> EvalResult<SciVal> {
         match self {
             Closure { env, name, params, app, expr, next } => {
                 let newnext = match next {
                     Some(next) => next.fun_comp(other)?,
                     None => other,
                 };
                 Ok(Closure{env, name, params, app, expr, next: Some(Box::new(newnext))})
             }
             Macro(name, next) => {
                 let newnext = match next {
                     Some(next) => next.fun_comp(other)?,
                     None => other,
                 };
                 Ok(Macro(name, Some(Box::new(newnext))))
             }
             ListSlice(slice, next) => {
                 let newnext = match next {
                     Some(next) => next.fun_comp(other)?,
                     None => other,
                 };
                 Ok(ListSlice(slice, Some(Box::new(newnext))))
             }
             MatrixSlice(slice1, slice2, next) => {
                 let newnext = match next {
                     Some(next) => next.fun_comp(other)?,
                     None => other,
                 };
                 Ok(MatrixSlice(slice1, slice2, Some(Box::new(newnext))))
             }
             _ => other.fun_app(vec![Arg::Val(Box::new(self))])
         }
     }

     // Slicing can be applied to lists, strings, or callable values. Slicing
     // matrices is handled differently
     pub fn list_slice(self, slice: Slice<i32, Option<i32>>) -> EvalResult<SciVal> {
        match self {
             List(mut v) => {
                 match slice.resolve_negative_indices(v.len())? {
                     Slice::Single(i) => Ok(v.remove(i)),
                     Slice::Range(i, j) => {
                         v.truncate(j);
                         Ok(List(v.split_off(i)))
                     }
                 }
             }
             Str(mut s) => {
                 match slice.resolve_negative_indices(s.len())? {
                     Slice::Single(i) => Ok(Str(s.chars().nth(i as usize).unwrap().to_string())),
                     Slice::Range(i, j) => {
                         s.truncate(j);
                         Ok(Str(s.split_off(i)))
                     }
                 }
             }
             Closure { env, name, params, app, expr, next } => {
                 Closure { env, name, params, app, expr, next }.fun_comp(ListSlice(slice, None))
             }
             Macro(name, next) => {
                 Macro(name, next).fun_comp(ListSlice(slice, None))
             }
             ListSlice(firstslice, next) => {
                 ListSlice(firstslice, next).fun_comp(ListSlice(slice, None))
             }
             MatrixSlice(slice1, slice2, next) => {
                 MatrixSlice(slice1, slice2, next).fun_comp(ListSlice(slice, None))
             }
             _ => Err(EvalError::TypeMismatch)
         }
     }

     // Matrix slicing can be applied to matrices or callable objects
     pub fn matrix_slice(self, slice1: Slice<i32, Option<i32>>, slice2: Slice<i32, Option<i32>>) -> EvalResult<SciVal> {
         match self {
             Matrix(r, c, v) => {
                 let slice1 = slice1.resolve_negative_indices(r)?;
                 let slice2 = slice2.resolve_negative_indices(c)?;
                 if let (Slice::Single(i), Slice::Single(j)) = (&slice1, &slice2) {
                     return Ok(Number(v[(*i)*r + (*j)]));
                 }
                 let (r, c, v) = matrix::matrix_slice(r, c, &v, slice1, slice2);
                 Ok(Matrix(r, c, v))
             }
             Closure { env, name, params, app, expr, next } => {
                 Closure { env, name, params, app, expr, next }.fun_comp(MatrixSlice(slice1, slice2, None))
             }
             Macro(name, next) => {
                 Macro(name, next).fun_comp(MatrixSlice(slice1, slice2, None))
             }
             ListSlice(firstslice, next) => {
                 ListSlice(firstslice, next).fun_comp(MatrixSlice(slice1, slice2, None))
             }
             MatrixSlice(firstslice1, firstslice2, next) => {
                 MatrixSlice(firstslice1, firstslice2, next).fun_comp(MatrixSlice(slice1, slice2, None))
             }
              _ => Err(EvalError::TypeMismatch)
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

     pub fn pow(self, rhs: SciVal) -> EvalResult<SciVal> {
         match (self, rhs) {
             (Number(n1), Number(n2)) => Ok(Number(n1.pow(n2)?)),
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

     pub fn equals(&self, rhs: &SciVal) -> EvalResult<SciVal> {
         match (self, rhs) {
             (Bool(b1), Bool(b2)) => Ok(Bool(b1 == b2)),
             (Number(n1), Number(n2)) => Ok(Bool(n1 == n2)),
             (Matrix(r1, c1, m1), Matrix(r2, c2, m2)) => {
                 if r1 != r2 || c1 != c2 { return Ok(Bool(false)); }
                 Ok(Bool(m1.iter().zip(m2.iter()).all(|p|{ p.0 == p.1 })))
             }
             (List(v1), List(v2)) |
             (Tuple(v1), Tuple(v2)) => {
                 for (a, b) in v1.iter().zip(v2.iter()) {
                     if let Bool(false) = a.equals(b)? { return Ok(Bool(false)); }
                 }
                 Ok(Bool(true))
             }
             _ => Err(EvalError::TypeMismatch),
         }
     }

     pub fn not_equals(&self, rhs: &SciVal) -> EvalResult<SciVal> {
         self.equals(rhs)?.negate()
     }

     pub fn compare(&self, rhs: &SciVal) -> EvalResult<cmp::Ordering> {
         if let (Number(n1), Number(n2)) = (self, rhs) { n1.compare(n2) }
         else { Err(EvalError::TypeMismatch) }
     }

     pub fn lt(&self, rhs: &SciVal) -> EvalResult<SciVal> {
         match self.compare(rhs)? {
             cmp::Ordering::Less => Ok(Bool(true)),
             cmp::Ordering::Equal |
             cmp::Ordering::Greater => Ok(Bool(false))
         }
     }

     pub fn gt(&self, rhs: &SciVal) -> EvalResult<SciVal> {
         match self.compare(rhs)? {
             cmp::Ordering::Less |
             cmp::Ordering::Equal => Ok(Bool(false)),
             cmp::Ordering::Greater => Ok(Bool(true))
         }
     }

     pub fn ge(&self, rhs: &SciVal) -> EvalResult<SciVal> {
         match self.compare(rhs)? {
             cmp::Ordering::Less => Ok(Bool(false)),
             cmp::Ordering::Equal |
             cmp::Ordering::Greater => Ok(Bool(true))
         }
     }

     pub fn le(&self, rhs: &SciVal) -> EvalResult<SciVal> {
         match self.compare(rhs)? {
             cmp::Ordering::Less |
             cmp::Ordering::Equal => Ok(Bool(true)),
             cmp::Ordering::Greater => Ok(Bool(false))
         }
     }

     pub fn negate(&self) -> EvalResult<SciVal> {
         if let Bool(b) = self { Ok(Bool(!b)) }
         else { Err(EvalError::TypeMismatch) }
     }

     pub fn logical_and(self, rhs: SciVal) -> EvalResult<SciVal> {
         match (self, rhs) {
             (Bool(b1), Bool(b2)) => Ok(Bool(b1 && b2)),
             _ => Err(EvalError::TypeMismatch)
         }
     }

     pub fn logical_or(self, rhs: SciVal) -> EvalResult<SciVal> {
         match (self, rhs) {
             (Bool(b1), Bool(b2)) => Ok(Bool(b1 || b2)),
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
             (Str(s1), Str(s2)) => {
                 Ok(Str(format!("{}{}", s1, s2)))
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
             (Str(s), Number(number::Number::Int(n))) => {
                 if n < 0 { return Err(EvalError::OutOfRange); }
                 let n = n as usize;
                 let mut sret = String::new();
                 for _ in 0..n {
                     sret.push_str(&s);
                 }
                 Ok(Str(sret))
             }
             _ => Err(EvalError::TypeMismatch)
         }
     }
 }

impl ops::Rem for SciVal {
    type Output = EvalResult<SciVal>;
    fn rem(self, rhs: SciVal) -> EvalResult<SciVal> {
        if let (Number(n1), Number(n2)) = (self, rhs) {
            Ok(Number((n1 % n2)?))
        } else { Err(EvalError::TypeMismatch) }
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
