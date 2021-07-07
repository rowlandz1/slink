/* callable.rs
 *
 * Defines first-class callable values such as closures, macros, and slicing operations.
 */

use std::collections::HashMap;
use crate::ast::ExprA;
use crate::error::{EvalError, EvalResult};
use crate::exec::Environ;
use crate::builtins::eval_builtin_function;
use crate::value::{Slice, SciVal};
use Callable::*;
use NextCall::*;

#[derive(Debug, Clone)]
pub enum Callable {
    Closure {
        env: HashMap<String, SciVal>,           // environment in which closure was defined
        name: Option<String>,                   // closure name (for error tracing and recursion)
        params: Vec<String>,                    // unapplied parameter list
        app: HashMap<String, SciVal>,           // applied parameters
        expr: Result<ExprA, String>,     // inner expression (or string for internal functions)
        next: NextCall,                         // for function composition
    },
    ListSlice(Slice<i32, Option<i32>>, NextCall),
    MatrixSlice(Slice<i32, Option<i32>>, Slice<i32, Option<i32>>, NextCall),
}

#[derive(Debug, Clone)]
pub enum NextCall {
    NoNext,
    Next(Box<Callable>),
    NextUnpack(Box<Callable>),
}

impl Callable {
    pub fn closure(env: HashMap<String, SciVal>, name: Option<String>, params: Vec<String>,
                   app: HashMap<String, SciVal>, expr: Result<ExprA, String>) -> Callable {
        Closure{env, name, params, app, expr: expr, next: NoNext}
    }

    pub fn mk_list_slice(slice: Slice<i32, Option<i32>>) -> Callable { ListSlice(slice, NoNext) }
    pub fn mk_matrix_slice(slice1: Slice<i32, Option<i32>>, slice2: Slice<i32, Option<i32>>) -> Callable { MatrixSlice(slice1, slice2, NoNext) }

    /// Function application with a normal argument list
    pub fn fun_app(self, args: Vec<SciVal>) -> EvalResult<SciVal> {
        if let Closure{mut env, name, mut params, mut app, expr, next} = self {
            if params.len() < args.len() {
                return Err(EvalError::ArityMismatch);
            }
            let appliedparams = params.split_off(params.len() - args.len());
            for (param, arg) in appliedparams.into_iter().zip(args) {
                match arg {
                    SciVal::Any => params.push(param),
                    arg => { app.insert(param, arg); }
                }
            }
            if params.len() == 0 {
                env.extend(app.into_iter());
                let result = match expr {
                    Ok(expr) => Environ::from_map(env).evaluate(expr),
                    Err(name) => eval_builtin_function(name, env),
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
                    NoNext => Ok(result),
                    Next(next) => next.fun_app(vec![result]),
                    NextUnpack(next) => if let SciVal::Tuple(values) = result { next.fun_app(values) }
                                        else { Err(EvalError::NothingToUnpack) }
                }
            } else { Ok(SciVal::Callable(Closure{env, name, params, app, expr, next})) }
        }
        else if let ListSlice(slice, next) = self {
            if args.len() != 1 { panic!("Error, ListSlice was applied to wrong number of arguments"); }
            let v = args.into_iter().next().unwrap();
            let result = v.list_slice(slice)?;
            match next {
                NoNext => Ok(result),
                Next(next) => next.fun_app(vec![result]),
                NextUnpack(next) => if let SciVal::Tuple(values) = result { next.fun_app(values) }
                                        else { Err(EvalError::NothingToUnpack) }
            }
        }
        else if let MatrixSlice(slice1, slice2, next) = self {
            if args.len() != 1 { panic!("Error, MatrixSlice was applied to wrong number of arguments"); }
            let v = args.into_iter().next().unwrap();
            let result = v.matrix_slice(slice1, slice2)?;
            match next {
                NoNext => Ok(result),
                Next(next) => next.fun_app(vec![result]),
                NextUnpack(next) => if let SciVal::Tuple(values) = result { next.fun_app(values) }
                                        else { Err(EvalError::NothingToUnpack) }
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
                     Ok(expr) => Environ::from_map(env).evaluate(expr),
                     Err(name) => eval_builtin_function(name, env),
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
                    NoNext => Ok(result),
                    Next(next) => next.fun_app(vec![result]),
                    NextUnpack(next) => if let SciVal::Tuple(values) = result { next.fun_app(values) }
                                        else { Err(EvalError::NothingToUnpack) }
                }
             } else {
                 Ok(SciVal::Callable(Closure{env, name, params, app, expr, next}))
             }

         } else { Err(EvalError::TypeMismatch) }
    }

    /// function composition
    pub fn fun_comp(self, other: Callable) -> EvalResult<Callable> {
        match self {
            Closure { env, name, params, app, expr, next } => {
                let next = match next {
                    NoNext => Next(Box::new(other)),
                    Next(next) => Next(Box::new(next.fun_comp(other)?)),
                    NextUnpack(next) => NextUnpack(Box::new(next.fun_comp(other)?)),
                };
                Ok(Closure{env, name, params, app, expr, next})
            }
            ListSlice(slice, next) => {
                let next = match next {
                    NoNext => Next(Box::new(other)),
                    Next(next) => Next(Box::new(next.fun_comp(other)?)),
                    NextUnpack(next) => NextUnpack(Box::new(next.fun_comp(other)?)),
                };
                Ok(ListSlice(slice, next))
            }
            MatrixSlice(slice1, slice2, next) => {
                let next = match next {
                    NoNext => Next(Box::new(other)),
                    Next(next) => Next(Box::new(next.fun_comp(other)?)),
                    NextUnpack(next) => NextUnpack(Box::new(next.fun_comp(other)?)),
                };
                Ok(MatrixSlice(slice1, slice2, next))
            }
        }
    }

    /// function composition, but if the first function returns a tuple, then
    /// the tuple contents are unpacked into the arguments of the second function.
    pub fn fun_comp_unpack(self, other: Callable) -> EvalResult<Callable> {
        match self {
            Closure { env, name, params, app, expr, next } => {
                let next = match next {
                    NoNext => NextUnpack(Box::new(other)),
                    Next(next) => Next(Box::new(next.fun_comp_unpack(other)?)),
                    NextUnpack(next) => NextUnpack(Box::new(next.fun_comp_unpack(other)?)),
                };
                Ok(Closure{env, name, params, app, expr, next})
            }
            ListSlice(slice, next) => {
                let next = match next {
                    NoNext => NextUnpack(Box::new(other)),
                    Next(next) => Next(Box::new(next.fun_comp_unpack(other)?)),
                    NextUnpack(next) => NextUnpack(Box::new(next.fun_comp_unpack(other)?)),
                };
                Ok(ListSlice(slice, next))
            }
            MatrixSlice(slice1, slice2, next) => {
                let next = match next {
                    NoNext => NextUnpack(Box::new(other)),
                    Next(next) => Next(Box::new(next.fun_comp_unpack(other)?)),
                    NextUnpack(next) => NextUnpack(Box::new(next.fun_comp_unpack(other)?)),
                };
                Ok(MatrixSlice(slice1, slice2, next))
            }
        }
    }
}