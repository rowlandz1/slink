/* callable.rs
 *
 * Defines first-class callable values such as closures, macros, and slicing operations.
 */

use std::collections::HashMap;
use crate::ast::AstExpr;
use crate::error::{EvalError, EvalResult};
use crate::exec::Environ;
use crate::internals::apply_to_internal;
use crate::macros::apply_to_macro;
use crate::value::{Arg, Slice, SciVal};
use Callable::*;

#[derive(Debug, Clone)]
pub enum Callable {
    Closure {
        env: HashMap<String, SciVal>,           // environment in which closure was defined
        name: Option<String>,                   // closure name (for error tracing and recursion)
        params: Vec<String>,                    // unapplied parameter list
        app: HashMap<String, SciVal>,           // applied parameters
        expr: Result<Box<AstExpr>, String>,     // inner expression (or string for internal functions)
        next: Option<Box<Callable>>,              // if Some(v), then this closure is the first in a composition
    },
    Macro(String, Option<Box<Callable>>),         // name, next
    ListSlice(Slice<i32, Option<i32>>, Option<Box<Callable>>),
    MatrixSlice(Slice<i32, Option<i32>>, Slice<i32, Option<i32>>, Option<Box<Callable>>),
}

impl Callable {
    // Unpacks top-level tuple values from an argument list.
    // Used by fun_app
    fn flatten_arg_list(args: Vec<Arg>) -> Vec<Arg> {
        let mut newargs: Vec<Arg> = Vec::new();
        for arg in args {
            match arg {
                Arg::Question => { newargs.push(Arg::Question); }
                Arg::Val(v) => if let SciVal::Tuple(v) = *v {
                    for va in v { newargs.push(Arg::Val(Box::new(va))); }
                } else { newargs.push(Arg::Val(v)); }
            }
        }
        newargs
    }

    // Function application with a normal argument list
    pub fn fun_app(self, args: Vec<Arg>) -> EvalResult<SciVal> {
        let args = Self::flatten_arg_list(args);
        if let Closure{mut env, name, mut params, mut app, expr, next} = self {
            if params.len() < args.len() {
                return Err(EvalError::ArityMismatch);
            }
            let appliedparams = params.split_off(params.len() - args.len());
            for (param, arg) in appliedparams.into_iter().zip(args) {
                match arg {
                    Arg::Question => params.push(param),
                    Arg::Val(arg) => { app.insert(param, *arg); }
                }
            }
            if params.len() == 0 {
                env.extend(app.into_iter());
                let result = match expr {
                    Ok(expr) => Environ::from_map(env).evaluate(*expr),
                    Err(name) => apply_to_internal(name, env),
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
                Ok(SciVal::Callable(Closure{env, name, params, app, expr, next}))
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
            let result = apply_to_macro(name, newargs)?;
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
                     Err(name) => apply_to_internal(name, env),
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
                 Ok(SciVal::Callable(Closure{env, name, params, app, expr, next}))
             }

         } else { Err(EvalError::TypeMismatch) }
    }

    // function composition
    pub fn fun_comp(self, other: Callable) -> EvalResult<Callable> {
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
        }
    }
}
