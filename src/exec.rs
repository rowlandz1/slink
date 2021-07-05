/* exec.rs
 *
 * Defines Environs and implements execute and evaluate operations for them.
 */

use std::collections::HashMap;
use crate::ast::{Expr as E, ExprA, Stmt, StmtA, AstSlice};
use crate::callable::Callable;
use crate::error::{EvalError, EvalResult};
use crate::builtins::get_builtin;
use crate::number::Number;
use crate::value::{SciVal as V, Slice};
use Callable::*;
use Number::*;

#[derive(Debug, Clone)]
pub struct Environ {
    var_store: HashMap<String, V>
}

impl Environ {
    pub fn new() -> Environ {
        Environ{var_store: HashMap::new()}
    }

    pub fn from_map(m: HashMap<String, V>) -> Environ {
        Environ {var_store: m}
    }

    /// Executes a statement and returns the output.
    pub fn execute(&mut self, stmt: StmtA) -> Result<String, String> {
        match *stmt.stmt {
            Stmt::Assign(v, e) => {
                match self.evaluate(e) {
                    Ok(V::Callable(Closure{env, params, app, expr, next, ..})) => {
                        let name = Some(v.clone());
                        self.var_store.insert(v, V::Callable(Closure{env, name, params, app, expr, next}));
                        Ok(String::from(""))
                    }
                    Ok(evaled) => { self.var_store.insert(v, evaled); Ok(String::from("")) }
                    Err(e) => Err(e.to_string()),
                }
            }
            Stmt::Display(e) => {
                match self.evaluate(e) {
                    Ok(evaled) => Ok(evaled.to_string()),
                    Err(err) => Err(err.to_string())
                }
            }
        }
    }

    pub fn evaluate(&self, expr: ExprA) -> EvalResult<V> {
        match *expr.expr {
            E::Binop(op, lhs, rhs) => {
                let lhs = self.evaluate(lhs)?;
                let rhs = self.evaluate(rhs)?;
                match op.as_str() {
                    "+"  => lhs + rhs,
                    "-"  => lhs - rhs,
                    "*"  => lhs * rhs,
                    "/"  => lhs * rhs.inv()?,
                    "%"  => lhs % rhs,
                    "**" => lhs.pow(rhs),
                    "."  => lhs.fun_comp(rhs),
                    "$"  => lhs.fun_comp_unpack(rhs),
                    "==" => lhs.equals(&rhs),
                    "!=" => lhs.not_equals(&rhs),
                    "<=" => lhs.le(&rhs),
                    ">=" => lhs.ge(&rhs),
                    "<"  => lhs.lt(&rhs),
                    ">"  => lhs.gt(&rhs),
                    "&&" => lhs.logical_and(rhs),
                    "||" => lhs.logical_or(rhs),
                    _ => panic!("Unrecognized binary operator")
                }
            }
            E::Unop(op, inner) => {
                let inner = self.evaluate(inner)?;
                match op.as_str() {
                    "-" => -inner,
                    "!" => inner.logical_not(),
                    _ => panic!("Unrecognized unary operator")
                }
            }
            E::ListIdx(e, slice) => {
                self.evaluate(e)?.list_slice(self.evaluate_slice(slice)?)
            }
            E::MatrixIdx(e, rslice, cslice) => {
                let rslice = self.evaluate_slice(rslice)?;
                let cslice = self.evaluate_slice(cslice)?;
                self.evaluate(e)?.matrix_slice(rslice, cslice)
            }
            E::Matrix(r, c, v) => {
                let v = traverse(v, |e| { if let V::Number(n) = self.evaluate(e)? { Ok(n) }
                                          else { Err(EvalError::TypeMismatch) }
                })?;
                Ok(V::Matrix(r, c, v))
            }
            E::List(v) => Ok(V::List(traverse(v, |x| self.evaluate(x))?)),
            E::Tuple(v) => Ok(V::Tuple(traverse(v, |x| self.evaluate(x))?)),
            E::Str(s) => Ok(V::Str(s)),
            E::Lambda(params, inner_expr, ..) => {
                Ok(V::Callable(Callable::closure(self.to_owned().var_store, None, params, HashMap::new(), Ok(inner_expr))))
            }
            E::FunApp(f, args) => {
                let args_evaled = traverse(args, |arg| self.evaluate(arg))?;
                let f = self.evaluate(f)?;
                f.fun_app(args_evaled)
            }
            E::FunKwApp(f, args) => {
                let mut args_evaled: HashMap<String, V> = HashMap::new();
                for (argname, arg) in args {
                    args_evaled.insert(argname, self.evaluate(arg)?);
                }
                let f = self.evaluate(f)?;
                f.fun_kw_app(args_evaled)
            }
            E::Bool(b) => Ok(V::Bool(b)),
            E::Int(n) => Ok(V::Number(Int(n))),
            E::Num(n) => Ok(V::Number(Float(n))),
            E::IntImag(n) => Ok(V::Number(IntCmplx(0, n))),
            E::FloatImag(n) => Ok(V::Number(FloatCmplx(0f64, n))),
            E::Id(x) if x.eq("_") => Ok(V::Any),
            E::Id(x) => {
                if let Some(v) = self.var_store.get(&x) {
                    Ok(v.clone())
                } else {
                    get_builtin(&x).ok_or(EvalError::UndefinedIdentifier(x))
                }
            }
        }
    }

    // Evaluates a slice object. Does not get rid of negative indexing.
    fn evaluate_slice(&self, slice: AstSlice) -> EvalResult<Slice<i32, Option<i32>>> {
        match slice {
            AstSlice::Single(i) => {
                let i = if let V::Number(Int(i)) = self.evaluate(i)? { i }
                        else { return Err(EvalError::TypeMismatch); };
                Ok(Slice::Single(i))
            }
            AstSlice::Range(Some(i), Some(j)) => {
                let i = if let V::Number(Int(i)) = self.evaluate(i)? { i }
                        else { return Err(EvalError::TypeMismatch); };
                let j = if let V::Number(Int(j)) = self.evaluate(j)? { j }
                        else { return Err(EvalError::TypeMismatch); };
                Ok(Slice::Range(i, Some(j)))
            }
            AstSlice::Range(None, Some(j)) => {
                let j = if let V::Number(Int(j)) = self.evaluate(j)? { j }
                        else { return Err(EvalError::TypeMismatch); };
                Ok(Slice::Range(0, Some(j)))
            }
            AstSlice::Range(Some(i), None) => {
                let i = if let V::Number(Int(i)) = self.evaluate(i)? { i }
                        else { return Err(EvalError::TypeMismatch); };
                Ok(Slice::Range(i, None))
            }
            AstSlice::Range(None, None) => {
                Ok(Slice::Range(0, None))
            }
        }
    }
}

impl Slice<i32, Option<i32>> {
    /// Changes negative indexing into normal indexing given the length of the
    /// collection. Also performs bounds checking. The slice returned will
    /// never be of the form Range(from, None).
    pub fn resolve_negative_indices(self, len: usize) -> EvalResult<Slice<usize, usize>> {
        let len: i32 = len as i32;
        match self {
            Slice::Single(s) => {
                let s = if s >= 0 { s } else { len + s };
                if s < 0 || s >= len { return Err(EvalError::OutOfRange); }
                Ok(Slice::Single(s as usize))
            }
            Slice::Range(from, Some(to)) => {
                let from = if from >= 0 { from } else { len + from };
                let to = if to >= 0 { to } else { len + to };
                if from < 0 || from >= len { return Err(EvalError::OutOfRange); }
                if to < 0 || to > len { return Err(EvalError::OutOfRange); }
                if to < from { return Err(EvalError::InvalidSlice); }
                Ok(Slice::Range(from as usize, to as usize))
            }
            Slice::Range(from, None) => {
                let from = if from >= 0 { from } else { len + from };
                if from < 0 || from >= len { return Err(EvalError::OutOfRange); }
                if len < from { return Err(EvalError::InvalidSlice); }
                Ok(Slice::Range(from as usize, len as usize))
            }
        }
    }
}

/// Utility function that maps a fallible function over a vector.
fn traverse<A, F, E, B>(v: Vec<A>, mut f: F) -> Result<Vec<B>, E>
where F: FnMut(A) -> Result<B, E> {
    let mut retv: Vec<B> = Vec::with_capacity(v.len());
    for a in v { retv.push(f(a)?); }
    Ok(retv)
}
