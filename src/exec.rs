/* exec.rs
 *
 * Defines Environs and implements execute and evaluate operations for them.
 */

use std::collections::HashMap;
use crate::ast::*;
use SciVal::*;
use crate::internals;
use crate::number;
use crate::error::*;

#[derive(Debug)]
pub struct Environ {
    pub var_store: HashMap<String, SciVal>,
}

impl Clone for Environ {
    fn clone(&self) -> Environ {
        Environ::from_map(self.var_store.clone())
    }
}

impl Environ {
    pub fn new() -> Environ {
        Environ {var_store: HashMap::new()}
    }

    pub fn from_map(m: HashMap<String, SciVal>) -> Environ {
        Environ {var_store: m}
    }

    pub fn execute(&mut self, stmt: AstStmt) {
        match stmt {
            AstStmt::Assign(v, e) => {
                match self.evaluate(*e) {
                    Ok(Closure{env, params, app, expr, next, ..}) => {
                        let name = Some(v.clone());
                        self.var_store.insert(v, Closure{env, name, params, app, expr, next});
                    }
                    Ok(evaled) => { self.var_store.insert(v, evaled); }
                    Err(e) => eprintln!("{}", e.to_string()),
                }
            }
            AstStmt::Display(e) => {
                match self.evaluate(*e) {
                    Ok(evaled) => println!("{}", evaled.to_string()),
                    Err(e) => eprintln!("{}", e.to_string())
                }
            }
        }
    }

    pub fn evaluate(&mut self, expr: AstExpr) -> EvalResult<SciVal> {
        match expr {
            AstExpr::Binop(op, lhs, rhs) => {
                let lhs = self.evaluate(*lhs)?;
                let rhs = self.evaluate(*rhs)?;
                if op.eq("+") { lhs + rhs }
                else if op.eq("-") { lhs - rhs }
                else if op.eq("*") { lhs * rhs }
                else if op.eq("/") { lhs * rhs.inv()? }
                else if op.eq("%") { lhs % rhs }
                else if op.eq("**") { lhs.pow(rhs) }
                else if op.eq(".") { lhs.fun_comp(rhs) }
                else if op.eq("==") { lhs.equals(&rhs) }
                else if op.eq("!=") { lhs.not_equals(&rhs) }
                else if op.eq("<=") { lhs.le(&rhs) }
                else if op.eq(">=") { lhs.ge(&rhs) }
                else if op.eq("<") { lhs.lt(&rhs) }
                else if op.eq(">") { lhs.gt(&rhs) }
                else if op.eq("&&") { lhs.logical_and(rhs) }
                else if op.eq("||") {lhs.logical_or(rhs) }
                else { panic!("Unrecognized binary operator"); }
            }
            AstExpr::Unop(op, inner) => {
                let inner = self.evaluate(*inner)?;
                if op.eq("-") { -inner }
                else { panic!("Unrecognized unary operator"); }
            }
            AstExpr::ListIndex(e, slice) => {
                self.evaluate(*e)?.list_slice(self.evaluate_slice(slice)?)
            }
            AstExpr::MatrixIndex(e, rslice, cslice) => {
                let rslice = self.evaluate_slice(rslice)?;
                let cslice = self.evaluate_slice(cslice)?;
                self.evaluate(*e)?.matrix_slice(rslice, cslice)
            }
            AstExpr::Matrix(r, c, v) => {
                let mut vret: Vec<number::Number> = vec![];
                for subexpr in v {
                    if let Number(n) = self.evaluate(subexpr)? {
                        vret.push(n);
                    } else { return Err(EvalError::TypeMismatch); }
                }
                Ok(Matrix(r, c, vret))
            }
            AstExpr::List(v) => {
                let mut newv: Vec<SciVal> = Vec::new();
                for x in v { newv.push(self.evaluate(x)?); }
                Ok(List(newv))
            }
            AstExpr::Tuple(v) => {
                let mut newv: Vec<SciVal> = Vec::new();
                for x in v { newv.push(self.evaluate(x)?); }
                Ok(Tuple(newv))
            }
            AstExpr::Str(s) => Ok(Str(s)),
            AstExpr::Lambda(params, inner_expr) => {
                Ok(Closure{
                    env: self.to_owned().var_store,
                    name: None,
                    params,
                    app: HashMap::new(),
                    expr: Ok(inner_expr),
                    next: None
                })
            }
            AstExpr::Let(bindings, inner_expr) => {
                for (v, e) in bindings {
                    let e_evaled = self.evaluate(e)?;
                    self.var_store.insert(v, e_evaled);
                }
                self.evaluate(*inner_expr)
            }
            AstExpr::FunApp(f, args) => {
                let mut args_evaled: Vec<Arg> = Vec::new();
                for arg in args {
                    args_evaled.push(match arg {
                        AstArg::Question => Arg::Question,
                        AstArg::Expr(e) => Arg::Val(Box::new(self.evaluate(*e)?))
                    })
                }
                let f = self.evaluate(*f)?;
                f.fun_app(args_evaled)
            }
            AstExpr::FunKwApp(f, args) => {
                let mut args_evaled: HashMap<String, SciVal> = HashMap::new();
                for (argname, arg) in args {
                    args_evaled.insert(argname, self.evaluate(arg)?);
                }
                let f = self.evaluate(*f)?;
                f.fun_kw_app(args_evaled)
            }
            AstExpr::Bool(b) => Ok(Bool(b)),
            AstExpr::Int(n) => Ok(Number(number::Number::Int(n))),
            AstExpr::Num(n) => Ok(Number(number::Number::Float(n))),
            AstExpr::IntImag(n) => Ok(Number(number::Number::IntCmplx(0, n))),
            AstExpr::FloatImag(n) => Ok(Number(number::Number::FloatCmplx(0f64, n))),
            AstExpr::Macro(name) => Ok(Macro(name, None)),
            AstExpr::Id(x) => {
                if let Some(v) = self.var_store.get(&x) {
                    Ok(v.clone())
                } else {
                    internals::get_internal(x)
                }
            }
        }
    }

    // Evaluates a slice object. Does not get rid of negative indexing.
    fn evaluate_slice(&mut self, slice: AstSlice) -> EvalResult<Slice<i32, Option<i32>>> {
        match slice {
            AstSlice::Single(i) => {
                let i = if let Number(number::Number::Int(i)) = self.evaluate(*i)? { i }
                        else { return Err(EvalError::TypeMismatch); };
                Ok(Slice::Single(i))
            }
            AstSlice::Range(Some(i), Some(j)) => {
                let i = if let Number(number::Number::Int(i)) = self.evaluate(*i)? { i }
                        else { return Err(EvalError::TypeMismatch); };
                let j = if let Number(number::Number::Int(j)) = self.evaluate(*j)? { j }
                        else { return Err(EvalError::TypeMismatch); };
                Ok(Slice::Range(i, Some(j)))
            }
            AstSlice::Range(None, Some(j)) => {
                let j = if let Number(number::Number::Int(j)) = self.evaluate(*j)? { j }
                        else { return Err(EvalError::TypeMismatch); };
                Ok(Slice::Range(0, Some(j)))
            }
            AstSlice::Range(Some(i), None) => {
                let i = if let Number(number::Number::Int(i)) = self.evaluate(*i)? { i }
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
    // Changes negative indexing into normal indexing given the length of the
    // collection. Also performs bounds checking. The slice returned will
    // never be of the form Range(from, None).
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
