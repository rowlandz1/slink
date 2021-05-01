/* exec.rs
 *
 * Defines Environs and implements execute and evaluate operations for them.
 */

use std::collections::HashMap;
use crate::ast::*;
use SciVal::*;
use crate::internals;
use crate::number;
use crate::matrix;
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
                let mut v = if let List(l) = self.evaluate(*e)? { l }
                            else { return Err(EvalError::TypeMismatch); };
                match self.evaluate_slice(slice, v.len())? {
                    Slice::Single(i) => Ok(v.remove(i)),
                    Slice::Range(i, j) => {
                        v.truncate(j);
                        Ok(List(v.split_off(i)))
                    }
                }
            }
            AstExpr::MatrixIndex(e, rslice, cslice) => {
                let (r, c, v) = if let Matrix(r, c, v) = self.evaluate(*e)? { (r, c, v) }
                                else { return Err(EvalError::TypeMismatch); };

                let rslice = self.evaluate_slice(rslice, r)?;
                let cslice = self.evaluate_slice(cslice, c)?;
                if let (Slice::Single(i), Slice::Single(j)) = (&rslice, &cslice) {
                    Ok(Number(v[i*r + j]))
                } else {
                    let (r, c, v) = matrix::matrix_slice(r, c, v.as_slice(), rslice, cslice);
                    Ok(Matrix(r, c, v))
                }
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

    // Evaluates a slice object. "len" is used for negative indexing purposes.
    pub fn evaluate_slice(&mut self, slice: AstSlice, len: usize) -> EvalResult<Slice> {
        match slice {
            AstSlice::Single(i) => {
                let i = if let Number(number::Number::Int(i)) = self.evaluate(*i)? { i }
                        else { return Err(EvalError::TypeMismatch); };
                let i = if i >= 0 { i } else { len as i32 + i };
                if i < 0 || i >= len as i32 { return Err(EvalError::OutOfRange); }
                Ok(Slice::Single(i as usize))
            }
            AstSlice::Range(Some(i), Some(j)) => {
                let i = if let Number(number::Number::Int(i)) = self.evaluate(*i)? { i }
                        else { return Err(EvalError::TypeMismatch); };
                let i = if i >= 0 { i } else { len as i32 + i };
                if i < 0 || i >= len as i32 { return Err(EvalError::InvalidSlice); }
                let j = if let Number(number::Number::Int(j)) = self.evaluate(*j)? { j }
                        else { return Err(EvalError::TypeMismatch); };
                let j = if j >= 0 { j } else { len as i32 + j };
                if j < 0 || j >= len as i32 { return Err(EvalError::InvalidSlice); }
                if j < i { return Err(EvalError::InvalidSlice); }
                Ok(Slice::Range(i as usize, j as usize))
            }
            AstSlice::Range(None, Some(j)) => {
                let j = if let Number(number::Number::Int(j)) = self.evaluate(*j)? { j }
                        else { return Err(EvalError::TypeMismatch); };
                let j = if j >= 0 { j } else { len as i32 + j };
                if j < 0 || j >= len as i32 { return Err(EvalError::InvalidSlice); }
                Ok(Slice::Range(0, j as usize))
            }
            AstSlice::Range(Some(i), None) => {
                let i = if let Number(number::Number::Int(i)) = self.evaluate(*i)? { i }
                        else { return Err(EvalError::TypeMismatch); };
                let i = if i >= 0 { i } else { len as i32 + i };
                if i < 0 || i >= len as i32 { return Err(EvalError::InvalidSlice); }
                Ok(Slice::Range(i as usize, len))
            }
            AstSlice::Range(None, None) => {
                Ok(Slice::Range(0, len))
            }
        }
    }
}
