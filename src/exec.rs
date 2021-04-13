/* exec.rs
 *
 * Implements operations for SciValue,
 * Defines Environs and implements execute and evaluate operations for them.
 */

use std::collections::HashMap;
use core::ops;
use crate::ast::*;
use SciVal::*;
use crate::internals;
use crate::error::*;

impl ops::Add<SciVal> for SciVal {
    type Output = EvalResult<SciVal>;
    fn add(self, rhs: SciVal) -> EvalResult<SciVal> {
        match (self, rhs) {
            (Matrix(r1, c1, m1), Matrix(r2, c2, m2)) => {
                if c1 != c2 || r1 != r2 {
                    return Err(EvalError::IncompatibleMatrixShapes);
                }
                let mut ret = vec![0f64; m1.len()];
                for i in 0..ret.len() {
                    ret[i] = m1[i] + m2[i];
                }
                Ok(Matrix(r1, c1, ret))
            }
            (Number(n), Matrix(r, c, m)) |
            (Matrix(r, c, m), Number(n)) => {
                let mut ret = vec![0f64; m.len()];
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
                let mut ret = vec![0f64; r1*c2];
                for r in 0..r1 {
                    for c in 0..c2 {
                        let mut sum = 0f64;
                        for k in 0..c1 {
                            sum += m1[r*c1 + k] * m2[k*c2 + c];
                        }
                        ret[r*c2 + c] = sum;
                    }
                }
                Ok(Matrix(r1, c2, ret))
            }
            (Number(n), Matrix(r, c, m)) |
            (Matrix(r, c, m), Number(n)) => {
                let mut ret = vec![0f64; m.len()];
                for i in 0..ret.len() {
                    ret[i] = m[i] * n;
                }
                Ok(Matrix(r, c, ret))
            }
            (Number(n1), Number(n2)) => Ok(Number(n1 * n2)),
            (List(v), Number(n)) => {
                if n < 0f64 || n.round() != n { return Err(EvalError::OutOfRange); }
                let n = n.round() as usize;
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

impl SciVal {
    pub fn fun_app(self, args: Vec<Arg>) -> EvalResult<SciVal> {
        match self {
            Closure(mut env, mut params, expr, next) => {
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
                        Err(name) => internals::apply_to_internal(&name, env),
                    }?;
                    match next {
                        Some(next) => next.fun_app(vec![Arg::Val(Box::new(result))]),
                        None => Ok(result),
                    }
                } else {
                    Ok(Closure(env, params, expr, next))
                }
            }
            _ => Err(EvalError::TypeMismatch)
        }
    }

    pub fn fun_comp(self, other: SciVal) -> EvalResult<SciVal> {
        if let Closure(env, params, body, next) = self {
            let newnext = match next {
                Some(next) => next.fun_comp(other)?,
                None => other,
            };
            Ok(Closure(env, params, body, Some(Box::new(newnext))))
        } else {
            other.fun_app(vec![Arg::Val(Box::new(self))])
        }
    }
}

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
                    Ok(evaled) => { self.var_store.insert(v, evaled); }
                    Err(e) => println!("{:?}", e),
                }
            }
            AstStmt::Display(e) => {
                match self.evaluate(*e) {
                    Ok(evaled) => println!("{}", evaled.to_string()),
                    Err(e) => println!("{:?}", e)
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
                else if op.eq("-") { lhs + (Number(-1f64) * rhs).unwrap() }
                else if op.eq("*") { lhs * rhs }
                else if op.eq(".") { lhs.fun_comp(rhs) }
                else { panic!("Unrecognized binary operator"); }
            }
            AstExpr::Matrix(r, c, v) => {
                let mut vret: Vec<f64> = vec![];
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
                Ok(Closure(self.to_owned().var_store, params, Ok(inner_expr), None))
            }
            AstExpr::Let(bindings, inner_expr) => {
                for (v, e) in bindings {
                    let e_evaled = self.evaluate(e)?;
                    self.var_store.insert(v, e_evaled);
                }
                self.evaluate(*inner_expr)
            }
            AstExpr::FunApp(f, args) => {
                let f = self.evaluate(*f)?;
                let mut args_evaled: Vec<Arg> = Vec::new();
                for arg in args {
                    args_evaled.push(match arg {
                        AstArg::Question => Arg::Question,
                        AstArg::Expr(e) => Arg::Val(Box::new(self.evaluate(*e)?))
                    })
                }
                f.fun_app(args_evaled)
            }
            AstExpr::Num(n) => Ok(Number(n)),
            AstExpr::Id(x) => {
                if let Some(v) = self.var_store.get(&x) {
                    Ok(v.clone())
                } else {
                    internals::get_internal(x)
                }
            }
        }
    }
}
