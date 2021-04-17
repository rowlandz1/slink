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
                    Ok(Closure{env, params, expr, next, ..}) => {
                        let name = Some(v.clone());
                        self.var_store.insert(v, Closure{env, name, params, expr, next});
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
                else if op.eq(".") { lhs.fun_comp(rhs) }
                else if op.eq("==") { lhs.equals(&rhs) }
                else if op.eq("!=") { lhs.not_equals(&rhs) }
                else if op.eq("&&") { lhs.logical_and(rhs) }
                else if op.eq("||") {lhs.logical_or(rhs) }
                else { panic!("Unrecognized binary operator"); }
            }
            AstExpr::Unop(op, inner) => {
                let inner = self.evaluate(*inner)?;
                if op.eq("-") { -inner }
                else { panic!("Unrecognized unary operator"); }
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
            AstExpr::Bool(b) => Ok(Bool(b)),
            AstExpr::Int(n) => Ok(Number(number::Number::Int(n))),
            AstExpr::Num(n) => Ok(Number(number::Number::Float(n))),
            AstExpr::IntImag(n) => Ok(Number(number::Number::IntCmplx(0, n))),
            AstExpr::FloatImag(n) => Ok(Number(number::Number::FloatCmplx(0f64, n))),
            AstExpr::Macro(name) => Ok(Macro(name)),
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
