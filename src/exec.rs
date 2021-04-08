use std::collections::HashMap;
use core::ops;
use crate::ast::{AstStmt, AstExpr, SciVal};
use SciVal::*;
use crate::internals;

impl ops::Add<SciVal> for SciVal {
    type Output = SciVal;
    fn add(self, rhs: SciVal) -> SciVal {
        match (self, rhs) {
            (Matrix(r1, c1, m1), Matrix(r2, c2, m2)) => {
                if c1 != c2 || r1 != r2 {
                    panic!("Error, cannot add matrices of different shapes");
                }
                let mut ret = vec![0f64; m1.len()];
                for i in 0..ret.len() {
                    ret[i] = m1[i] + m2[i];
                }
                Matrix(r1, c1, ret)
            }
            (Number(n), Matrix(r, c, m)) |
            (Matrix(r, c, m), Number(n)) => {
                let mut ret = vec![0f64; m.len()];
                for i in 0..ret.len() {
                    ret[i] = m[i] + n;
                }
                Matrix(r, c, ret)
            }
            (Number(n1), Number(n2)) => Number(n1 + n2),
            _ => panic!("Error, addition is not defined for this type"),
        }
    }
}

impl ops::Mul<SciVal> for SciVal {
    type Output = SciVal;

    fn mul(self, rhs: SciVal) -> SciVal {
        match (self, rhs) {
            (Matrix(r1, c1, m1), Matrix(r2, c2, m2)) => {
                if c1 != r2 {
                    panic!("Error, attempted to multiply matrices of incompatible sizes");
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
                Matrix(r1, c2, ret)
            }
            (Number(n), Matrix(r, c, m)) |
            (Matrix(r, c, m), Number(n)) => {
                let mut ret = vec![0f64; m.len()];
                for i in 0..ret.len() {
                    ret[i] = m[i] * n;
                }
                Matrix(r, c, ret)
            }
            (Number(n1), Number(n2)) => Number(n1 * n2),
            _ => panic!("Error, multiplication is not defined for this type"),
        }
    }
}

impl SciVal {
    fn fun_app(self, mut args: Vec<SciVal>) -> SciVal {
        match self {
            Closure(mut env, mut params, expr) => {
                if params.len() < args.len() {
                    panic!("Error: arity mismatch. Too many arguments supplied.");
                }
                // full argument application
                else if params.len() == args.len() {
                    for (param, arg) in params.iter().zip(args) {
                        env.insert(param.to_owned(), arg);
                    }
                    Environ::from_map(env).evaluate(*expr)
                }
                // partial application
                else {
                    let params2 = params.split_off(params.len() - args.len());
                    for (param, arg) in params2.iter().zip(args) {
                        env.insert(param.to_owned(), arg);
                    }
                    Closure(env, params, expr)
                }
            }
            Comclos(cls1, cls2) => {
                let inter_result = cls1.fun_app(args);
                cls2.fun_app(vec![inter_result])
            }
            Internal(s, mut a) => {
                args.append(&mut a);
                internals::apply_to_internal(&s, args).unwrap()
            }
            _ => panic!("Cannot apply arguments to this object"),
        }
    }
}

#[derive(Debug)]
pub struct Environ {
    var_store: HashMap<String, SciVal>,
}

impl ToOwned for Environ {
    type Owned = Environ;

    fn to_owned(&self) -> Environ {
        let mut new_environ = Environ::new();
        for (key, val) in self.var_store.iter() {
            new_environ.var_store.insert(key.to_owned(), val.to_owned());
        }
        new_environ
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
                let evaled = self.evaluate(*e);
                self.var_store.insert(v, evaled);
            }
            AstStmt::Display(e) => {
                let evaled = self.evaluate(*e);
                println!("{:?}", evaled);
            }
        }
    }

    pub fn evaluate(&mut self, expr: AstExpr) -> SciVal {
        match expr {
            AstExpr::Binop(op, lhs, rhs) => {
                let lhs = self.evaluate(*lhs);
                let rhs = self.evaluate(*rhs);
                if op.eq("+") { lhs + rhs }
                else if op.eq("-") { lhs + (Number(-1f64) * rhs) }
                else if op.eq("*") { lhs * rhs }
                else if op.eq(".") {
                    match lhs {
                        Closure(_,_,_) | Comclos(_,_) => Comclos(Box::new(lhs), Box::new(rhs)),
                        _ => rhs.fun_app(vec![lhs]),
                    }
                }
                else { panic!("Unrecognized binary operator"); }
            }
            AstExpr::Matrix(r, c, v) => {
                let mut vret: Vec<f64> = vec![];
                for subexpr in v {
                    if let Number(n) = self.evaluate(subexpr) {
                        vret.push(n);
                    } else { panic!("Error! Non-numerical value in a matrix"); }
                }
                Matrix(r, c, vret)
            }
            AstExpr::Lambda(params, inner_expr) => {
                Closure(self.to_owned().var_store, params, inner_expr)
            }
            AstExpr::Let(bindings, inner_expr) => {
                for (v, e) in bindings {
                    let e_evaled = self.evaluate(e);
                    self.var_store.insert(v, e_evaled);
                }
                self.evaluate(*inner_expr)
            }
            AstExpr::FunApp(f, args) => {
                let f = self.evaluate(*f);
                let args_evaled = args.into_iter().map(|x| self.evaluate(x)).collect();
                f.fun_app(args_evaled)
            }
            AstExpr::Num(n) => Number(n),
            AstExpr::Id(x) => {
                if let Some(v) = self.var_store.get(&x) {
                    v.clone()
                } else {
                    Internal(x, Vec::new())
                }
            }
        }
    }
}
