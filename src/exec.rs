use std::collections::HashMap;
use core::ops;
use crate::parser::{AstStmt, AstExpr};

#[derive(Debug)]
pub enum SciVal {
    Matrix(usize, usize, Vec<f64>),  // numrows, numcols, index = row*numcols + col
    Number(f64),
    Closure(Environ, Vec<String>, AstExpr),
}

use SciVal::*;

impl ToOwned for SciVal {
    type Owned = SciVal;

    fn to_owned(&self) -> SciVal {
        match self {
            Matrix(r, c, v) => {
                Matrix(*r, *c, v.to_vec())
            }
            Number(n) => Number(*n),
            Closure(env, params, inner_expr) => Closure(env.to_owned(), params.to_vec(), inner_expr.to_owned()),
        }
    }
}

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
            (Closure(_,_,_), _) |
            (_, Closure(_,_,_)) => panic!("Error, addition is not defined for closures"),
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
            (Closure(_,_,_), _) |
            (_, Closure(_,_,_)) => panic!("Error, multiplication is not defined for closures"),
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
                    if let (Closure(e1, p1, b1), Closure(e2, p2, b2)) = (lhs, rhs) {
                        if p2.len() != 1 {
                            panic!("Error, second function in function composition must take 1 argument");
                        }
                        todo!();

                    } else { panic!("Error, function composition is not defined for non-closure types") }
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
                Closure(self.to_owned(), params, *inner_expr)
            }
            AstExpr::Let(bindings, inner_expr) => {
                for (v, e) in bindings {
                    let e_evaled = self.evaluate(e);
                    self.var_store.insert(v, e_evaled);
                }
                self.evaluate(*inner_expr)
            }
            AstExpr::FunApp(f, args) => {
                if let Closure(mut env, mut params, inner_expr) = self.evaluate(*f) {
                    if params.len() < args.len() {
                        panic!("Error: arity mismatch. Too many arguments supplied.");
                    }
                    // full argument application
                    else if params.len() == args.len() {
                        for (param, arg) in params.iter().zip(args) {
                            env.var_store.insert(param.to_owned(), self.evaluate(arg));
                        }
                        env.evaluate(inner_expr)
                    }
                    // partial application
                    else {
                        let params2 = params.split_off(params.len() - args.len());
                        for (param, arg) in params2.iter().zip(args) {
                            env.var_store.insert(param.to_owned(), self.evaluate(arg));
                        }
                        Closure(env, params, inner_expr)
                    }
                } else { panic!("Error, cannot apply arguments to a non-closure value") }
            }
            AstExpr::Num(n) => Number(n),
            AstExpr::Id(x) => {
                let val = self.var_store.get(&x).expect("Error, x not defined");
                val.to_owned()
            }
        }
    }
}
