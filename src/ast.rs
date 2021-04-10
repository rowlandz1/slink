/* ast.rs
 *
 * Contains definitions of abstract statements, expressions, and values
 */

use std::collections::HashMap;

#[derive(Debug)]
pub enum AstStmt {
    Assign(String, Box<AstExpr>),
    Display(Box<AstExpr>),
}

#[derive(Debug)]
pub enum AstExpr {
    Binop(String, Box<AstExpr>, Box<AstExpr>),
    Lambda(Vec<String>, Box<AstExpr>),
    FunApp(Box<AstExpr>, Vec<AstArg>),
    Let(Vec<(String, AstExpr)>, Box<AstExpr>),
    Matrix(usize, usize, Vec<AstExpr>),
    Num(f64),
    Id(String),
}

#[derive(Debug)]
pub enum AstArg {
    Question,
    Expr(Box<AstExpr>),
}

#[derive(Debug)]
pub enum SciVal {
    Matrix(usize, usize, Vec<f64>),  // numrows, numcols, index = row*numcols + col
    Number(f64),
    Closure(HashMap<String, SciVal>, Vec<String>, Box<AstExpr>),  // env, params, expr
    Comclos(Box<SciVal>, Box<SciVal>),
    Internal(HashMap<usize, SciVal>, Vec<usize>, String),      // env, params, name
}

pub enum Arg {
    Question,
    Val(Box<SciVal>),
}

impl Clone for AstExpr {
    fn clone(&self) -> AstExpr {
        match self {
            AstExpr::Binop(op, lhs, rhs) => AstExpr::Binop(op.clone(), lhs.clone(), rhs.clone()),
            AstExpr::Lambda(params, inner_expr) => AstExpr::Lambda(params.to_vec(), inner_expr.clone()),
            AstExpr::FunApp(f, args) => AstExpr::FunApp(f.clone(), args.to_vec()),
            AstExpr::Let(bindings, e) => AstExpr::Let(bindings.clone(), e.clone()),
            AstExpr::Matrix(r, c, v) => AstExpr::Matrix(*r, *c, v.to_vec()),
            AstExpr::Num(n) => AstExpr::Num(*n),
            AstExpr::Id(x) => AstExpr::Id(x.clone()),
        }
    }
}

impl Clone for AstArg {
    fn clone(&self) -> AstArg {
        match self {
            AstArg::Question => AstArg::Question,
            AstArg::Expr(e) => AstArg::Expr(e.clone()),
        }
    }
}

impl Clone for SciVal {
    fn clone(&self) -> SciVal {
        match self {
            SciVal::Matrix(r, c, v) => SciVal::Matrix(*r, *c, v.to_vec()),
            SciVal::Number(n) => SciVal::Number(*n),
            SciVal::Closure(env, params, inner_expr) => {
                SciVal::Closure(env.clone(), params.to_vec(), inner_expr.clone())
            }
            SciVal::Comclos(cls1, cls2) => SciVal::Comclos(cls1.clone(), cls2.clone()),
            SciVal::Internal(env, params, name) => SciVal::Internal(env.clone(), params.clone(), name.clone()),
        }
    }
}
