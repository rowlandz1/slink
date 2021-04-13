/* ast.rs
 *
 * Contains definitions of abstract statements, expressions, and values
 */

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum AstStmt {
    Assign(String, Box<AstExpr>),
    Display(Box<AstExpr>),
}

#[derive(Debug, Clone)]
pub enum AstExpr {
    Binop(String, Box<AstExpr>, Box<AstExpr>),
    Lambda(Vec<String>, Box<AstExpr>),
    FunApp(Box<AstExpr>, Vec<AstArg>),
    Let(Vec<(String, AstExpr)>, Box<AstExpr>),
    Matrix(usize, usize, Vec<AstExpr>),
    List(Vec<AstExpr>),
    Tuple(Vec<AstExpr>),
    Num(f64),
    Id(String),
}

#[derive(Debug, Clone)]
pub enum AstArg {
    Question,
    Expr(Box<AstExpr>),
}

#[derive(Debug, Clone)]
pub enum SciVal {
    Number(f64),
    Matrix(usize, usize, Vec<f64>),  // numrows, numcols, index = row*numcols + col
    List(Vec<SciVal>),
    Tuple(Vec<SciVal>),
    Closure {
        env: HashMap<String, SciVal>,
        name: Option<String>,
        params: Vec<String>,
        expr: Result<Box<AstExpr>, String>,
        next: Option<Box<SciVal>>,
    }
}

pub enum Arg {
    Question,
    Val(Box<SciVal>),
}
