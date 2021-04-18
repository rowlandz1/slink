/* ast.rs
 *
 * Contains definitions of abstract statements, expressions, and values
 */

use std::collections::HashMap;
use crate::number;

#[derive(Debug, Clone)]
pub enum AstStmt {
    Assign(String, Box<AstExpr>),
    Display(Box<AstExpr>),
}

#[derive(Debug, Clone)]
pub enum AstExpr {
    Binop(String, Box<AstExpr>, Box<AstExpr>),
    Unop(String, Box<AstExpr>),
    Lambda(Vec<String>, Box<AstExpr>),
    FunApp(Box<AstExpr>, Vec<AstArg>),
    Macro(String),
    Let(Vec<(String, AstExpr)>, Box<AstExpr>),
    Matrix(usize, usize, Vec<AstExpr>),
    List(Vec<AstExpr>),
    Tuple(Vec<AstExpr>),
    Bool(bool),
    Int(i32),
    Num(f64),
    IntImag(i32),
    FloatImag(f64),
    Id(String),
}

#[derive(Debug, Clone)]
pub enum AstArg {
    Question,
    Expr(Box<AstExpr>),
}

#[derive(Debug, Clone)]
pub enum SciVal {
    Number(number::Number),
    Bool(bool),
    Matrix(usize, usize, Vec<number::Number>),  // numrows, numcols, index = row*numcols + col
    List(Vec<SciVal>),
    Tuple(Vec<SciVal>),
    Closure {
        env: HashMap<String, SciVal>,
        name: Option<String>,
        params: Vec<String>,
        expr: Result<Box<AstExpr>, String>,
        next: Option<Box<SciVal>>,
    },
    Macro(String, Option<Box<SciVal>>),    // name, next
}

pub enum Arg {
    Question,
    Val(Box<SciVal>),
}
