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
    ListIndex(Box<AstExpr>, AstSlice),
    MatrixIndex(Box<AstExpr>, AstSlice, AstSlice),
    Lambda(Vec<String>, Box<AstExpr>),
    FunApp(Box<AstExpr>, Vec<AstArg>),
    FunKwApp(Box<AstExpr>, HashMap<String, AstExpr>),
    Macro(String),
    Let(Vec<(String, AstExpr)>, Box<AstExpr>),
    Matrix(usize, usize, Vec<AstExpr>),
    List(Vec<AstExpr>),
    Tuple(Vec<AstExpr>),
    Bool(bool),
    Str(String),
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
pub enum AstSlice {
    Single(Box<AstExpr>),
    Range(Option<Box<AstExpr>>, Option<Box<AstExpr>>),
}

#[derive(Debug, Clone)]
pub enum SciVal {
    Number(number::Number),
    Bool(bool),
    Matrix(usize, usize, Vec<number::Number>),  // numrows, numcols, index = row*numcols + col
    List(Vec<SciVal>),
    Tuple(Vec<SciVal>),
    Str(String),
    Closure {
        env: HashMap<String, SciVal>,           // environment in which closure was defined
        name: Option<String>,                   // closure name (for error tracing and recursion)
        params: Vec<String>,                    // unapplied parameter list
        app: HashMap<String, SciVal>,           // applied parameters
        expr: Result<Box<AstExpr>, String>,     // inner expression (or string for internal functions)
        next: Option<Box<SciVal>>,              // if Some(v), then this closure is the first in a composition
    },
    Macro(String, Option<Box<SciVal>>),         // name, next
}

pub enum Arg {
    Question,
    Val(Box<SciVal>),
}

#[derive(Debug)]
 pub enum Slice {
     Single(usize),
     Range(usize, usize),
 }
