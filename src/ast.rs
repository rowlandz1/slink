/* ast.rs
 *
 * Contains definitions of abstract statements, expressions, and values
 */

use std::collections::HashMap;
use crate::number::Number;
use crate::callable::Callable;

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
    VNumber(Number),
    Bool(bool),
    Matrix(usize, usize, Vec<Number>),  // numrows, numcols, index = row*numcols + col
    List(Vec<SciVal>),
    Tuple(Vec<SciVal>),
    Str(String),
    VCallable(Callable)
}

pub enum Arg {
    Question,
    Val(Box<SciVal>),
}

#[derive(Debug, Clone)]
 pub enum Slice<F, S> {
     Single(F),
     Range(F, S),
 }
