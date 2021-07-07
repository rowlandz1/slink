/* ast.rs
 *
 * Defines the abstract syntax tree.
 * The two main constructs are Stmt and Expr.
 */

use std::collections::HashMap;
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct StmtA {
    pub start: (usize, usize),  // (lineno from 1, col from 0)
    pub end: (usize, usize),
    pub stmt: Box<Stmt>
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Assign(String, ExprA),
    Display(ExprA),
}

#[derive(Debug, Clone)]
pub struct ExprA {
    pub start: (usize, usize),
    pub end: (usize, usize),
    pub expr: Box<Expr>
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binop(String, ExprA, ExprA),
    Unop(String, ExprA),
    ListIdx(ExprA, AstSlice),
    MatrixIdx(ExprA, AstSlice, AstSlice),
    /// `Lambda(argnames, expr, typeparams, paramtypes, rettype)`
    Lambda(Vec<String>, ExprA, Vec<String>, Vec<Type>, Type),
    FunApp(ExprA, Vec<ExprA>),
    FunKwApp(ExprA, HashMap<String, ExprA>),
    Matrix(usize, usize, Vec<ExprA>),
    List(Vec<ExprA>),
    Tuple(Vec<ExprA>),
    Bool(bool),
    Str(String),
    Int(i32),
    Num(f64),
    IntImag(i32),
    FloatImag(f64),
    Id(String),
}

#[derive(Debug, Clone)]
pub enum AstSlice {
    Single(ExprA),
    Range(Option<ExprA>, Option<ExprA>),
}
