/* ast.rs
 *
 * Contains definitions of abstract statements, expressions, and values
 */

use std::collections::HashMap;
use crate::number::Number;
use crate::callable::Callable;
use crate::types::Type;

#[derive(Debug, Clone)]
pub enum AstStmt {
    Assign(String, Box<AstExpr>),
    Display(Box<AstExpr>),
}

#[derive(Debug, Clone)]
pub struct AstExpr {
    pub tree: AstExprTree,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum AstExprTree {
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

impl AstExpr {
    pub fn binop(op: String, lhs: AstExpr, rhs: AstExpr) -> AstExpr { AstExpr{tree: AstExprTree::Binop(op, Box::new(lhs), Box::new(rhs)), typ: Type::Unknown} }
    pub fn unop(op: String, e: AstExpr) -> AstExpr { AstExpr{tree: AstExprTree::Unop(op, Box::new(e)), typ: Type::Unknown} }
    pub fn list_index(e: AstExpr, s: AstSlice) -> AstExpr { AstExpr{tree: AstExprTree::ListIndex(Box::new(e), s), typ: Type::Unknown} }
    pub fn matrix_index(e: AstExpr, s1: AstSlice, s2: AstSlice) -> AstExpr { AstExpr{tree: AstExprTree::MatrixIndex(Box::new(e), s1, s2), typ: Type::Unknown} }
    pub fn lambda(args: Vec<String>, e: AstExpr) -> AstExpr { AstExpr{tree: AstExprTree::Lambda(args, Box::new(e)), typ: Type::Unknown} }
    pub fn fun_app(f: AstExpr, args: Vec<AstArg>) -> AstExpr { AstExpr{tree: AstExprTree::FunApp(Box::new(f), args), typ: Type::Unknown} }
    pub fn fun_kw_app(f: AstExpr, args: HashMap<String, AstExpr>) -> AstExpr { AstExpr{tree: AstExprTree::FunKwApp(Box::new(f), args), typ: Type::Unknown} }
    pub fn macro_expr(s: String) -> AstExpr { AstExpr{tree: AstExprTree::Macro(s), typ: Type::Unknown} }
    pub fn let_expr(v: Vec<(String, AstExpr)>, e: AstExpr) -> AstExpr { AstExpr{tree: AstExprTree::Let(v, Box::new(e)), typ: Type::Unknown} }
    pub fn matrix(r: usize, c: usize, v: Vec<AstExpr>) -> AstExpr { AstExpr{tree: AstExprTree::Matrix(r, c, v), typ: Type::Unknown} }
    pub fn list(v: Vec<AstExpr>) -> AstExpr { AstExpr{tree: AstExprTree::List(v), typ: Type::Unknown} }
    pub fn tuple(v: Vec<AstExpr>) -> AstExpr { AstExpr{tree: AstExprTree::Tuple(v), typ: Type::Unknown} }
    pub fn bool(b: bool) -> AstExpr { AstExpr{tree: AstExprTree::Bool(b), typ: Type::Unknown} }
    pub fn str(s: String) -> AstExpr { AstExpr{tree: AstExprTree::Str(s), typ: Type::Unknown} }
    pub fn int(i: i32) -> AstExpr { AstExpr{tree: AstExprTree::Int(i), typ: Type::Unknown} }
    pub fn num(i: f64) -> AstExpr { AstExpr{tree: AstExprTree::Num(i), typ: Type::Unknown} }
    pub fn int_imag(i: i32) -> AstExpr { AstExpr{tree: AstExprTree::IntImag(i), typ: Type::Unknown} }
    pub fn float_imag(i: f64) -> AstExpr { AstExpr{tree: AstExprTree::FloatImag(i), typ: Type::Unknown} }
    pub fn id(s: String) -> AstExpr { AstExpr{tree: AstExprTree::Id(s), typ: Type::Unknown} }
}
