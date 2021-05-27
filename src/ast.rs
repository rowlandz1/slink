/* ast.rs
 *
 * Defines the abstract syntax tree.
 * The two main constructs are AstStmt and AstExpr.
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
    Unop(String, Box<AstExpr>),
    ListIdx(Box<AstExpr>, AstSlice),
    MatrixIdx(Box<AstExpr>, AstSlice, AstSlice),
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

impl AstExpr {
    pub fn binop      (op: String, lhs: AstExpr, rhs: AstExpr)     -> AstExpr { AstExpr::Binop(op, Box::new(lhs), Box::new(rhs)) }
    pub fn unop       (op: String, e: AstExpr)                     -> AstExpr { AstExpr::Unop(op, Box::new(e)) }
    pub fn list_idx   (e: AstExpr, s: AstSlice)                    -> AstExpr { AstExpr::ListIdx(Box::new(e), s) }
    pub fn matrix_idx (e: AstExpr, s1: AstSlice, s2: AstSlice)     -> AstExpr { AstExpr::MatrixIdx(Box::new(e), s1, s2) }
    pub fn lambda     (args: Vec<String>, e: AstExpr)              -> AstExpr { AstExpr::Lambda(args, Box::new(e)) }
    pub fn fun_app    (f: AstExpr, args: Vec<AstArg>)              -> AstExpr { AstExpr::FunApp(Box::new(f), args) }
    pub fn fun_kw_app (f: AstExpr, args: HashMap<String, AstExpr>) -> AstExpr { AstExpr::FunKwApp(Box::new(f), args) }
    pub fn macro_expr (s: String)                                  -> AstExpr { AstExpr::Macro(s) }
    pub fn let_expr   (v: Vec<(String, AstExpr)>, e: AstExpr)      -> AstExpr { AstExpr::Let(v, Box::new(e)) }
    pub fn matrix     (r: usize, c: usize, v: Vec<AstExpr>)        -> AstExpr { AstExpr::Matrix(r, c, v) }
    pub fn list       (v: Vec<AstExpr>)                            -> AstExpr { AstExpr::List(v) }
    pub fn tuple      (v: Vec<AstExpr>)                            -> AstExpr { AstExpr::Tuple(v) }
    pub fn bool       (b: bool)                                    -> AstExpr { AstExpr::Bool(b) }
    pub fn str        (s: String)                                  -> AstExpr { AstExpr::Str(s) }
    pub fn int        (i: i32)                                     -> AstExpr { AstExpr::Int(i) }
    pub fn num        (i: f64)                                     -> AstExpr { AstExpr::Num(i) }
    pub fn int_imag   (i: i32)                                     -> AstExpr { AstExpr::IntImag(i) }
    pub fn float_imag (i: f64)                                     -> AstExpr { AstExpr::FloatImag(i) }
    pub fn id         (s: String)                                  -> AstExpr { AstExpr::Id(s) }
}
