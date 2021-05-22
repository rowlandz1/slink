/* types.rs
 *
 * Type definitions and type-checking algorithm
 */

use crate::ast::{AstExpr, AstExprTree as E};
use crate::error::TypeCheckResult;

#[derive(Debug, Clone)]
pub enum Type {
    Num,
    Matrix,
    Bool,
    String,
    List,
    Tuple,
    Forall(String),
    Func(Vec<Type>, Box<Type>),
    Unknown,
}

impl AstExpr {
    /// Performs type checking and type annotation for expressions
    pub fn type_check(&mut self) -> TypeCheckResult<()> {
        self.typ = self.tree.type_check()?;
        Ok(())
    }
}

impl E {
    pub fn type_check(&self) -> TypeCheckResult<Type> {
        let typ = match self {
            E::Matrix(_, _, _) => Type::Matrix,
            E::List(_) => Type::List,
            E::Tuple(_) => Type::Tuple,
            E::Bool(_) => Type::Bool,
            E::Str(_) => Type::String,
            E::Int(_) => Type::Num,
            E::Num(_) => Type::Num,
            E::IntImag(_) => Type::Num,
            E::FloatImag(_) => Type::Num,
            E::Id(_) => Type::Forall(String::from("A")),
            _ => Type::Unknown
        };
        Ok(typ)
    }
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Num => String::from("Num"),
            Type::Matrix => String::from("Matrix"),
            Type::Bool => String::from("Bool"),
            Type::String => String::from("String"),
            Type::List => String::from("List"),
            Type::Tuple => String::from("Tuple"),
            Type::Forall(s) => String::from(s),
            Type::Func(v, r) => {
                let params: Vec<String> = v.iter().map(|t| t.to_string()).collect();
                format!("({}) -> {}", params.join(", "), r.to_string())
            }
            Type::Unknown => String::from("Unknown"),
        }
    }
}
