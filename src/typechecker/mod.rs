/* typechecker/mod.rs
 *
 * Defines types and type inference procedures.
 */

pub mod typecheckerror;
mod types;

use std::collections::HashMap;
use crate::ast::{AstArg, AstExpr as E, AstStmt};
use crate::builtins::get_builtin_type;
use typecheckerror::{TypeCheckResult, TypeError};
use types::TAssums;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Num,
    Matrix,
    Bool,
    String,
    List(Box<Type>),
    Tuple,
    TVar(String),
    Func(Vec<Type>, Box<Type>),
    Any,    // used for ?-argument syntax
    Unknown,
}

pub struct TypeEnv {
    var_types: HashMap<String, Type>,
}

impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv{var_types: HashMap::new()}
    }

    /// Type checks expressions within a statement. If the statement is a display,
    /// the type in string form is returned.
    pub fn type_check_stmt(&mut self, stmt: &AstStmt) -> TypeCheckResult<String> {
        match stmt {
            AstStmt::Assign(v, expr) => {
                let mut typ = self.type_check(expr)?;
                typ.normalize_type_var_names();
                self.var_types.insert(v.clone(), typ);
                Ok(String::from(""))
            }
            AstStmt::Display(expr) => {
                let mut typ = self.type_check(expr)?;
                typ.normalize_type_var_names();
                Ok(typ.to_string())
            }
        }
    }

    /// Performs type checking for expressions.
    pub fn type_check(&self, expr: &E) -> TypeCheckResult<Type> {
        self.type_check_helper(expr, &mut TAssums::new())
    }

    /// Performs type checking for expressions.
    /// `ta`: a set of type assumptions for variables, modified as type
    /// refinement happens
    fn type_check_helper(&self, expr: &E, ta: &mut TAssums) -> TypeCheckResult<Type> {
        match expr {
            E::Lambda(params, body) => {
                ta.push_new_frame(&params);
                let bodytype = self.type_check_helper(body, ta)?;
                Ok(Type::Func(ta.pop_frame(), Box::new(bodytype)))
            }
            E::FunApp(f, args) => {
                // type check each of the argument expressions
                for arg in args.iter().rev() {
                    match arg {
                        AstArg::Question => ta.push_type(Type::Any),
                        AstArg::Expr(e) => {
                            let typ = self.type_check_helper(e, ta)?;
                            ta.push_type(typ);
                        }
                    }
                }

                // type check the function expression
                let ftype = self.type_check_helper(f, ta)?;

                // retrieve (possibly) modified argument types
                let mut argtypes: Vec<Type> = Vec::new();
                for _ in 0..args.len() {
                    argtypes.push(ta.pop_type());
                }

                // If partial application is happening (including ?-syntax), partially-curry
                // the function type (e.g. (A,B,C) -> D becomes (B,C) -> (A) -> D).
                // Also, remove the "Any" types from the arg list.
                let type1 = if let Type::Func(mut paramtypes, rettype) = ftype {
                    if paramtypes.len() < argtypes.len() { return Err(TypeError::ArgumentListTooLong); }
                    let mut appliedparams = paramtypes.split_off(paramtypes.len() - argtypes.len());

                    let mut i = 0;
                    while i < appliedparams.len() {
                        if let Type::Any = argtypes[i] {
                            argtypes.remove(i);
                            paramtypes.push(appliedparams.remove(i));
                        } else {
                            i += 1;
                        }
                    }

                    if paramtypes.len() > 0 {
                        Type::Func(appliedparams, Box::new(Type::Func(paramtypes, rettype)))
                    } else {
                        Type::Func(appliedparams, rettype)
                    }
                } else { ftype };

                // Unify the function type and "(argtypes) -> newtypevar"
                // and return the return type.
                let type2 = Type::Func(argtypes, Box::new(Type::TVar(ta.fresh_type_var())));
                if let Type::Func(_, rettype) = ta.unify(type1, type2)? {
                    Ok(*rettype)
                } else { unreachable!() }
            }
            E::Binop(op, lhs, rhs) => {
                let lhs = self.type_check_helper(lhs, ta)?;
                let rhs = self.type_check_helper(rhs, ta)?;
                let typ1 = Type::func2(lhs, rhs, Type::TVar(ta.fresh_type_var()));
                let typ2 = match op.as_str() {
                    "." => Some(Type::Unknown),
                    "+"  => get_builtin_type(&String::from("op+")),
                    "-"  => get_builtin_type(&String::from("op-")),
                    "*"  => get_builtin_type(&String::from("op*")),
                    "/"  => get_builtin_type(&String::from("op/")),
                    "%"  => get_builtin_type(&String::from("op%")),
                    "**" => get_builtin_type(&String::from("op**")),
                    "==" => get_builtin_type(&String::from("op==")),
                    "!=" => get_builtin_type(&String::from("op!=")),
                    "<=" => get_builtin_type(&String::from("op<=")),
                    ">=" => get_builtin_type(&String::from("op>=")),
                    "<"  => get_builtin_type(&String::from("op<")),
                    ">"  => get_builtin_type(&String::from("op>")),
                    "&&" => get_builtin_type(&String::from("op&&")),
                    "||" => get_builtin_type(&String::from("op||")),
                    _ => panic!("Unrecognized binary operator")
                }.unwrap();
                if let Type::Func(_, rettype) = ta.unify(typ1, typ2)? {
                    Ok(*rettype)
                } else { unreachable!() }
            }
            E::List(l) => {
                if l.len() <= 0 { return Ok(Type::list(Type::TVar(ta.fresh_type_var()))); }
                let mut typ = self.type_check_helper(&l[0], ta)?;
                for item in &l[1..] {
                    let typ2 = self.type_check_helper(item, ta)?;
                    typ = ta.unify(typ, typ2)?;
                }
                Ok(Type::list(typ))
            },
            E::Matrix(_, _, _) => Ok(Type::Matrix),
            E::Tuple(_) => Ok(Type::Tuple),
            E::Bool(_) => Ok(Type::Bool),
            E::Str(_) => Ok(Type::String),
            E::Int(_) => Ok(Type::Num),
            E::Num(_) => Ok(Type::Num),
            E::IntImag(_) => Ok(Type::Num),
            E::FloatImag(_) => Ok(Type::Num),
            E::Id(x) => {
                if let Some(t) = ta.get(x) { Ok(t) }
                else if let Some(t) = self.var_types.get(x) {
                    let mut t = t.clone();
                    ta.refresh_type_vars(&mut t);
                    Ok(t)
                }
                else if let Some(mut t) = get_builtin_type(x) { ta.refresh_type_vars(&mut t); Ok(t) }
                else { Err(TypeError::UnknownIdentifier(x.clone())) }
            },
            _ => Ok(Type::Unknown)
        }
    }
}

impl Type {
    pub fn list(t: Type) -> Type { Type::List(Box::new(t)) }
    pub fn var(v: &str) -> Type { Type::TVar(String::from(v)) }
    pub fn func1(arg: Type, ret: Type) -> Type { Type::Func(vec![arg], Box::new(ret)) }
    pub fn func2(arg1: Type, arg2: Type, ret: Type) -> Type { Type::Func(vec![arg1, arg2], Box::new(ret)) }
    pub fn func3(arg1: Type, arg2: Type, arg3: Type, ret: Type) -> Type { Type::Func(vec![arg1, arg2, arg3], Box::new(ret)) }
}