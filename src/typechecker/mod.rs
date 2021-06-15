/* typechecker/mod.rs
 *
 * Defines types and type inference procedures.
 */

pub mod typecheckerror;
mod types;

use std::collections::HashMap;
use crate::ast::{AstExpr as E, AstStmt};
use crate::builtins::get_builtin_type;
use typecheckerror::{TypeCheckResult, TypeError};
use types::TAssums;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Num,
    Mat,
    Bool,
    Str,
    List(Box<Type>),
    Tuple(Vec<Type>),
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
                let typ = self.type_check(expr)?.normalize_type_var_names();
                self.var_types.insert(v.clone(), typ);
                Ok(String::from(""))
            }
            AstStmt::Display(expr) => {
                let typ = self.type_check(expr)?.normalize_type_var_names();
                Ok(typ.to_string())
            }
        }
    }

    /// Essentially a "get type" function for expressions, but examination of the
    /// expression can result in type refinement of other expressions. For example,
    /// `x + 1` must be a `Num`. So `(x, x + 1)` must be `(Num, Num)`, not `(A, Num)`
    pub fn type_check(&self, expr: &E) -> TypeCheckResult<Type> {
        self.type_check_helper(expr, &mut TAssums::new())
    }

    /// Performs type checking for expressions.
    /// `ta`: a set of type assumptions for variables, modified as type
    /// refinement happens
    fn type_check_helper(&self, expr: &E, ta: &mut TAssums) -> TypeCheckResult<Type> {
        match expr {
            E::Lambda(params, body, paramtypes, rettype) => {
                let paramtypes = paramtypes.iter().map(|t| match t {
                    Type::Any => Type::TVar(ta.fresh_type_var()),
                    Type::TVar(_) => panic!("Shouldn't be a type var"),
                    t => t.clone(),
                }).collect::<Vec<Type>>();
                ta.push_new_frame(params.clone(), paramtypes);
                let bodytype = self.type_check_helper(body, ta)?;
                let bodytype = ta.unify(rettype.clone(), bodytype)?;
                Ok(Type::Func(ta.pop_frame(), Box::new(bodytype)))
            }
            E::FunApp(f, args) => {
                // type check each of the argument expressions
                for arg in args.iter() {
                    let typ = self.type_check_helper(arg, ta)?;
                    ta.push_type(typ);
                }

                // type check the function expression
                let ftype = self.type_check_helper(f, ta)?;

                // retrieve (possibly) modified argument types
                let mut argtypes = ta.pop_types(args.len());

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
            E::FunKwApp(_f, _args) => Ok(Type::Unknown),
            E::Macro(_) => Ok(Type::Unknown),
            E::ListIdx(_expr, _slice) => Ok(Type::Unknown),
            E::MatrixIdx(_expr, _slice1, _slice2) => Ok(Type::Unknown),
            E::Binop(op, lhs, rhs) => {
                let lhs = self.type_check_helper(lhs, ta)?;
                let rhs = self.type_check_helper(rhs, ta)?;
                let typ2 = match op.as_str() {
                    "."  => return self.type_check_dot(lhs, rhs, ta),
                    "$"  => return self.type_check_dollar(lhs, rhs, ta),
                    "+"  => get_builtin_type("op+"),
                    "-"  => get_builtin_type("op-"),
                    "*"  => get_builtin_type("op*"),
                    "/"  => get_builtin_type("op/"),
                    "%"  => get_builtin_type("op%"),
                    "**" => get_builtin_type("op**"),
                    "==" => get_builtin_type("op=="),
                    "!=" => get_builtin_type("op!="),
                    "<=" => get_builtin_type("op<="),
                    ">=" => get_builtin_type("op>="),
                    "<"  => get_builtin_type("op<"),
                    ">"  => get_builtin_type("op>"),
                    "&&" => Some(Type::func2(Type::Bool, Type::Bool, Type::Bool)),
                    "||" => Some(Type::func2(Type::Bool, Type::Bool, Type::Bool)),
                    _ => panic!("Unrecognized binary operator")
                }.unwrap().refresh_type_vars(ta);
                let typ1 = Type::func2(lhs, rhs, Type::TVar(ta.fresh_type_var()));
                if let Type::Func(_, rettype) = ta.unify(typ1, typ2)? {
                    Ok(*rettype)
                } else { unreachable!() }
            }
            E::Unop(op, expr) => {
                let typ1 = self.type_check_helper(expr, ta)?;
                let typ2 = match op.as_str() {
                    "-" => Type::func1(Type::Num, Type::Num),
                    "!" => Type::func1(Type::Bool, Type::Bool),
                    _   => panic!("Unrecognized unary operator")
                }.refresh_type_vars(ta);
                let typ1 = Type::func1(typ1, Type::TVar(ta.fresh_type_var()));
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
            E::Tuple(l) => {
                if l.len() <= 0 { return Ok(Type::Tuple(Vec::new())) }
                let n = l.len();
                for expr in l {
                    let t = self.type_check_helper(expr, ta)?;
                    ta.push_type(t);
                }
                Ok(Type::Tuple(ta.pop_types(n)))
            },
            E::Matrix(_, _, v) => {
                for x in v {
                    let t = self.type_check_helper(x, ta)?;
                    ta.unify(Type::Num, t)?;
                }
                Ok(Type::Mat)
            },
            E::Bool(_) => Ok(Type::Bool),
            E::Str(_) => Ok(Type::Str),
            E::Int(_) => Ok(Type::Num),
            E::Num(_) => Ok(Type::Num),
            E::IntImag(_) => Ok(Type::Num),
            E::FloatImag(_) => Ok(Type::Num),
            E::Id(x) if x.eq("_") => Ok(Type::Any),
            E::Id(x) => {
                if let Some(t) = ta.get(x) { Ok(t) }
                else if let Some(t) = self.var_types.get(x) { Ok(t.clone().refresh_type_vars(ta)) }
                else if let Some(t) = get_builtin_type(x)   { Ok(t.refresh_type_vars(ta)) }
                else { Err(TypeError::UnknownIdentifier(x.clone())) }
            }
        }
    }

    /// Type check a dot expression given the left and right expression types
    fn type_check_dot(&self, lhs: Type, rhs: Type, ta: &mut TAssums) -> TypeCheckResult<Type> {
        match (lhs, rhs) {
            (Type::TVar(_), _) => Err(TypeError::CannotTypeCheckDot),
            (Type::Func(args1, ret1), Type::Func(mut args2, ret2)) => {
                if args2.len() != 1 { return Err(TypeError::ArgumentListTooLong); }
                ta.push_type(Type::Func(args1, ret2));
                ta.unify(*ret1, args2.pop().unwrap())?;
                Ok(ta.pop_type())
            }
            (lhs, Type::Func(mut args2, ret2)) => {
                if args2.len() != 1 { return Err(TypeError::ArgumentListTooLong); }
                ta.push_type(*ret2);
                ta.unify(lhs, args2.pop().unwrap())?;
                Ok(ta.pop_type())
            }
            (lhs, rhs) => {
                let lhstype = Type::TVar(ta.fresh_type_var());
                let rettype = Type::TVar(ta.fresh_type_var());
                let rhstype = Type::func1(lhstype.clone(), rettype.clone());
                ta.push_type(rettype);
                ta.unify_pairs(vec![lhstype, rhstype], vec![lhs, rhs])?;
                Ok(ta.pop_type())
            }
        }
    }

    /// Type check a dollar expression given the left and right expression types
    fn type_check_dollar(&self, lhs: Type, rhs: Type, ta: &mut TAssums) -> TypeCheckResult<Type> {
        match (lhs, rhs) {
            (Type::Func(args1, ret1), Type::Func(args2, ret2)) => {
                if let Type::Tuple(ret1) = *ret1 {
                    if args2.len() != ret1.len() { return Err(TypeError::ArityMistmatch); }
                    ta.push_type(Type::Func(args1, ret2));
                    ta.unify_pairs(ret1, args2)?;
                    Ok(ta.pop_type())
                } else { Err(TypeError::ExpectedTuple) }
            }
            (Type::Tuple(lhs), Type::Func(args2, ret2)) => {
                if args2.len() != lhs.len() { return Err(TypeError::ArityMistmatch); }
                ta.push_type(*ret2);
                ta.unify_pairs(lhs, args2)?;
                Ok(ta.pop_type())
            }
            (Type::Func(args1, ret1), Type::TVar(v)) => {
                if let Type::Tuple(ret1) = *ret1 {
                    let ret2 = Box::new(Type::TVar(ta.fresh_type_var()));
                    ta.push_type(Type::Func(args1, ret2.clone()));
                    ta.unify(Type::TVar(v), Type::Func(ret1.clone(), ret2))?;
                    Ok(ta.pop_type())
                } else { Err(TypeError::ExpectedTuple) }
            }
            (Type::Tuple(ret1), Type::TVar(v)) => {
                let ret2 = Type::TVar(ta.fresh_type_var());
                ta.push_type(ret2.clone());
                ta.unify(Type::TVar(v), Type::Func(ret1.clone(), Box::new(ret2)))?;
                Ok(ta.pop_type())
            }
            _ => Err(TypeError::CannotTypeCheckDollar)
        }
    }
}

impl Type {
    pub fn list(t: Type) -> Type { Type::List(Box::new(t)) }
    pub fn tup2(t1: Type, t2: Type) -> Type { Type::Tuple(vec![t1, t2]) }
    pub fn var(v: &str) -> Type { Type::TVar(String::from(v)) }
    pub fn func1(arg: Type, ret: Type) -> Type { Type::Func(vec![arg], Box::new(ret)) }
    pub fn func2(arg1: Type, arg2: Type, ret: Type) -> Type { Type::Func(vec![arg1, arg2], Box::new(ret)) }
    pub fn func3(arg1: Type, arg2: Type, arg3: Type, ret: Type) -> Type { Type::Func(vec![arg1, arg2, arg3], Box::new(ret)) }
}