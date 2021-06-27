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
use types::{TypeRefinement, unify, unify_pairs, fresh_type_var};

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
    // Mapping from global ids to the type of the expressions to which they resolve
    var_types: HashMap<String, Type>,
    // A stack of frames. Each frame maps a variable name to a type assumption
    // and its position in the variable list at the time of being pushed.
    id_frames: Vec<HashMap<String, (usize, Type)>>,
}

impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv{var_types: HashMap::new(), id_frames: Vec::new()}
    }

    /// Introduces fresh type variables for each id and adds them to
    /// a new local type-assumption frame.
    fn push_new_frame(&mut self, ids: Vec<String>, types: Vec<Type>) {
        let mut hm: HashMap<String, (usize, Type)> = HashMap::new();
        let mut i: usize = 0;
        for (id, typ) in ids.into_iter().zip(types.into_iter()) {
            hm.insert(id, (i, typ));
            i += 1;
        }
        self.id_frames.push(hm);
    }

    /// Pops the top frame from the stack. The type assumptions are returned in the
    /// same order as the variable list that was passed to push_new_frame.
    fn pop_frame(&mut self) -> Vec<Type> {
        let frame = self.id_frames.pop().unwrap();
        let mut ret: Vec<(usize, Type)> = frame.into_iter().map(|(_, p)| p).collect();
        let mut i = 0;
        while i < ret.len() {
            let j = ret[i].0;
            ret.swap(i, j);
            if ret[i].0 == i { i += 1; }
        }
        ret.into_iter().map(|(_, t)| t).collect()
    }

    /// Lookup the assumed type for the given id.
    pub fn get(&self, id: &String, i: &mut u32) -> Option<Type> {
        for frame in self.id_frames.iter().rev() {
            if let Some((_, typ)) = frame.get(id) { return Some(typ.clone()); }
        }
        if let Some(typ) = self.var_types.get(id) { return Some(typ.clone().refresh_type_vars(i)); }
        None
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
    pub fn type_check(&mut self, expr: &E) -> TypeCheckResult<Type> {
        let (typ, _) = self.type_check_helper(expr, &mut 0)?;
        Ok(typ)
    }

    /// Performs type checking for expressions. The i is used for generating fresh type variables.
    /// Returns the inferred type and any type refinements that were necessary for inference to succeed.
    fn type_check_helper(&mut self, expr: &E, i: &mut u32) -> TypeCheckResult<(Type, TypeRefinement)> {
        match expr {
            E::Lambda(params, body, typeparams, paramtypes, rettype) => {
                // replace type parameters with fresh type variables
                let mut hm: HashMap<String, Type> = HashMap::new();
                typeparams.iter().map(|t| hm.insert(t.clone(), Type::TVar(fresh_type_var(i)))).for_each(drop);
                let paramtypes = paramtypes.iter().map(|t| t.clone().replace_type_vars(&hm));
                let rettype = rettype.clone().replace_type_vars(&hm);

                // replace `Any` types (from underscores) with new type variables
                let paramtypes = paramtypes.into_iter().map(|t| match t {
                    Type::Any => Type::TVar(fresh_type_var(i)),
                    t => t,
                }).collect::<Vec<Type>>();

                // actually typecheck the expression now
                self.push_new_frame(params.clone(), paramtypes);
                let (bodytype, rf) = self.type_check_helper(body, i)?;
                let rf = unify(&rettype, &bodytype, rf)?;
                let paramtypes = self.pop_frame().into_iter().map(|t| rf.refine(t)).collect::<Vec<Type>>();
                let bodytype = rf.refine(bodytype);
                Ok((Type::Func(paramtypes, Box::new(bodytype)), rf))
            }
            E::FunApp(f, args) => {
                let mut rf = TypeRefinement::new();

                // type check each of the argument expressions
                let mut argtypes: Vec<Type> = Vec::new();
                for arg in args.iter() {
                    let (typ, rf2) = self.type_check_helper(arg, i)?;
                    rf = rf.combine(rf2)?;
                    argtypes.push(typ);
                }

                // type check the function expression
                let (ftype, rf2) = self.type_check_helper(f, i)?;
                rf = rf.combine(rf2)?;

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
                let type2 = Type::Func(argtypes, Box::new(Type::TVar(fresh_type_var(i))));
                let rf = unify(&type1, &type2, rf)?;
                if let Type::Func(_, rettype) = rf.refine(type1) {
                    Ok((*rettype, rf))
                } else { unreachable!() }
            }
            E::FunKwApp(_f, _args) => Ok((Type::Unknown, TypeRefinement::new())),
            E::ListIdx(_expr, _slice) => Ok((Type::Unknown, TypeRefinement::new())),
            E::MatrixIdx(_expr, _slice1, _slice2) => Ok((Type::Unknown, TypeRefinement::new())),
            E::Binop(op, lhs, rhs) => {
                let (lhs, mut rf) = self.type_check_helper(lhs, i)?;
                let (rhs, rf2) = self.type_check_helper(rhs, i)?;
                rf = rf.combine(rf2)?;
                let typ2 = match op.as_str() {
                    "."  => return self.type_check_dot(lhs, rhs, i),
                    "$"  => return self.type_check_dollar(lhs, rhs, i),
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
                }.unwrap().refresh_type_vars(i);
                let typ1 = Type::func2(lhs, rhs, Type::TVar(fresh_type_var(i)));
                let rf = unify(&typ1, &typ2, rf)?;
                if let Type::Func(_, rettype) = rf.refine(typ1) {
                    Ok((*rettype, rf))
                } else { unreachable!() }
            }
            E::Unop(op, expr) => {
                let (typ1, rf) = self.type_check_helper(expr, i)?;
                let typ2 = match op.as_str() {
                    "-" => Type::func1(Type::Num, Type::Num),
                    "!" => Type::func1(Type::Bool, Type::Bool),
                    _   => panic!("Unrecognized unary operator")
                }.refresh_type_vars(i);
                let typ1 = Type::func1(typ1, Type::TVar(fresh_type_var(i)));
                let rf = unify(&typ1, &typ2, rf)?;
                if let Type::Func(_, rettype) = rf.refine(typ1) {
                    Ok((*rettype, rf))
                } else { unreachable!() }
            }
            E::List(l) => {
                if l.len() <= 0 { return Ok((Type::list(Type::TVar(fresh_type_var(i))), TypeRefinement::new())); }
                let (typ, mut rf) = self.type_check_helper(&l[0], i)?;
                for item in &l[1..] {
                    let (typ2, rf2) = self.type_check_helper(item, i)?;
                    rf = rf.combine(rf2)?;
                    rf = unify(&typ, &typ2, rf)?;
                }
                Ok((Type::list(rf.refine(typ)), rf))
            },
            E::Tuple(l) => {
                if l.len() <= 0 { return Ok((Type::Tuple(Vec::new()), TypeRefinement::new())) }
                let mut ls: Vec<Type> = Vec::new();
                let mut rf = TypeRefinement::new();
                for expr in l {
                    let (t, rf2) = self.type_check_helper(expr, i)?;
                    rf = rf.combine(rf2)?;
                    ls.push(t);
                }
                Ok((Type::Tuple(ls), rf))
            },
            E::Matrix(_, _, v) => {
                let mut rf = TypeRefinement::new();
                for x in v {
                    let (t, rf2) = self.type_check_helper(x, i)?;
                    rf = rf.combine(rf2)?;
                    rf = unify(&Type::Num, &t, rf)?;
                }
                Ok((Type::Mat, rf))
            },
            E::Bool(_) => Ok((Type::Bool, TypeRefinement::new())),
            E::Str(_) => Ok((Type::Str, TypeRefinement::new())),
            E::Int(_) => Ok((Type::Num, TypeRefinement::new())),
            E::Num(_) => Ok((Type::Num, TypeRefinement::new())),
            E::IntImag(_) => Ok((Type::Num, TypeRefinement::new())),
            E::FloatImag(_) => Ok((Type::Num, TypeRefinement::new())),
            E::Id(x) if x.eq("_") => Ok((Type::Any, TypeRefinement::new())),
            E::Id(x) => {
                if let Some(t) = self.get(x, i) { Ok((t, TypeRefinement::new())) }
                else if let Some(t) = get_builtin_type(x) { Ok((t.refresh_type_vars(i), TypeRefinement::new())) }
                else { Err(TypeError::UnknownIdentifier(x.clone())) }
            }
        }
    }

    /// Type check a dot expression given the left and right expression types
    fn type_check_dot(&self, lhs: Type, rhs: Type, i: &mut u32) -> TypeCheckResult<(Type, TypeRefinement)> {
        match (lhs, rhs) {
            (Type::TVar(_), _) => Err(TypeError::CannotTypeCheckDot),
            (Type::Func(args1, ret1), Type::Func(mut args2, ret2)) => {
                if args2.len() != 1 { return Err(TypeError::ArgumentListTooLong); }
                let rf = unify(&*ret1, &args2.pop().unwrap(), TypeRefinement::new())?;
                let args1 = args1.into_iter().map(|t| rf.refine(t)).collect::<Vec<Type>>();
                let ret2 = Box::new(rf.refine(*ret2));
                Ok((Type::Func(args1, ret2), rf))
            }
            (lhs, Type::Func(mut args2, ret2)) => {
                if args2.len() != 1 { return Err(TypeError::ArgumentListTooLong); }
                let rf = unify(&lhs, &args2.pop().unwrap(), TypeRefinement::new())?;
                let ret2 = rf.refine(*ret2);
                Ok((ret2, rf))
            }
            (lhs, rhs) => {
                let lhstype = Type::TVar(fresh_type_var(i));
                let rettype = Type::TVar(fresh_type_var(i));
                let rhstype = Type::func1(lhstype.clone(), rettype.clone());
                let rf = unify_pairs(&vec![lhstype, rhstype], &vec![lhs, rhs], TypeRefinement::new())?;
                Ok((rf.refine(rettype), rf))
            }
        }
    }

    /// Type check a dollar expression given the left and right expression types
    fn type_check_dollar(&self, lhs: Type, rhs: Type, i: &mut u32) -> TypeCheckResult<(Type, TypeRefinement)> {
        match (lhs, rhs) {
            (Type::Func(args1, ret1), Type::Func(args2, ret2)) => {
                if let Type::Tuple(ret1) = *ret1 {
                    if args2.len() != ret1.len() { return Err(TypeError::ArityMistmatch); }
                    let rf = unify_pairs(&ret1, &args2, TypeRefinement::new())?;
                    let args1 = args1.into_iter().map(|t| rf.refine(t)).collect::<Vec<Type>>();
                    let ret2 = Box::new(rf.refine(*ret2));
                    Ok((Type::Func(args1, ret2), rf))
                } else { Err(TypeError::ExpectedTuple) }
            }
            (Type::Tuple(lhs), Type::Func(args2, ret2)) => {
                if args2.len() != lhs.len() { return Err(TypeError::ArityMistmatch); }
                let rf = unify_pairs(&lhs, &args2, TypeRefinement::new())?;
                let ret2 = rf.refine(*ret2);
                Ok((ret2, rf))
            }
            (Type::Func(args1, ret1), Type::TVar(v)) => {
                if let Type::Tuple(ret1) = *ret1 {
                    let ret2 = Box::new(Type::TVar(fresh_type_var(i)));
                    let rf = unify(&Type::TVar(v), &Type::Func(ret1.clone(), ret2.clone()), TypeRefinement::new())?;
                    let args1 = args1.into_iter().map(|t| rf.refine(t)).collect::<Vec<Type>>();
                    let ret2 = Box::new(rf.refine(*ret2));
                    Ok((Type::Func(args1, ret2), rf))
                } else { Err(TypeError::ExpectedTuple) }
            }
            (Type::Tuple(ret1), Type::TVar(v)) => {
                let ret2 = Type::TVar(fresh_type_var(i));
                let rf = unify(&Type::TVar(v), &Type::Func(ret1.clone(), Box::new(ret2.clone())), TypeRefinement::new())?;
                let ret2 = rf.refine(ret2);
                Ok((ret2, rf))
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