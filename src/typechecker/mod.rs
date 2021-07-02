/* typechecker/mod.rs
 *
 * Defines types and type inference procedures.
 */

pub mod typecheckerror;
mod types;

use std::collections::HashMap;
use crate::ast::{AstExpr as E, AstStmt};
use crate::builtins::type_check_builtin;
use typecheckerror::{TypeCheckResult, TypeError};
use types::{unify, unify_pairs, fresh_type_var};
use TypeRefinement as TR;

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
    Any,    // the type of underscore arguments
}

/// A mapping from type variables to more specific types
#[derive(Debug, Clone)]
pub struct TypeRefinement {
    bindings: Vec<HashMap<String, Type>>
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
        let mut i = 0;
        let typ = Type::TVar(fresh_type_var(&mut i));
        let rf = self.type_check_helper(expr, &typ, &mut i, TR::new())?;
        
        // DEBUG
        for rf in rf.bindings.iter() {
            println!("{}", typ.clone().replace_type_vars(rf).to_string());
        }

        Ok(typ.replace_type_vars(&rf.expect_one()?))
    }

    /// Unifies the type of the expression with the hint and returns the necessary bindings. The i is
    /// used for generating fresh type variables. The rf is a set of existing bindings.
    fn type_check_helper(&mut self, expr: &E, hint: &Type, i: &mut u32, mut rf: TR) -> TypeCheckResult<TR> {
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
                let rettype = match rettype {
                    Type::Any => Type::TVar(fresh_type_var(i)),
                    t => t,
                };

                // actually typecheck the expression now
                self.push_new_frame(params.clone(), paramtypes);
                let rf = self.type_check_helper(body, &rettype, i, rf)?;
                let paramtypes = self.pop_frame();
                unify(&Type::Func(paramtypes, Box::new(rettype)), hint, rf)
            }
            E::FunApp(f, args) => {

                // type check the function expression
                let ftype = Type::TVar(fresh_type_var(i));
                let rf = self.type_check_helper(f, &ftype, i, rf)?;

                rf.flat_traverse(|rf| match ftype.clone().replace_type_vars(&rf) {
                    Type::Func(mut paramtypes, rettype) => {
                        if paramtypes.len() < args.len() { return Err(TypeError::ArgumentListTooLong); }
                        let appliedparams = paramtypes.split_off(paramtypes.len() - args.len());

                        // type check arguments
                        let mut rf = TR::pure(rf);
                        for (arg, paramtype) in args.into_iter().zip(appliedparams.into_iter()) {
                            match arg {
                                E::Id(x) if x.eq("_") => paramtypes.push(paramtype),
                                _ => rf = self.type_check_helper(arg, &paramtype, i, rf)?,
                            }
                        }

                        let typ = if paramtypes.len() > 0 {
                            Type::Func(paramtypes, rettype)
                        } else { *rettype };
                        unify(&typ, hint, rf)
                    }
                    Type::TVar(v) => {
                        // type check arguments
                        let mut argtypes: Vec<Type> = Vec::new();
                        let mut rf = TR::pure(rf);
                        for arg in args {
                            let t = Type::TVar(fresh_type_var(i));
                            rf = self.type_check_helper(arg, &t, i, rf)?;
                            argtypes.push(t);
                        }
                        let rettype = Type::TVar(fresh_type_var(i));
                        let rf = unify(&rettype, hint, rf)?;
                        unify(&Type::TVar(v), &Type::Func(argtypes, Box::new(rettype)), rf)
                    }
                    ftype => Err(TypeError::ExpectedFunction(ftype))
                })
            }
            E::FunKwApp(_f, _args) => Ok(rf),
            E::ListIdx(_expr, _slice) => Ok(rf),
            E::MatrixIdx(_expr, _slice1, _slice2) => Ok(rf),
            E::Binop(op, lhs, rhs) => {
                match op.as_str() {
                    "." => return self.type_check_dot(lhs, rhs, hint, i, rf),
                    "$" => return self.type_check_dollar(lhs, rhs, hint, i, rf),
                    _ => {}
                }
                let optype = Type::TVar(fresh_type_var(i));
                let rf = match op.as_str() {
                    "+"  => self.type_check_builtin("op+", &optype, i, rf),
                    "-"  => self.type_check_builtin("op-", &optype, i, rf),
                    "*"  => self.type_check_builtin("op*", &optype, i, rf),
                    "/"  => self.type_check_builtin("op/", &optype, i, rf),
                    "%"  => self.type_check_builtin("op%", &optype, i, rf),
                    "**" => self.type_check_builtin("op**", &optype, i, rf),
                    "==" => self.type_check_builtin("op==", &optype, i, rf),
                    "!=" => self.type_check_builtin("op!=", &optype, i, rf),
                    "<=" => self.type_check_builtin("op<=", &optype, i, rf),
                    ">=" => self.type_check_builtin("op>=", &optype, i, rf),
                    "<"  => self.type_check_builtin("op<", &optype, i, rf),
                    ">"  => self.type_check_builtin("op>", &optype, i, rf),
                    "&&" => self.type_check_builtin("&&", &optype, i, rf),
                    "||" => self.type_check_builtin("||", &optype, i, rf),
                    _ => panic!("Unrecognized binary operator")
                }?;
                rf.flat_traverse(|rf| match optype.clone().replace_type_vars(&rf) {
                    Type::Func(argtypes, rettype) => {
                        if argtypes.len() != 2 { panic!("Binary op type is not binary"); }
                        let rf = self.type_check_helper(lhs, &argtypes[0], i, TR::pure(rf))?;
                        let rf = self.type_check_helper(rhs, &argtypes[1], i, rf)?;
                        unify(&*rettype, hint, rf)
                    }
                    Type::TVar(v) => {
                        let lhstype = Type::TVar(fresh_type_var(i));
                        let rhstype = Type::TVar(fresh_type_var(i));
                        let rf = self.type_check_helper(lhs, &lhstype, i, TR::pure(rf))?;
                        let rf = self.type_check_helper(rhs, &rhstype, i, rf)?;
                        
                        let rettype = Type::TVar(fresh_type_var(i));
                        let rf = unify(&rettype, hint, rf)?;
                        unify(&Type::TVar(v), &Type::func2(lhstype, rhstype, rettype), rf)
                    }
                    optype => Err(TypeError::ExpectedFunction(optype))
                })
            }
            E::Unop(op, expr) => {
                let optype = match op.as_str() {
                    "-" => Type::func1(Type::Num, Type::Num),
                    "!" => Type::func1(Type::Bool, Type::Bool),
                    _   => panic!("Unrecognized unary operator")
                }.refresh_type_vars(i);
                match optype {
                    Type::Func(argtypes, rettype) => {
                        if argtypes.len() != 1 { panic!("Unary operator is not unary"); }
                        let rf = self.type_check_helper(expr, &argtypes[0], i, rf)?;
                        unify(&*rettype, hint, rf)
                    }
                    Type::TVar(v) => {
                        let exprtype = Type::TVar(fresh_type_var(i));
                        let rf = self.type_check_helper(expr, &exprtype, i, rf)?;

                        let rettype = Type::TVar(fresh_type_var(i));
                        let rf = unify(&rettype, hint, rf)?;
                        unify(&Type::TVar(v), &Type::func1(exprtype, rettype), rf)
                    }
                    optype => Err(TypeError::ExpectedFunction(optype))
                }
            }
            E::List(l) => {
                let innertype = Type::TVar(fresh_type_var(i));
                for item in l {
                    rf = self.type_check_helper(item, &innertype, i, rf)?;
                }
                unify(&Type::list(innertype), hint, rf)
            },
            E::Tuple(l) => {
                let mut ls: Vec<Type> = Vec::new();
                for expr in l {
                    let t = Type::TVar(fresh_type_var(i));
                    rf = self.type_check_helper(expr, &t, i, rf)?;
                    ls.push(t);
                }
                unify(&Type::Tuple(ls), hint, rf)
            },
            E::Matrix(_, _, v) => {
                for x in v {
                    rf = self.type_check_helper(x, &Type::Num, i, rf)?;
                }
                unify(&Type::Mat, hint, rf)
            },
            E::Bool(_) => unify(&Type::Bool, hint, rf),
            E::Str(_) => unify(&Type::Str, hint, rf),
            E::Int(_) => unify(&Type::Num, hint, rf),
            E::Num(_) => unify(&Type::Num, hint, rf),
            E::IntImag(_) => unify(&Type::Num, hint, rf),
            E::FloatImag(_) => unify(&Type::Num, hint, rf),
            E::Id(x) if x.eq("_") => Ok(rf),
            E::Id(x) => {
                if let Some(t) = self.get(x, i) { unify(&t, hint, rf) }
                else { self.type_check_builtin(&x, hint, i, rf) }
//                else if let Some(t) = get_builtin_type(x) { unify(&t.refresh_type_vars(i), hint, rf) }
//                else { Err(TypeError::UnknownIdentifier(x.clone())) }
            }
        }
    }

    fn type_check_builtin(&mut self, name: &str, hint: &Type, i: &mut u32, rf: TR) -> TypeCheckResult<TR> {
        let bindings = type_check_builtin(name).into_iter().flat_map(|typ| {
            match unify(&typ.refresh_type_vars(i), hint, rf.clone()) {
                Ok(rf) => rf.bindings,
                Err(_) => vec![],
            }
        }).collect::<Vec<HashMap<String, Type>>>();
        match bindings.len() {
            0 => Err(TypeError::FlatTraverseFailed),
            _ => Ok(TR{bindings})
        }
    }

    /// Type check a dot expression given the left and right expression types
    fn type_check_dot(&mut self, lhs: &E, rhs: &E, hint: &Type, i: &mut u32, rf: TR) -> TypeCheckResult<TR> {
        let lhstype = Type::TVar(fresh_type_var(i));
        let rhstype = Type::TVar(fresh_type_var(i));
        let rf = self.type_check_helper(lhs, &lhstype, i, rf)?;
        let rf = self.type_check_helper(rhs, &rhstype, i, rf)?;

        rf.flat_traverse(|rf| match (lhstype.clone().replace_type_vars(&rf), rhstype.clone().replace_type_vars(&rf)) {
            (Type::TVar(_), _) => Err(TypeError::CannotTypeCheckDot),
            (Type::Func(args1, ret1), Type::Func(mut args2, ret2)) => {
                if args2.len() != 1 { return Err(TypeError::ArgumentListTooLong); }
                let rf = unify(&*ret1, &args2.pop().unwrap(), TR::pure(rf))?;
                unify(&Type::Func(args1, ret2), hint, rf)
            }
            (lhstype, Type::Func(mut args2, ret2)) => {
                if args2.len() != 1 { return Err(TypeError::ArgumentListTooLong); }
                let rf = unify(&lhstype, &args2.pop().unwrap(), TR::pure(rf))?;
                unify(&*ret2, hint, rf)
            }
            (lhstype, rhstype) => {
                let rettype = Type::TVar(fresh_type_var(i));
                let rf = unify(&rettype, hint, TR::pure(rf))?;
                unify(&rhstype, &Type::func1(lhstype.clone(), rettype), rf)
            }
        })
    }

    /// Type check a dollar expression given the left and right expression types
    fn type_check_dollar(&mut self, lhs: &E, rhs: &E, hint: &Type, i: &mut u32, rf: TR) -> TypeCheckResult<TR> {
        let lhstype = Type::TVar(fresh_type_var(i));
        let rhstype = Type::TVar(fresh_type_var(i));
        let rf = self.type_check_helper(lhs, &lhstype, i, rf)?;
        let rf = self.type_check_helper(rhs, &rhstype, i, rf)?;

        rf.flat_traverse(|rf| match (lhstype.clone().replace_type_vars(&rf), rhstype.clone().replace_type_vars(&rf)) {
            (Type::Func(args1, ret1), Type::Func(args2, ret2)) => {
                if let Type::Tuple(ret1) = *ret1 {
                    if args2.len() != ret1.len() { return Err(TypeError::ArityMistmatch); }
                    let rf = unify_pairs(&ret1, &args2, rf)?;
                    unify(&Type::Func(args1, ret2), hint, TR::pure(rf))
                } else { Err(TypeError::ExpectedTuple) }
            }
            (Type::Tuple(lhs), Type::Func(args2, ret2)) => {
                if args2.len() != lhs.len() { return Err(TypeError::ArityMistmatch); }
                let rf = unify_pairs(&lhs, &args2, rf)?;
                unify(&ret2, hint, TR::pure(rf))
            }
            (Type::Func(args1, ret1), Type::TVar(v)) => {
                if let Type::Tuple(ret1) = *ret1 {
                    let ret2 = Box::new(Type::TVar(fresh_type_var(i)));
                    let rf = unify(&Type::TVar(v), &Type::Func(ret1.clone(), ret2.clone()), TR::pure(rf))?;
                    unify(&Type::Func(args1, ret2), hint, rf)
                } else { Err(TypeError::ExpectedTuple) }
            }
            (Type::Tuple(ret1), Type::TVar(v)) => {
                let ret2 = Type::TVar(fresh_type_var(i));
                let rf = unify(&Type::TVar(v), &Type::Func(ret1.clone(), Box::new(ret2.clone())), TR::new())?;
                unify(&ret2, hint, rf)
            }
            _ => Err(TypeError::CannotTypeCheckDollar)
        })
    }
}

impl Type {
    pub fn list(t: Type) -> Type { Type::List(Box::new(t)) }
    pub fn var(v: &str) -> Type { Type::TVar(String::from(v)) }
    pub fn func1(arg: Type, ret: Type) -> Type { Type::Func(vec![arg], Box::new(ret)) }
    pub fn func2(arg1: Type, arg2: Type, ret: Type) -> Type { Type::Func(vec![arg1, arg2], Box::new(ret)) }
}