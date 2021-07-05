/* types/typechecker.rs
 *
 * Defines type-checking procedures for expressions (and stmts too sort of)
 */

use std::collections::HashMap;
use crate::ast::{Expr as E, ExprA, AstSlice, Stmt, StmtA};
use crate::builtins::type_check_builtin;
use crate::error::{TypeCheckResult, TypeError};
use super::{Type, TypeEnv, fresh_type_var};
use super::unifier::{unify, unify_pairs};
use Refinements as RS;

/// A mapping from type variables to more specific types
pub type Refinement = HashMap<String, Type>;
pub type Refinements = Vec<Refinement>;

impl TypeEnv {
    /// Type checks expressions within a statement. If the statement is a display,
    /// the type in string form is returned.
    pub fn type_check_stmt(&mut self, stmt: &StmtA) -> TypeCheckResult<String> {
        match &*stmt.stmt {
            Stmt::Assign(v, expr) => {
                let mut types = self.get_expr_types(expr);
                if types.len() != 1 { return Err(TypeError::ExpectedOne); }
                self.var_types.insert(v.clone(), types.pop().unwrap());
                Ok(String::from(""))
            }
            Stmt::Display(expr) => {
                let types = self.get_expr_types(expr);
                let output = types.into_iter().map(|t| t.to_string()).collect::<Vec<String>>().join("\n");
                Ok(output)
            }
        }
    }

    /// Main type checking function for expressions. Binds `hint` to the type of `expr` by
    /// modifying the refinements `rs`. The `i` is used to generate new type variables.
    pub fn typecheck_expr(&mut self, expr: &ExprA, hint: &Type, i: &mut u32, mut rs: RS) -> RS {
        match &*expr.expr {
            E::Lambda(params, body, typeparams, paramtypes, rettype) => {
                // replace type parameters with fresh type variables
                let mut hm: HashMap<String, Type> = HashMap::new();
                typeparams.iter().map(|t| hm.insert(t.clone(), Type::TVar(fresh_type_var(i)))).for_each(drop);
                let paramtypes = paramtypes.iter().map(|t| t.clone().refine(&hm));
                let rettype = rettype.clone().refine(&hm);

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
                let rs = self.typecheck_expr(body, &rettype, i, rs);
                let paramtypes = self.pop_frame();
                unify(&Type::Func(paramtypes, Box::new(rettype)), hint, rs)
            }
            E::FunApp(f, args) => {
                // type check the function expression
                let ftype = Type::TVar(fresh_type_var(i));
                let rs = self.typecheck_expr(f, &ftype, i, rs);

                rs.into_iter().flat_map(|r| match ftype.clone().refine(&r) {
                    Type::Func(mut paramtypes, rettype) => {
                        if paramtypes.len() < args.len() { return vec![]; }
                        let appliedparams = paramtypes.split_off(paramtypes.len() - args.len());

                        // type check arguments
                        let mut rs = vec![r];
                        for (arg, paramtype) in args.into_iter().zip(appliedparams.into_iter()) {
                            match &*arg.expr {
                                E::Id(x) if x.eq("_") => paramtypes.push(paramtype),
                                _ => rs = self.typecheck_expr(arg, &paramtype, i, rs),
                            }
                        }

                        let typ = if paramtypes.len() > 0 {
                            Type::Func(paramtypes, rettype)
                        } else { *rettype };
                        unify(&typ, hint, rs)
                    }
                    Type::TVar(v) => {
                        // type check arguments
                        let mut argtypes: Vec<Type> = Vec::new();
                        let mut rs = vec![r];
                        for arg in args {
                            let t = Type::TVar(fresh_type_var(i));
                            rs = self.typecheck_expr(arg, &t, i, rs);
                            argtypes.push(t);
                        }
                        let rettype = Type::TVar(fresh_type_var(i));
                        let rs = unify(&rettype, hint, rs);
                        unify(&Type::TVar(v), &Type::Func(argtypes, Box::new(rettype)), rs)
                    }
                    _ => vec![]
                }).collect::<RS>()
            }
            E::FunKwApp(_f, _args) => todo!(),
            E::ListIdx(expr, slice) => {
                let ltype = Type::TVar(fresh_type_var(i));
                let rs = self.typecheck_expr(expr, &ltype, i, rs);
                let rs = self.typecheck_slice(slice, i, rs);

                rs.into_iter().flat_map(|r| match ltype.clone().refine(&r) {
                    Type::List(item) => match slice {
                        AstSlice::Single(_) => unify(&*item, hint, vec![r]),
                        AstSlice::Range(_,_) => unify(&Type::List(item), hint, vec![r])
                    }
                    Type::Func(paramtypes, rettype) => match *rettype {
                        Type::List(item) => match slice {
                            AstSlice::Single(_) => unify(&Type::Func(paramtypes, item), hint, vec![r]),
                            AstSlice::Range(..) => unify(&Type::Func(paramtypes, Box::new(Type::List(item))), hint, vec![r])
                        }
                        Type::Str => unify(&Type::Func(paramtypes, Box::new(Type::Str)), hint, vec![r]),
                        _ => vec![]
                    }
                    Type::Str => unify(&Type::Str, hint, vec![r]),
                    _ => vec![]
                }).collect::<RS>()
            }
            E::MatrixIdx(expr, slice1, slice2) => {
                let mtype = Type::TVar(fresh_type_var(i));
                let rs = self.typecheck_expr(expr, &mtype, i, rs);
                let rs = self.typecheck_slice(slice1, i, rs);
                let rs = self.typecheck_slice(slice2, i, rs);
                
                rs.into_iter().flat_map(|r| match mtype.clone().refine(&r) {
                    Type::Mat => match (slice1, slice2) {
                        (AstSlice::Single(_), AstSlice::Single(_)) => unify(&Type::Num, hint, vec![r]),
                        _ => unify(&Type::Mat, hint, vec![r])
                    }
                    Type::Func(paramtypes, rettype) => match *rettype {
                        Type::Mat => match (slice1, slice2) {
                            (AstSlice::Single(_), AstSlice::Single(_)) => unify(&Type::Func(paramtypes, Box::new(Type::Num)), hint, vec![r]),
                            _ => unify(&Type::Func(paramtypes, Box::new(Type::Mat)), hint, vec![r])
                        }
                        _ => vec![]
                    }
                    _ => vec![]
                }).collect::<RS>()
            }
            E::Binop(op, lhs, rhs) => {
                match op.as_str() {
                    "." => return self.typecheck_dot(lhs, rhs, hint, i, rs),
                    "$" => return self.typecheck_dollar(lhs, rhs, hint, i, rs),
                    _ => {}
                }
                let optype = Type::TVar(fresh_type_var(i));
                let rs = self.typecheck_builtin(&format!("op{}", op), &optype, i, rs);
                rs.into_iter().flat_map(|r| match optype.clone().refine(&r) {
                    Type::Func(argtypes, rettype) => {
                        if argtypes.len() != 2 { panic!("Binary op type is not binary"); }
                        let rs = self.typecheck_expr(lhs, &argtypes[0], i, vec![r]);
                        let rs = self.typecheck_expr(rhs, &argtypes[1], i, rs);
                        unify(&*rettype, hint, rs)
                    }
                    Type::TVar(v) => {
                        let lhstype = Type::TVar(fresh_type_var(i));
                        let rhstype = Type::TVar(fresh_type_var(i));
                        let rs = self.typecheck_expr(lhs, &lhstype, i, vec![r]);
                        let rs = self.typecheck_expr(rhs, &rhstype, i, rs);
                        
                        let rettype = Type::TVar(fresh_type_var(i));
                        let rs = unify(&rettype, hint, rs);
                        unify(&Type::TVar(v), &Type::func2(lhstype, rhstype, rettype), rs)
                    }
                    _ => vec![]
                }).collect::<RS>()
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
                        let rs = self.typecheck_expr(expr, &argtypes[0], i, rs);
                        unify(&*rettype, hint, rs)
                    }
                    Type::TVar(v) => {
                        let exprtype = Type::TVar(fresh_type_var(i));
                        let rs = self.typecheck_expr(expr, &exprtype, i, rs);

                        let rettype = Type::TVar(fresh_type_var(i));
                        let rs = unify(&rettype, hint, rs);
                        unify(&Type::TVar(v), &Type::func1(exprtype, rettype), rs)
                    }
                    _ => vec![]
                }
            }
            E::List(l) => {
                let innertype = Type::TVar(fresh_type_var(i));
                for item in l {
                    rs = self.typecheck_expr(item, &innertype, i, rs);
                }
                unify(&Type::list(innertype), hint, rs)
            },
            E::Tuple(l) => {
                let mut ls: Vec<Type> = Vec::new();
                for expr in l {
                    let t = Type::TVar(fresh_type_var(i));
                    rs = self.typecheck_expr(expr, &t, i, rs);
                    ls.push(t);
                }
                unify(&Type::Tuple(ls), hint, rs)
            },
            E::Matrix(_, _, v) => {
                for x in v {
                    rs = self.typecheck_expr(x, &Type::Num, i, rs);
                }
                unify(&Type::Mat, hint, rs)
            },
            E::Bool(_) => unify(&Type::Bool, hint, rs),
            E::Str(_) => unify(&Type::Str, hint, rs),
            E::Int(_) => unify(&Type::Num, hint, rs),
            E::Num(_) => unify(&Type::Num, hint, rs),
            E::IntImag(_) => unify(&Type::Num, hint, rs),
            E::FloatImag(_) => unify(&Type::Num, hint, rs),
            E::Id(x) if x.eq("_") => rs,
            E::Id(x) => {
                if let Some(t) = self.get(x, i) { unify(&t, hint, rs) }
                else { self.typecheck_builtin(&x, hint, i, rs) }
            }
        }
    }

    fn typecheck_builtin(&mut self, name: &str, hint: &Type, i: &mut u32, rs: RS) -> RS {
        type_check_builtin(name).into_iter().flat_map(|typ| {
            unify(&typ.refresh_type_vars(i), hint, rs.clone())
        }).collect::<RS>()
    }

    fn typecheck_dot(&mut self, lhs: &ExprA, rhs: &ExprA, hint: &Type, i: &mut u32, rs: RS) -> RS {
        let lhstype = Type::TVar(fresh_type_var(i));
        let rhstype = Type::TVar(fresh_type_var(i));
        let rs = self.typecheck_expr(lhs, &lhstype, i, rs);
        let rs = self.typecheck_expr(rhs, &rhstype, i, rs);

        rs.into_iter().flat_map(|r| match (lhstype.clone().refine(&r), rhstype.clone().refine(&r)) {
            (Type::TVar(_), _) => vec![],
            (Type::Func(args1, ret1), Type::Func(mut args2, ret2)) => {
                if args2.len() != 1 { return vec![]; }
                let rs = unify(&*ret1, &args2.pop().unwrap(), vec![r]);
                unify(&Type::Func(args1, ret2), hint, rs)
            }
            (lhstype, Type::Func(mut args2, ret2)) => {
                if args2.len() != 1 { return vec![]; }
                let rs = unify(&lhstype, &args2.pop().unwrap(), vec![r]);
                unify(&*ret2, hint, rs)
            }
            (lhstype, rhstype) => {
                let rettype = Type::TVar(fresh_type_var(i));
                let rs = unify(&rettype, hint, vec![r]);
                unify(&rhstype, &Type::func1(lhstype.clone(), rettype), rs)
            }
        }).collect::<RS>()
    }

    fn typecheck_dollar(&mut self, lhs: &ExprA, rhs: &ExprA, hint: &Type, i: &mut u32, rs: RS) -> RS {
        let lhstype = Type::TVar(fresh_type_var(i));
        let rhstype = Type::TVar(fresh_type_var(i));
        let rs = self.typecheck_expr(lhs, &lhstype, i, rs);
        let rs = self.typecheck_expr(rhs, &rhstype, i, rs);

        rs.into_iter().flat_map(|r| match (lhstype.clone().refine(&r), rhstype.clone().refine(&r)) {
            (Type::Func(args1, ret1), Type::Func(args2, ret2)) => {
                if let Type::Tuple(ret1) = *ret1 {
                    if args2.len() != ret1.len() { return vec![]; }
                    let rs = unify_pairs(&ret1, &args2, vec![r]);
                    unify(&Type::Func(args1, ret2), hint, rs)
                } else { vec![] }
            }
            (Type::Tuple(lhs), Type::Func(args2, ret2)) => {
                if args2.len() != lhs.len() { return vec![]; }
                let rs = unify_pairs(&lhs, &args2, vec![r]);
                unify(&ret2, hint, rs)
            }
            (Type::Func(args1, ret1), Type::TVar(v)) => {
                if let Type::Tuple(ret1) = *ret1 {
                    let ret2 = Box::new(Type::TVar(fresh_type_var(i)));
                    let rs = unify(&Type::TVar(v), &Type::Func(ret1.clone(), ret2.clone()), vec![r]);
                    unify(&Type::Func(args1, ret2), hint, rs)
                } else { vec![] }
            }
            (Type::Tuple(ret1), Type::TVar(v)) => {
                let ret2 = Type::TVar(fresh_type_var(i));
                let rs = unify(&Type::TVar(v), &Type::Func(ret1.clone(), Box::new(ret2.clone())), vec![r]);
                unify(&ret2, hint, rs)
            }
            _ => vec![]
        }).collect::<RS>()
    }

    /// Make sure the indices have type `Num`
    fn typecheck_slice(&mut self, slice: &AstSlice, i: &mut u32, mut rs: RS) -> RS {
        match slice {
            AstSlice::Single(idx) => self.typecheck_expr(idx, &Type::Num, i, rs),
            AstSlice::Range(idx1, idx2) => {
                if let Some(idx1) = idx1 { rs = self.typecheck_expr(idx1, &Type::Num, i, rs); }
                if let Some(idx2) = idx2 { rs = self.typecheck_expr(idx2, &Type::Num, i, rs); }
                rs
            }
        }
    }
}