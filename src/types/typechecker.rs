/* types/typechecker.rs
 *
 * Defines type-checking procedures for expressions (and stmts too sort of)
 */

use std::collections::HashMap;
use crate::ast::{Expr as E, ExprA, AstSlice, Stmt, StmtA};
use crate::builtins::type_check_builtin;
use crate::error::TypeCheckResult;
use super::{Refinements as RS, Type, TypeEnv};
use super::unifier::{unify, unify_pairs};

impl TypeEnv {
    /// Type checks expressions within a statement. If the statement is a display,
    /// the type in string form is returned.
    pub fn typecheck_stmt(&mut self, stmt: &StmtA) -> TypeCheckResult<String> {
        match &*stmt.stmt {
            Stmt::Assign(v, expr) => {
                let rs = self.rs.clone();   // NOT IDEAL
                let vtype = Type::TVar(self.fresh_type_var());
                let rs = self.typecheck_expr(expr, &vtype, rs)?;
                self.rs = rs;
                self.var_types.insert(v.clone(), vtype);
                Ok(String::from(""))
            }
            Stmt::Display(expr) => {
                let typ = Type::TVar(self.fresh_type_var());
                let rs = self.typecheck_expr(expr, &typ, self.rs.clone())?;
                Ok(rs.into_iter().map(|r| typ.clone().refine(&r).normalize_type_var_names().to_string()).collect::<Vec<String>>().join("\n"))
            }
        }
    }

    /// Main type checking function for expressions. Binds `hint` to the type of `expr` by
    /// modifying the refinements `rs`. The `i` is used to generate new type variables.
    pub fn typecheck_expr(&mut self, expr: &ExprA, hint: &Type, mut rs: RS) -> TypeCheckResult<RS> {
        match &*expr.expr {
            E::Lambda(params, body, typeparams, paramtypes, rettype) => {
                // replace type parameters with fresh type variables
                let mut hm: HashMap<String, Type> = HashMap::new();
                typeparams.iter().map(|t| hm.insert(t.clone(), Type::TVar(self.fresh_type_var()))).for_each(drop);
                let paramtypes = paramtypes.iter().map(|t| t.clone().refine(&hm));
                let rettype = rettype.clone().refine(&hm);

                // replace `Any` types (from underscores) with new type variables
                let paramtypes = paramtypes.into_iter().map(|t| match t {
                    Type::Any => Type::TVar(self.fresh_type_var()),
                    t => t,
                }).collect::<Vec<Type>>();
                let rettype = match rettype {
                    Type::Any => Type::TVar(self.fresh_type_var()),
                    t => t,
                };

                // actually typecheck the expression now
                self.push_new_frame(params.clone(), paramtypes);
                let rs = self.typecheck_expr(body, &rettype, rs)?;
                let paramtypes = self.pop_frame();
                unify(&Type::Func(paramtypes, Box::new(rettype)), hint, rs).expect_type(hint, expr)
            }
            E::FunApp(f, args) => {
                // type check the function expression
                let ftype = Type::TVar(self.fresh_type_var());
                let mut rs = self.typecheck_expr(f, &ftype, rs)?;

                // type check arguments
                let mut argtypes: Vec<Type> = Vec::with_capacity(args.len());
                let mut unappliedargtypes: Vec<Type> = Vec::new();
                for arg in args {
                    let argtype = Type::TVar(self.fresh_type_var());
                    match &*arg.expr {
                        E::Id(x) if x.eq("_") => unappliedargtypes.push(argtype.clone()),
                        _ => rs = self.typecheck_expr(arg, &argtype, rs)?,
                    }
                    argtypes.push(argtype);
                }

                // unification
                let rettype = Type::TVar(self.fresh_type_var());
                let rs = if unappliedargtypes.len() > 0 {
                    unify(&Type::Func(unappliedargtypes, Box::new(rettype.clone())), hint, rs)
                } else {
                    unify(&rettype, hint, rs)
                }.expect_type(hint, expr)?;
                unify(&ftype, &Type::Func(argtypes, Box::new(rettype)), rs).expect_nonempty(expr)
            }
            E::FunKwApp(_f, _args) => todo!(),
            E::ListIdx(lexpr, slice) => {
                let ltype = Type::TVar(self.fresh_type_var());
                let rs = self.typecheck_expr(lexpr, &ltype, rs)?;
                let rs = self.typecheck_slice(slice, rs)?;

                rs.traverse(expr, |r| match ltype.clone().refine(&r) {
                    Type::List(item) => match slice {
                        AstSlice::Single(_) => unify(&*item, hint, RS::pure(r)).to_option(),
                        AstSlice::Range(_,_) => unify(&Type::List(item), hint, RS::pure(r)).to_option()
                    }
                    Type::Func(paramtypes, rettype) => match *rettype {
                        Type::List(item) => match slice {
                            AstSlice::Single(_) => unify(&Type::Func(paramtypes, item), hint, RS::pure(r)).to_option(),
                            AstSlice::Range(..) => unify(&Type::Func(paramtypes, Box::new(Type::List(item))), hint, RS::pure(r)).to_option()
                        }
                        Type::Str => unify(&Type::Func(paramtypes, Box::new(Type::Str)), hint, RS::pure(r)).to_option(),
                        _ => None
                    }
                    Type::Str => unify(&Type::Str, hint, RS::pure(r)).to_option(),
                    _ => None
                })
            }
            E::MatrixIdx(mat, slice1, slice2) => {
                let mtype = Type::TVar(self.fresh_type_var());
                let rs = self.typecheck_expr(mat, &mtype, rs)?;
                let rs = self.typecheck_slice(slice1, rs)?;
                let rs = self.typecheck_slice(slice2, rs)?;
                
                rs.traverse(expr, |r| match mtype.clone().refine(&r) {
                    Type::Mat => match (slice1, slice2) {
                        (AstSlice::Single(_), AstSlice::Single(_)) => unify(&Type::Num, hint, RS::pure(r)).to_option(),
                        _ => unify(&Type::Mat, hint, RS::pure(r)).to_option()
                    }
                    Type::Func(paramtypes, rettype) => match *rettype {
                        Type::Mat => match (slice1, slice2) {
                            (AstSlice::Single(_), AstSlice::Single(_)) => unify(&Type::Func(paramtypes, Box::new(Type::Num)), hint, RS::pure(r)).to_option(),
                            _ => unify(&Type::Func(paramtypes, Box::new(Type::Mat)), hint, RS::pure(r)).to_option()
                        }
                        _ => None
                    }
                    _ => None
                })
            }
            E::Binop(op, lhs, rhs) if op.eq(".") => self.typecheck_dot(expr, lhs, rhs, hint, rs),
            E::Binop(op, lhs, rhs) if op.eq("$") => self.typecheck_dollar(expr, lhs, rhs, hint, rs),
            E::Binop(op, lhs, rhs) => {
                // type check operator
                let optype = Type::TVar(self.fresh_type_var());
                let rs = self.typecheck_builtin(expr, &format!("op{}", op), &optype, rs)?;

                // type check operands
                let lhstype = Type::TVar(self.fresh_type_var());
                let rs = self.typecheck_expr(lhs, &lhstype, rs)?;
                let rhstype = Type::TVar(self.fresh_type_var());
                let rs = self.typecheck_expr(rhs, &rhstype, rs)?;

                // unification
                let rettype = Type::TVar(self.fresh_type_var());
                let rs = unify(&rettype, hint, rs).expect_type(hint, expr)?;
                unify(&Type::func2(lhstype, rhstype, rettype), &optype, rs).expect_nonempty(expr)
            }
            E::Unop(op, expr) => {
                let optype = match op.as_str() {
                    "-" => Type::func1(Type::Num, Type::Num),
                    "!" => Type::func1(Type::Bool, Type::Bool),
                    _   => panic!("Unrecognized unary operator")
                }.refresh_type_vars(&mut self.i);
                
                let exprtype = Type::TVar(self.fresh_type_var());
                let rs = self.typecheck_expr(expr, &exprtype, rs)?;

                let rettype = Type::TVar(self.fresh_type_var());
                let rs = unify(&rettype, hint, rs).expect_type(hint, expr)?;
                unify(&Type::func1(exprtype, rettype), &optype, rs).expect_nonempty(expr)
            }
            E::List(l) => {
                let innertype = Type::TVar(self.fresh_type_var());
                for item in l {
                    rs = self.typecheck_expr(item, &innertype, rs)?;
                }
                unify(&Type::list(innertype), hint, rs).expect_type(hint, expr)
            },
            E::Tuple(l) => {
                let mut ls: Vec<Type> = Vec::with_capacity(l.len());
                for expr in l {
                    let t = Type::TVar(self.fresh_type_var());
                    rs = self.typecheck_expr(expr, &t, rs)?;
                    ls.push(t);
                }
                unify(&Type::Tuple(ls), hint, rs).expect_type(hint, expr)
            },
            E::Matrix(_, _, v) => {
                for x in v {
                    rs = self.typecheck_expr(x, &Type::Num, rs)?;
                }
                unify(&Type::Mat, hint, rs).expect_type(hint, expr)
            },
            E::Bool(_) => unify(&Type::Bool, hint, rs).expect_type(hint, expr),
            E::Str(_) => unify(&Type::Str, hint, rs).expect_type(hint, expr),
            E::Int(_) => unify(&Type::Num, hint, rs).expect_type(hint, expr),
            E::Num(_) => unify(&Type::Num, hint, rs).expect_type(hint, expr),
            E::IntImag(_) => unify(&Type::Num, hint, rs).expect_type(hint, expr),
            E::FloatImag(_) => unify(&Type::Num, hint, rs).expect_type(hint, expr),
            E::Id(x) if x.eq("_") => Ok(rs),
            E::Id(x) => {
                if let Some(t) = self.get(x) { unify(&t, hint, rs).expect_type(hint, expr) }
                else { self.typecheck_builtin(expr, &x, hint, rs) }
            }
        }
    }

    fn typecheck_builtin(&mut self, expr: &ExprA, name: &str, hint: &Type, rs: RS) -> TypeCheckResult<RS> {
        type_check_builtin(name).into_iter().filter_map(|typ| {
            unify(&typ.refresh_type_vars(&mut self.i), hint, rs.clone()).to_option()
        }).flatten().collect::<RS>().expect_nonempty(expr)
    }

    fn typecheck_dot(&mut self, expr: &ExprA, lhs: &ExprA, rhs: &ExprA, hint: &Type, rs: RS) -> TypeCheckResult<RS> {
        let lhstype = Type::TVar(self.fresh_type_var());
        let rhstype = Type::TVar(self.fresh_type_var());
        let rs = self.typecheck_expr(lhs, &lhstype, rs)?;
        let rs = self.typecheck_expr(rhs, &rhstype, rs)?;

        rs.traverse(expr, |r| match (lhstype.clone().refine(&r), rhstype.clone().refine(&r)) {
            (Type::TVar(_), _) => None,
            (Type::Func(args1, ret1), Type::Func(mut args2, ret2)) => {
                if args2.len() != 1 { return None; }
                let rs = unify(&*ret1, &args2.pop().unwrap(), RS::pure(r));
                unify(&Type::Func(args1, ret2), hint, rs).to_option()
            }
            (lhstype, Type::Func(mut args2, ret2)) => {
                if args2.len() != 1 { return None; }
                let rs = unify(&lhstype, &args2.pop().unwrap(), RS::pure(r));
                unify(&*ret2, hint, rs).to_option()
            }
            (lhstype, rhstype) => {
                let rettype = Type::TVar(self.fresh_type_var());
                let rs = unify(&rettype, hint, RS::pure(r));
                unify(&rhstype, &Type::func1(lhstype.clone(), rettype), rs).to_option()
            }
        })
    }

    fn typecheck_dollar(&mut self, expr: &ExprA, lhs: &ExprA, rhs: &ExprA, hint: &Type, rs: RS) -> TypeCheckResult<RS> {
        let lhstype = Type::TVar(self.fresh_type_var());
        let rhstype = Type::TVar(self.fresh_type_var());
        let rs = self.typecheck_expr(lhs, &lhstype, rs)?;
        let rs = self.typecheck_expr(rhs, &rhstype, rs)?;

        rs.traverse(expr, |r| match (lhstype.clone().refine(&r), rhstype.clone().refine(&r)) {
            (Type::Func(args1, ret1), Type::Func(args2, ret2)) => {
                if let Type::Tuple(ret1) = *ret1 {
                    if args2.len() != ret1.len() { return None; }
                    let rs = unify_pairs(&ret1, &args2, RS::pure(r));
                    unify(&Type::Func(args1, ret2), hint, rs).to_option()
                } else { None }
            }
            (Type::Tuple(lhs), Type::Func(args2, ret2)) => {
                if args2.len() != lhs.len() { return None; }
                let rs = unify_pairs(&lhs, &args2, RS::pure(r));
                unify(&ret2, hint, rs).to_option()
            }
            (Type::Func(args1, ret1), Type::TVar(v)) => {
                if let Type::Tuple(ret1) = *ret1 {
                    let ret2 = Box::new(Type::TVar(self.fresh_type_var()));
                    let rs = unify(&Type::TVar(v), &Type::Func(ret1.clone(), ret2.clone()), RS::pure(r));
                    unify(&Type::Func(args1, ret2), hint, rs).to_option()
                } else { None }
            }
            (Type::Tuple(ret1), Type::TVar(v)) => {
                let ret2 = Type::TVar(self.fresh_type_var());
                let rs = unify(&Type::TVar(v), &Type::Func(ret1.clone(), Box::new(ret2.clone())), RS::pure(r));
                unify(&ret2, hint, rs).to_option()
            }
            _ => None
        })
    }

    /// Make sure the indices have type `Num`
    fn typecheck_slice(&mut self, slice: &AstSlice, mut rs: RS) -> TypeCheckResult<RS> {
        match slice {
            AstSlice::Single(idx) => self.typecheck_expr(idx, &Type::Num, rs),
            AstSlice::Range(idx1, idx2) => {
                if let Some(idx1) = idx1 { rs = self.typecheck_expr(idx1, &Type::Num, rs)?; }
                if let Some(idx2) = idx2 { rs = self.typecheck_expr(idx2, &Type::Num, rs)?; }
                Ok(rs)
            }
        }
    }

    fn fresh_type_var(&mut self) -> String {
        let num_primes = (self.i / 26) as usize;
        let letter = char::from_u32((self.i % 26) + 0x41).unwrap();
        self.i += 1;
        format!("{}{}", letter, "'".repeat(num_primes))
    }
}