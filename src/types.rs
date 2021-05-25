/* types.rs
 *
 * Type definitions and type-checking algorithm
 */

use std::collections::HashMap;
use crate::ast::{AstExpr, AstExprTree as E};
use crate::ast::AstArg;
use crate::error::{TypeError, TypeCheckResult};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Num,
    Matrix,
    Bool,
    String,
    List,
    Tuple,
    TVar(String),
    Func(Vec<Type>, Box<Type>),
    Any,
    Unknown,
}

/// Utility object for keeping track of type assumptions and generating
/// fresh type variables
pub struct TAssums {
    // A stack of frames. Each frame maps a variable name to a type assumption
    // and its position in the variable list at the time of being pushed.
    id_frames: Vec<HashMap<String, (usize, Type)>>,
    // Types in this stack are manipulated by unification.
    type_stack: Vec<Type>,
    i: u32,     // first unused type variable (integer representation)
}

impl TAssums {
    pub fn new() -> TAssums {
        TAssums{id_frames: Vec::new(), type_stack: Vec::new(), i: 0}
    }

    /// Introduces fresh type variables for each id and adds them to
    /// a new local type-assumption frame.
    pub fn push_new_frame(&mut self, ids: &[String]) {
        let mut hm: HashMap<String, (usize, Type)> = HashMap::new();
        for i in 0..ids.len() {
            let new_assumption = Type::TVar(TAssums::str_repr(self.i));
            self.i += 1;
            hm.insert(ids[i].clone(), (i, new_assumption));
        }
        self.id_frames.push(hm);
    }

    /// Pops the top frame from the stack. The type assumptions are returned in the
    /// same order as the variable list that was passed to push_new_frame.
    pub fn pop_frame(&mut self) -> Vec<Type> {
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

    /// Push a type onto the type stack. Types on this stack are manipulated by unification
    pub fn push_type(&mut self, typ: Type) { self.type_stack.push(typ); }
    /// Pop a type from the type stack. The stack must be non-empty.
    pub fn pop_type(&mut self) -> Type { self.type_stack.pop().unwrap() }

    /// Lookup the assumed type for the given id.
    pub fn get(&self, id: &String) -> Option<Type> {
        for frame in self.id_frames.iter().rev() {
            if let Some((_, typ)) = frame.get(id) { return Some(typ.clone()); }
        }
        None
    }

    /// Type unification. Returns the intersection of two types.
    /// A UnificationFailed error is returned if the intersection
    /// is empty.
    pub fn unify(&mut self, t1: Type, t2: Type) -> TypeCheckResult<Type> {
        println!("Unifying {} and {}", t1.to_string(), t2.to_string());
        match (t1, t2) {
            (Type::Any, t2) |
            (t2, Type::Any) => Ok(t2),
            (Type::TVar(v1), Type::TVar(v2)) => {
                self.replace_type_var(v1, Type::TVar(v2.clone()));
                Ok(Type::TVar(v2))
            }
            (Type::TVar(v), t2) |
            (t2, Type::TVar(v)) => {
                if t2.contains_var(&v) {
                    return Err(TypeError::UnificationFailed(Type::TVar(v), t2));
                }
                self.replace_type_var(v, t2.clone());
                println!("---Result: {}", t2.to_string());
                Ok(t2)
            }
            (Type::Unknown, _) | (_, Type::Unknown)=> Ok(Type::Unknown),
            (Type::Func(mut args1, ret1), Type::Func(mut args2, ret2)) => {
                if args1.len() == args2.len() {
                    args1.push(*ret1);
                    args2.push(*ret2);
                    let mut args3 = self.unify_pairs(args1, args2)?;
                    let ret3 = Box::new(args3.pop().unwrap());
                    Ok(Type::Func(args3, ret3))
                } else if args1.len() > args2.len() {
                    let args1_mod = args1.split_off(args1.len() - args2.len());
                    let ret1_mod = Box::new(Type::Func(args1, ret1));
                    self.unify(Type::Func(args1_mod, ret1_mod), Type::Func(args2, ret2))
                    // let mut args3: Vec<Type> = Vec::new();
                    // for (arg1, arg2) in args1_mod.into_iter().zip(args2) {
                    //     args3.push(self.unify(arg1, arg2)?);
                    // }
                    // let ret3 = self.unify(ret1_mod, *ret2)?;
                    // Ok(Type::Func(args3, Box::new(ret3)))
                } else {
                    // IMPORTANT: the argument list should not always just be switched around!
                    self.unify(Type::Func(args2, ret2), Type::Func(args1, ret1))
                }
            }
            (t1, t2) => if t1 == t2 { Ok(t1) }
                               else { Err(TypeError::UnificationFailed(t1, t2)) }
        }
    }

    /// Unify pairs of types. The pairs are specified as parallel vectors (that must have
    /// the same length).
    pub fn unify_pairs(&mut self, mut types1: Vec<Type>, mut types2: Vec<Type>) -> TypeCheckResult<Vec<Type>> {
        if types1.len() != types2.len() { panic!("Error, cannot unify pairs if vectors are not the same size"); }
        let numpairs = types1.len();
        let origlen = self.type_stack.len();
        let base = origlen + numpairs - 1;
        self.type_stack.append(&mut types1);
        self.type_stack.append(&mut types2);

        for i in 0..numpairs {
            let t1 = self.type_stack.get(base - i).unwrap().clone();
            let t2 = self.type_stack.pop().unwrap();
            self.unify(t1, t2)?;
        }

        Ok(self.type_stack.split_off(origlen))
    }

    /// Returns a fresh type variable.
    pub fn fresh_type_var(&mut self) -> String {
        let ret = TAssums::str_repr(self.i);
        self.i += 1;
        ret
    }

    fn replace_type_var(&mut self, tvar: String, with: Type) {
        for hm in self.id_frames.iter_mut() {
            for (_, t) in hm.values_mut() {
                t.replace_type_var(&tvar, &with);
            }
        }
        for typ in self.type_stack.iter_mut() {
            typ.replace_type_var(&tvar, &with);
        }
    }

    /// Converts a type variable (numeric representation) into its string representation
    fn str_repr(i: u32) -> String {
        let num_primes = (i / 26) as usize;
        let letter = char::from_u32((i % 26) + 0x41).unwrap();
        format!("{}{}", letter, "'".repeat(num_primes))
    }
}

impl AstExpr {
    /// Performs type checking and type annotation for expressions.
    /// `ta`: a set of type assumptions for variables and will be modified as
    /// type refinement happens
    pub fn type_check(&mut self, ta: &mut TAssums) -> TypeCheckResult<Type> {
        self.tree.type_check(ta)
    }
}

impl E {
    pub fn type_check(&mut self, ta: &mut TAssums) -> TypeCheckResult<Type> {
        let typ = match self {
            E::Lambda(params, body) => {
                ta.push_new_frame(&params);
                let bodytype = body.type_check(ta)?;
                Type::Func(ta.pop_frame(), Box::new(bodytype))
            }
            E::FunApp(f, args) => {
                let mut is_question_mark: Vec<bool> = Vec::new();
                for arg in args.iter_mut().rev() {
                    match arg {
                        AstArg::Question => {
                            let fv = ta.fresh_type_var();
                            ta.push_type(Type::Any);
                            is_question_mark.push(true);
                        }
                        AstArg::Expr(e) => {
                            let typ = e.type_check(ta)?;
                            ta.push_type(typ);
                            is_question_mark.push(false);
                        }
                    }
                }
                let ftype = f.type_check(ta)?;

                let mut argtypes: Vec<Type> = Vec::new();
                for _ in 0..args.len() {
                    argtypes.push(ta.pop_type());
                }

                let type2 = Type::Func(argtypes, Box::new(Type::TVar(ta.fresh_type_var())));
                let unified = ta.unify(ftype, type2)?;
                if let Type::Func(argtypes, rettype) = unified {
                    if argtypes.len() != is_question_mark.len() {
                        panic!("{} vs. {}", argtypes.len(), is_question_mark.len());
                    }
                    let mut newargtypes: Vec<Type> = Vec::new();
                    for (argtype, q) in argtypes.into_iter().zip(is_question_mark) {
                        if q { newargtypes.push(argtype); }
                    }
                    if newargtypes.len() > 0 {
                        Type::Func(newargtypes, rettype)
                    } else {
                        *rettype
                    }
                } else { panic!("Error, unified type wasn't a function"); }
            }
            E::Matrix(_, _, _) => Type::Matrix,
            E::List(_) => Type::List,
            E::Tuple(_) => Type::Tuple,
            E::Bool(_) => Type::Bool,
            E::Str(_) => Type::String,
            E::Int(_) => Type::Num,
            E::Num(_) => Type::Num,
            E::IntImag(_) => Type::Num,
            E::FloatImag(_) => Type::Num,
            E::Id(x) => {
                match ta.get(x) {
                    Some(t) => t,
                    None => return Err(TypeError::UnknownIdentifier(x.clone()))
                }
            },
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
            Type::TVar(s) => String::from(s),
            Type::Func(v, r) => {
                let params: Vec<String> = v.iter().map(|t| t.to_string()).collect();
                format!("({}) -> {}", params.join(", "), r.to_string())
            }
            Type::Any => String::from("?"),
            Type::Unknown => String::from("Unknown"),
        }
    }
}

impl Type {
    pub fn replace_type_var(&mut self, tvar: &String, with: &Type) {
        match self {
            Type::TVar(v) => {
                if v == tvar { *self = with.clone(); }
            }
            Type::Func(args, ret) => {
                for arg in args {
                    arg.replace_type_var(tvar, with);
                }
                //args.iter_mut().map(|arg| arg.replace_type_var(tvar, with));
                ret.replace_type_var(tvar, with);
            }
            _ => {}
        }
    }

    pub fn contains_var(&self, tvar: &String) -> bool {
        match self {
            Type::TVar(v) => v == tvar,
            Type::Func(args, ret) => ret.contains_var(tvar) || args.iter().any(|arg| arg.contains_var(tvar)),
            _ => false
        }
    }
}
