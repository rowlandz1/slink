/* types.rs
 *
 * Type definitions and type-checking algorithm.
 */

use std::collections::HashMap;
use crate::ast::{AstArg, AstExpr as E, AstStmt};
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
        let typ = match expr {
            E::Lambda(params, body) => {
                ta.push_new_frame(&params);
                let bodytype = self.type_check_helper(body, ta)?;
                Type::Func(ta.pop_frame(), Box::new(bodytype))
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
                    *rettype
                } else { unreachable!() }
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
                if let Some(t) = ta.get(x) { t }
                else if let Some(t) = self.var_types.get(x) { t.clone() }
                else { return Err(TypeError::UnknownIdentifier(x.clone())); }
            },
            _ => Type::Unknown
        };
        Ok(typ)
    }
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
            let new_assumption = Type::TVar(int2typevar(self.i));
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
                Ok(t2)
            }
            (Type::Unknown, _) | (_, Type::Unknown)=> Ok(Type::Unknown),
            (Type::Func(mut args1, ret1), Type::Func(mut args2, ret2)) => {
                if args1.len() != args2.len() { return Err(TypeError::ArgumentListTooLong); }
                args1.push(*ret1);
                args2.push(*ret2);
                let mut args3 = self.unify_pairs(args1, args2)?;
                let ret3 = Box::new(args3.pop().unwrap());
                Ok(Type::Func(args3, ret3))
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
        let ret = int2typevar(self.i);
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

    /// Renames type variables to be more visually appealing.
    pub fn normalize_type_var_names(&mut self) {
        self.normalize_helper(&mut 0, &mut HashMap::new());
    }

    fn normalize_helper(&mut self, i: &mut u32, h: &mut HashMap<String, String>) {
        match self {
            Type::TVar(v) => {
                if let Some(r) = h.get(v) {
                    *v = r.clone();
                } else {
                    let newv = int2typevar(i.clone());
                    h.insert((*v).clone(), newv.clone());
                    *v = newv;
                    *i += 1;
                }
            }
            Type::Func(argtypes, rettype) => {
                for arg in argtypes {
                    arg.normalize_helper(i, h);
                }
                rettype.normalize_helper(i, h);
            }
            _ => {}
        }
    }
}

/// Converts a type variable (numeric representation) into its string representation
fn int2typevar(i: u32) -> String {
    let num_primes = (i / 26) as usize;
    let letter = char::from_u32((i % 26) + 0x41).unwrap();
    format!("{}{}", letter, "'".repeat(num_primes))
}
