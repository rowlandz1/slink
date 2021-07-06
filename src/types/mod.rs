/* types/mod.rs
 *
 * Defines types type environments
 */

mod typechecker;
mod unifier;

use std::collections::HashMap;
use crate::ast::ExprA;
use crate::error::{TypeError, TypeCheckResult};

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

pub struct TypeEnv {
    // Mapping from global ids to the type of the expressions to which they resolve
    var_types: HashMap<String, Type>,
    // A stack of frames. Each frame maps a variable name to a type assumption
    // and its position in the variable list at the time of being pushed.
    id_frames: Vec<HashMap<String, (usize, Type)>>,
}

/// A mapping from type variables to more specific types
pub type Refinement = HashMap<String, Type>;

/// A collection of refinements used to handle function overloading
#[derive(Debug, Clone)]
pub struct Refinements(Vec<Refinement>);

impl Type {
    pub fn list(t: Type) -> Type { Type::List(Box::new(t)) }
    pub fn var(v: &str) -> Type { Type::TVar(String::from(v)) }
    pub fn func1(arg: Type, ret: Type) -> Type { Type::Func(vec![arg], Box::new(ret)) }
    pub fn func2(arg1: Type, arg2: Type, ret: Type) -> Type { Type::Func(vec![arg1, arg2], Box::new(ret)) }

    /// Resolves all bound type variables in the given type. Doesn't check for circular bindings.
    pub fn refine(self, r: &Refinement) -> Type {
        match self {
            Type::TVar(v) => if let Some(t) = r.get(&v) {
                t.clone().refine(r)
            } else { Type::TVar(v) }
            Type::List(t) => Type::list(t.refine(r)),
            Type::Tuple(ts) => Type::Tuple(ts.into_iter().map(|t| t.refine(r)).collect()),
            Type::Func(args, ret) => Type::Func(
                args.into_iter().map(|t| t.refine(r)).collect(),
                Box::new(ret.refine(r))
            ),
            t => t
        }
    }

    pub fn contains_var(&self, tvar: &String) -> bool {
        match self {
            Type::TVar(v) => v == tvar,
            Type::List(a) => a.contains_var(tvar),
            Type::Tuple(ts) => ts.iter().any(|t| t.contains_var(tvar)),
            Type::Func(args, ret) => ret.contains_var(tvar) || args.iter().any(|arg| arg.contains_var(tvar)),
            _ => false
        }
    }

    /// Replaces all type variables with symbols unused in the given TAssums context.
    pub fn refresh_type_vars(self, i: &mut u32) -> Type {
        self.refresh_helper(i, &mut HashMap::new())
    }

    fn refresh_helper(self, i: &mut u32, h: &mut HashMap<String, String>) -> Type {
        match self {
            Type::TVar(v) => {
                if let Some(r) = h.get(&v) {
                    Type::TVar(r.clone())
                } else {
                    let newv = fresh_type_var(i);
                    h.insert(v, newv.clone());
                    Type::TVar(newv)
                }
            }
            Type::Func(argtypes, rettype) => {
                let argtypes: Vec<Type> = argtypes.into_iter().map(|arg|{arg.refresh_helper(i, h)}).collect();
                let rettype = rettype.refresh_helper(i, h);
                Type::Func(argtypes, Box::new(rettype))
            }
            Type::List(a) => Type::list(a.refresh_helper(i, h)),
            Type::Tuple(ts) => Type::Tuple(ts.into_iter().map(|t|{t.refresh_helper(i, h)}).collect()),
            typ => typ
        }
    }

    /// Renames type variables to be more visually appealing.
    pub fn normalize_type_var_names(self) -> Type {
        self.refresh_helper(&mut 0, &mut HashMap::new())
    }
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

    /// Front end for expression type checking. Returns possible types for the given expression.
    /// Type names are normalized.
    pub fn get_expr_types(&mut self, expr: &ExprA) -> TypeCheckResult<Vec<Type>> {
        let mut i = 0;
        let typ = Type::TVar(fresh_type_var(&mut i));
        let rs = self.typecheck_expr(expr, &typ, &mut i, Refinements::new())?;
        
        Ok(rs.into_iter().map(|r| typ.clone().refine(&r).normalize_type_var_names()).collect::<Vec<Type>>())
    }
}

impl Refinements {
    pub fn new() -> Refinements { Refinements(vec![HashMap::new()]) }
    pub fn pure(r: Refinement) -> Refinements { Refinements(vec![r]) }
    pub fn fail() -> Refinements { Refinements(Vec::new()) }

    pub fn non_empty(self, expr: &ExprA) -> TypeCheckResult<Refinements> {
        if self.0.len() == 0 {
            Err(TypeError::InferenceFailed(expr.clone()))
        } else { Ok(self) }
    }

    pub fn to_option(self) -> Option<Refinements> {
        if self.0.len() == 0 { None } else { Some(self) }
    }

    /// Applies a function to each refinement. Successes (if any) are collected
    /// and returned. Otherwise an InferenceFailed error on `expr` is returned.
    pub fn traverse<F>(self, expr: &ExprA, f: F) -> TypeCheckResult<Refinements>
    where F: FnMut(Refinement) -> Option<Refinements> {
        self.into_iter().filter_map(f).flatten().collect::<Refinements>().non_empty(expr)
    }
}

impl IntoIterator for Refinements {
    type Item = Refinement;
    type IntoIter = std::vec::IntoIter<Refinement>;
    fn into_iter(self) -> Self::IntoIter { self.0.into_iter() }
}

impl std::iter::FromIterator<Refinement> for Refinements {
    fn from_iter<I: IntoIterator<Item=Refinement>>(iter: I) -> Refinements {
        Refinements(iter.into_iter().collect::<Vec<Refinement>>())
    }
}

/// Returns a fresh type variable.
pub fn fresh_type_var(i: &mut u32) -> String {
    let num_primes = (*i / 26) as usize;
    let letter = char::from_u32((*i % 26) + 0x41).unwrap();
    *i = *i + 1;
    format!("{}{}", letter, "'".repeat(num_primes))
}