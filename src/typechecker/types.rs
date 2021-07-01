/* types.rs
 *
 * Type definitions and type-checking algorithm.
 */

use std::collections::HashMap;
use super::Type;
use super::typecheckerror::{TypeError, TypeCheckResult};

/// A mapping from type variables to more specific types
#[derive(Debug, Clone)]
pub struct TypeRefinement {
    bindings: HashMap<String, Type>
}

impl TypeRefinement {
    pub fn new() -> TypeRefinement {
        TypeRefinement{bindings: HashMap::new()}
    }

    /// Lookup a type-variable binding
    pub fn lookup(&self, v: &String) -> Option<&Type> {
        self.bindings.get(v)
    }

    /// Safely combine two type refinements. Returns ConflictingBindings on error.
    /// TODO! Check this logic
    pub fn combine(self, mut then: TypeRefinement) -> TypeCheckResult<TypeRefinement> {
        for (k, t1) in self.bindings.into_iter() {
            match then.bindings.get(&k) {
                Some(t2) => { then = unify(&t1, &t2.clone(), then)?; }
                None => { then.bindings.insert(k, t1); }
            };
        }
        Ok(then)
    }

    /// Resolves all bound type variables in the given type. Doesn't check for circular bindings.
    pub fn refine(&self, typ: Type) -> Type {
        match typ {
            Type::List(t) => Type::list(self.refine(*t)),
            Type::Tuple(ts) => Type::Tuple(ts.into_iter().map(|t| self.refine(t)).collect()),
            Type::Func(paramtypes, rettype) => Type::Func(
                paramtypes.into_iter().map(|t| self.refine(t)).collect(),
                Box::new(self.refine(*rettype))),
            Type::TVar(v) => if let Some(repl) = self.bindings.get(&v) {
                self.refine(repl.clone())
            } else { Type::TVar(v) }
            typ => typ
        }
    }
}

/// Adds bindings to the type refinement to make the two types equal.
pub fn unify(t1: &Type, t2: &Type, mut rf: TypeRefinement) -> TypeCheckResult<TypeRefinement> {
    match (t1, t2) {
        (Type::Any, _) |
        (_, Type::Any) => Ok(rf),
        (Type::Unknown, _) | (_, Type::Unknown) => Ok(rf),
        (Type::TVar(v1), Type::TVar(v2)) if v1.eq(v2) => Ok(rf),
        (Type::TVar(v), t) |
        (t, Type::TVar(v)) => {
            let x = rf.lookup(v);
            if let Some(t2) = x {
                unify(t, &t2.clone(), rf)
            } else if !rf.refine(t.clone()).contains_var(v) {
                rf.bindings.insert(v.clone(), t.clone());
                Ok(rf)
            } else { Err(TypeError::UnificationFailed(rf.refine(t1.clone()), rf.refine(t2.clone()))) }
        }
        (Type::Func(args1, ret1), Type::Func(args2, ret2)) => {
            if args1.len() != args2.len() { return Err(TypeError::ArgumentListTooLong); }
            let rf = unify_pairs(args1, args2, rf)?;
            unify(ret1, ret2, rf)
        }
        (Type::List(a), Type::List(b)) => unify(a, b, rf),
        (Type::Tuple(types1), Type::Tuple(types2)) => if types1.len() == types2.len() {
            unify_pairs(types1, types2, rf)
        } else { Err(TypeError::UnificationFailed(Type::Tuple(types1.clone()), Type::Tuple(types2.clone()))) }
        (t1, t2) => if t1 == t2 {
            Ok(rf)
        } else { Err(TypeError::UnificationFailed(t1.clone(), t2.clone())) }
    }
}

pub fn unify_pairs(types1: &Vec<Type>, types2: &Vec<Type>, mut rf: TypeRefinement) -> TypeCheckResult<TypeRefinement> {
    if types1.len() != types2.len() { panic!("Error, cannot unify pairs if vectors are not the same size"); }
    for (t1, t2) in types1.iter().zip(types2.iter()) {
        rf = unify(t1, t2, rf)?;
    }
    Ok(rf)
}

impl Type {
    pub fn replace_type_vars(self, replacements: &HashMap<String, Type>) -> Type {
        match self {
            Type::TVar(v) => if let Some(t) = replacements.get(&v) { t.clone() } else { Type::TVar(v) }
            Type::List(t) => Type::list(t.replace_type_vars(replacements)),
            Type::Tuple(ts) => Type::Tuple(ts.into_iter().map(|t| t.replace_type_vars(replacements)).collect()),
            Type::Func(args, ret) => Type::Func(
                args.into_iter().map(|t| t.replace_type_vars(replacements)).collect(),
                Box::new(ret.replace_type_vars(replacements))
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

/// Returns a fresh type variable.
pub fn fresh_type_var(i: &mut u32) -> String {
    let num_primes = (*i / 26) as usize;
    let letter = char::from_u32((*i % 26) + 0x41).unwrap();
    *i = *i + 1;
    format!("{}{}", letter, "'".repeat(num_primes))
}