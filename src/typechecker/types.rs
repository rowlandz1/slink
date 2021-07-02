/* types.rs
 *
 * Type definitions and type-checking algorithm.
 */

use std::collections::HashMap;
use super::{Type, TypeRefinement};
use super::typecheckerror::{TypeError, TypeCheckResult};

impl TypeRefinement {
    pub fn new() -> TypeRefinement {
        TypeRefinement{bindings: vec![HashMap::new()]}
    }

    pub fn pure(binding: HashMap<String, Type>) -> TypeRefinement {
        TypeRefinement{bindings: vec![binding]}
    }

    /// Apply a unify-esque function on each possible binding
    pub fn flat_traverse<F>(self, mut f: F) -> TypeCheckResult<TypeRefinement>
    where F: FnMut(HashMap<String,Type>) -> TypeCheckResult<TypeRefinement> {
        let bindings = self.bindings.into_iter().flat_map(|b| match f(b) {
            Ok(rf) => rf.bindings,
            Err(_) => vec![],
        }).collect::<Vec<HashMap<String, Type>>>();
        match bindings.len() {
            0 => Err(TypeError::FlatTraverseFailed),
            _ => Ok(TypeRefinement{bindings})
        }
    }

    pub fn expect_one(mut self) -> TypeCheckResult<HashMap<String,Type>> {
        match self.bindings.len() {
            1 => Ok(self.bindings.pop().unwrap()),
            _ => Err(TypeError::ExpectedOne)
        }
    }
}

pub fn unify(t1: &Type, t2: &Type, rf: TypeRefinement) -> TypeCheckResult<TypeRefinement> {
    let bindings = rf.bindings.into_iter().flat_map(|rf| match unify_one(t1, t2, rf) {
        Ok(binding) => vec![binding],
        Err(_) => vec![],
    }).collect::<Vec<HashMap<String, Type>>>();
    match bindings.len() {
        0 => Err(TypeError::FlatTraverseFailed),
        _ => Ok(TypeRefinement{bindings})
    }
}

/// Adds bindings to the type refinement to make the two types equal.
fn unify_one(t1: &Type, t2: &Type, mut rf: HashMap<String, Type>) -> TypeCheckResult<HashMap<String, Type>> {
    match (t1, t2) {
        (Type::Any, _) |
        (_, Type::Any) => Ok(rf),
        (Type::TVar(v1), Type::TVar(v2)) if v1.eq(v2) => Ok(rf),
        (Type::TVar(v), t) |
        (t, Type::TVar(v)) => {
            let x = rf.get(v);
            if let Some(t2) = x {
                unify_one(t, &t2.clone(), rf)
            } else if !t.clone().replace_type_vars(&rf).contains_var(v) {
                rf.insert(v.clone(), t.clone());
                Ok(rf)
            } else { Err(TypeError::UnificationFailed(t1.clone().replace_type_vars(&rf), t2.clone().replace_type_vars(&rf))) }
        }
        (Type::Func(args1, ret1), Type::Func(args2, ret2)) => {
            if args1.len() != args2.len() { return Err(TypeError::ArgumentListTooLong); }
            let rf = unify_pairs(args1, args2, rf)?;
            unify_one(ret1, ret2, rf)
        }
        (Type::List(a), Type::List(b)) => unify_one(a, b, rf),
        (Type::Tuple(types1), Type::Tuple(types2)) => if types1.len() == types2.len() {
            unify_pairs(types1, types2, rf)
        } else { Err(TypeError::UnificationFailed(Type::Tuple(types1.clone()), Type::Tuple(types2.clone()))) }
        (t1, t2) => if t1 == t2 {
            Ok(rf)
        } else { Err(TypeError::UnificationFailed(t1.clone(), t2.clone())) }
    }
}

pub fn unify_pairs(types1: &Vec<Type>, types2: &Vec<Type>, mut rf: HashMap<String, Type>) -> TypeCheckResult<HashMap<String, Type>> {
    if types1.len() != types2.len() { panic!("Error, cannot unify pairs if vectors are not the same size"); }
    for (t1, t2) in types1.iter().zip(types2.iter()) {
        rf = unify_one(t1, t2, rf)?;
    }
    Ok(rf)
}

impl Type {
    // Resolves all bound type variables in the given type. Doesn't check for circular bindings.
    pub fn replace_type_vars(self, replacements: &HashMap<String, Type>) -> Type {
        match self {
            Type::TVar(v) => if let Some(t) = replacements.get(&v) {
                t.clone().replace_type_vars(replacements)
            } else { Type::TVar(v) }
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