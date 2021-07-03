/* types/unifier.rs
 *
 * Defines type unification.
 */

use super::Type;
use super::typechecker::Refinements;

/// Adds bindings to the type refinement to make the two types equal.
pub fn unify(t1: &Type, t2: &Type, rs: Refinements) -> Refinements {
    match (t1, t2) {
        (Type::Any, _) |
        (_, Type::Any) => rs,
        (Type::TVar(v1), Type::TVar(v2)) if v1.eq(v2) => rs,
        (Type::TVar(v), t) |
        (t, Type::TVar(v)) => rs.into_iter().flat_map(|mut r| {
            let x = r.get(v);
            if let Some(t2) = x {
                unify(t, &t2.clone(), vec![r])
            } else if !t.clone().refine(&r).contains_var(v) {
                r.insert(v.clone(), t.clone());
                vec![r]
            } else { vec![] }
        }).collect::<Refinements>(),
        (Type::Func(args1, ret1), Type::Func(args2, ret2)) => {
            if args1.len() != args2.len() { return vec![]; }
            let rs = unify_pairs(args1, args2, rs);
            unify(ret1, ret2, rs)
        }
        (Type::List(a), Type::List(b)) => unify(a, b, rs),
        (Type::Tuple(types1), Type::Tuple(types2)) => if types1.len() == types2.len() {
            unify_pairs(types1, types2, rs)
        } else { vec![] }
        (t1, t2) => if t1 == t2 {
            rs
        } else { vec![] }
    }
}

/// Unify pairs of types specified by the two provided parallel vectors
pub fn unify_pairs(types1: &Vec<Type>, types2: &Vec<Type>, mut rs: Refinements) -> Refinements {
    if types1.len() != types2.len() { panic!("Error, cannot unify pairs if vectors are not the same size"); }
    for (t1, t2) in types1.iter().zip(types2.iter()) {
        rs = unify(t1, t2, rs);
    }
    rs
}