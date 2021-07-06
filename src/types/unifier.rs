/* types/unifier.rs
 *
 * Defines type unification.
 */

use super::Type;
use super::Refinements as RS;

/// Adds bindings to the type refinement to make the two types equal.
pub fn unify(t1: &Type, t2: &Type, rs: RS) -> RS {
    match (t1, t2) {
        (Type::Any, _) |
        (_, Type::Any) => rs,
        (Type::TVar(v1), Type::TVar(v2)) if v1.eq(v2) => rs,
        (Type::TVar(v), t) |
        (t, Type::TVar(v)) => rs.into_iter().flat_map(|mut r| {
            let x = r.get(v);
            if let Some(t2) = x {
                unify(t, &t2.clone(), RS::pure(r))
            } else if !t.clone().refine(&r).contains_var(v) {
                r.insert(v.clone(), t.clone());
                RS::pure(r)
            } else { RS::fail() }
        }).collect::<RS>(),
        (Type::Func(args1, ret1), Type::Func(args2, ret2)) => if args1.len() == args2.len() {
            let rs = unify_pairs(args1, args2, rs);
            unify(ret1, ret2, rs)
        } else if args1.len() > args2.len() {
            let diff = args1.len() - args2.len();
            let rs = unify_pairs(&args1[diff..], args2, rs);
            unify(&Type::Func(args1[..diff].to_vec(), ret1.clone()), ret2, rs)
        } else {
            let diff = args2.len() - args1.len();
            let rs = unify_pairs(args1, &args2[diff..], rs);
            unify(ret1, &Type::Func(args2[..diff].to_vec(), ret2.clone()), rs)
        }
        (Type::List(a), Type::List(b)) => unify(a, b, rs),
        (Type::Tuple(types1), Type::Tuple(types2)) => if types1.len() == types2.len() {
            unify_pairs(types1, types2, rs)
        } else { RS::fail() }
        (t1, t2) => if t1 == t2 {
            rs
        } else { RS::fail() }
    }
}

/// Unify pairs of types specified by the two provided parallel vectors
pub fn unify_pairs(types1: &[Type], types2: &[Type], mut rs: RS) -> RS {
    if types1.len() != types2.len() { panic!("Error, cannot unify pairs if vectors are not the same size"); }
    for (t1, t2) in types1.iter().zip(types2.iter()) {
        rs = unify(t1, t2, rs);
    }
    rs
}