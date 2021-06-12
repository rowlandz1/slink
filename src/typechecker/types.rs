/* types.rs
 *
 * Type definitions and type-checking algorithm.
 */

use std::collections::HashMap;
use super::Type;
use super::typecheckerror::{TypeError, TypeCheckResult};

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
    /// Pop n types at once. Types that were pushed first will be at start of returned vector.
    pub fn pop_types(&mut self, n: usize) -> Vec<Type> { let at = self.type_stack.len() - n; self.type_stack.split_off(at) }

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
            (Type::List(a), Type::List(b)) => {
                let a = self.unify(*a, *b)?;
                Ok(Type::list(a))
            }
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() { return Err(TypeError::UnificationFailed(Type::Tuple(types1), Type::Tuple(types2))); }
                Ok(Type::Tuple(self.unify_pairs(types1, types2)?))
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

    /// Renames type variables to be more visually appealing.
    pub fn refresh_type_vars(&mut self, typ: &mut Type) {
        self.refresh_helper(typ, &mut HashMap::new());
    }

    fn refresh_helper(&mut self, typ: &mut Type, h: &mut HashMap<String, String>) {
        match typ {
            Type::TVar(v) => {
                if let Some(r) = h.get(v) {
                    *v = r.clone();
                } else {
                    let newv = self.fresh_type_var();
                    h.insert((*v).clone(), newv.clone());
                    *v = newv;
                }
            }
            Type::Func(argtypes, rettype) => {
                for arg in argtypes {
                    self.refresh_helper(arg, h);
                }
                self.refresh_helper(rettype, h);
            }
            Type::List(a) => self.refresh_helper(&mut **a, h),
            Type::Tuple(ts) => for t in ts { self.refresh_helper(t, h); }
            _ => {}
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
            Type::List(a) => format!("[{}]", a.to_string()),
            Type::TVar(s) => String::from(s),
            Type::Tuple(ts) => {
                format!("({})", ts.iter().map(Type::to_string).collect::<Vec<String>>().join(", "))
            }
            Type::Func(v, r) => {
                let params: Vec<String> = v.iter().map(Type::to_string).collect();
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
            Type::TVar(v) => if v == tvar { *self = with.clone(); },
            Type::List(a) => a.replace_type_var(tvar, with),
            Type::Tuple(ts) => for t in ts { t.replace_type_var(tvar, with); },
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
            Type::List(a) => a.contains_var(tvar),
            Type::Tuple(ts) => ts.iter().any(|t| t.contains_var(tvar)),
            Type::Func(args, ret) => ret.contains_var(tvar) || args.iter().any(|arg| arg.contains_var(tvar)),
            _ => false
        }
    }

    /// Renames type variables to be more visually appealing.
    pub fn normalize_type_var_names(&mut self) {
        TAssums::new().refresh_type_vars(self);
    }
}

/// Converts a type variable (numeric representation) into its string representation
fn int2typevar(i: u32) -> String {
    let num_primes = (i / 26) as usize;
    let letter = char::from_u32((i % 26) + 0x41).unwrap();
    format!("{}{}", letter, "'".repeat(num_primes))
}
