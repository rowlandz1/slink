/* builtins/mod.rs
 *
 * Definitions of predefined constants, functions, and macros.
 */

mod builtintypes;
mod internals;

use std::collections::HashMap;
use crate::error::EvalResult;
use crate::value::SciVal;
use crate::number::Number::Float;
use crate::typechecker::Type;

pub fn get_builtin(name: &String) -> Option<SciVal> {
    match name.as_str() {
        "pi" => { return Some(SciVal::Number(Float(std::f64::consts::PI))); }
        "e" => { return Some(SciVal::Number(Float(std::f64::consts::E))); }
        _ => {}
    }
    internals::get_builtin_function(&name)
}

/// Get types of a builtin construct
pub fn type_check_builtin(name: &str) -> Option<Vec<Type>> {
    let types = builtintypes::type_check_builtin(name);
    if types.len() == 0 { None } else { Some(types) }
}

// Applies the arguments to the internal function.
// NOTE: Arity mismatches need to be checked BEFORE calling this function.
pub fn eval_builtin_function(name: String, args: HashMap<String, SciVal>) -> EvalResult<SciVal> {
    internals::apply_to_internal(name, args)
}