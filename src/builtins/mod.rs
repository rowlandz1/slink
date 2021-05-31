/* builtins/mod.rs
 *
 * Definitions of predefined constants, functions, and macros.
 */

mod builtintypes;
mod internals;
mod macros;

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

/// Returns the type of a builtin construct.
pub fn get_builtin_type(name: &str) -> Option<Type> {
    builtintypes::get_builtin_type(name)
}

pub fn get_macro(name: &String) -> Option<SciVal> {
    macros::get_macro(name.clone())
}

// Applies the arguments to the internal function.
// NOTE: Arity mismatches need to be checked BEFORE calling this function.
pub fn eval_builtin_function(name: String, args: HashMap<String, SciVal>) -> EvalResult<SciVal> {
    internals::apply_to_internal(name, args)
}

pub fn eval_macro(name: String, args: Vec<SciVal>) -> EvalResult<SciVal> {
    macros::apply_to_macro(name, args)
}