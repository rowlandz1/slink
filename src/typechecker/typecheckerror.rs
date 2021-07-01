/* typecheckererror.rs
 *
 * Defines type-checking errors and their string representations.
 */

use super::Type;

#[derive(Debug)]
pub enum TypeError {
    UnknownIdentifier(String),
    UnificationFailed(Type, Type),
    CannotTypeCheckDot,
    CannotTypeCheckDollar,
    ArityMistmatch,
    ExpectedTuple,
    ExpectedFunction(Type),
    ArgumentListTooLong,
    ConflictingBindings(String, Type, Type),
}

pub type TypeCheckResult<T> = std::result::Result<T, TypeError>;

impl ToString for TypeError {
    fn to_string(&self) -> String {
        match self {
            TypeError::UnknownIdentifier(id) => {
                format!("TypeError: unknown identifier '{}'", id)
            }
            TypeError::UnificationFailed(t1, t2) => {
                format!("TypeError: cannot unify types {} and {}", t1.to_string(), t2.to_string())
            }
            TypeError::CannotTypeCheckDot => {
                format!("TypeError: cannot determine the type of dot expression")
            }
            TypeError::CannotTypeCheckDollar => {
                format!("TypeError: cannot determine the type of dollar expression")
            }
            TypeError::ArityMistmatch => {
                format!("TypeError: arity mismatch")
            }
            TypeError::ExpectedTuple => {
                format!("TypeError: expected tuple")
            }
            TypeError::ExpectedFunction(t) => {
                format!("TypeError: expected function type {}", t.to_string())
            }
            TypeError::ArgumentListTooLong => {
                format!("TypeError: argument list is too long")
            }
            TypeError::ConflictingBindings(v, t1, t2) => {
                format!("TypeError: conflicting bindings for {}: {} and {}", v, t1.to_string(), t2.to_string())
            }
        }
    }
}