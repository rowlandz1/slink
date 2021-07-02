/* typecheckererror.rs
 *
 * Defines type-checking errors and their string representations.
 */

use super::Type;

#[derive(Debug)]
pub enum TypeError {
    UnificationFailed(Type, Type),
    CannotTypeCheckDot,
    CannotTypeCheckDollar,
    ArityMistmatch,
    ExpectedTuple,
    ExpectedFunction(Type),
    ArgumentListTooLong,
    FlatTraverseFailed,
    ExpectedOne,
}

pub type TypeCheckResult<T> = std::result::Result<T, TypeError>;

impl ToString for TypeError {
    fn to_string(&self) -> String {
        match self {
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
            TypeError::FlatTraverseFailed => {
                format!("TypeError: flat traverse failed")
            }
            TypeError::ExpectedOne => {
                format!("TypeError: expected a single type")
            }
        }
    }
}