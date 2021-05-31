/* typecheckererror.rs
 *
 * Defines type-checking errors and their string representations.
 */

use super::Type;

pub enum TypeError {
    UnknownIdentifier(String),
    UnificationFailed(Type, Type),
    ArgumentListTooLong,
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
            TypeError::ArgumentListTooLong => {
                format!("TypeError: argument list is too long")
            }
        }
    }
}