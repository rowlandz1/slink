/* types/typecheckerror.rs
 *
 * Defines type-checking errors and their string representations.
 */

#[derive(Debug)]
pub enum TypeError {
    ExpectedOne,
}

pub type TypeCheckResult<T> = std::result::Result<T, TypeError>;

impl ToString for TypeError {
    fn to_string(&self) -> String {
        match self {
            TypeError::ExpectedOne => {
                format!("TypeError: expected a single type")
            }
        }
    }
}