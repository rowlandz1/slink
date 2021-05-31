/* error.rs
 *
 * Defines evaluation errors and error messages.
 */

#[derive(Debug)]
pub enum EvalError {
    InvalidMatrixShape,
    IncompatibleMatrixShapes,
    NoninvertableMatrix,
    ArityMismatch,
    TypeMismatch,
    TypeConversionError,
    UndefinedIdentifier(String),
    OutOfRange,
    InvalidSlice,
    DivideByZero,
    QuestionMarkMacroArg,
    InvalidKeywordArgument,
    InResolvedExpr(Box<EvalError>, String),
}

pub type EvalResult<T> = std::result::Result<T, EvalError>;

impl ToString for EvalError {
    fn to_string(&self) -> String {
        match self {
            EvalError::InvalidMatrixShape => {
                format!("Error: invalid matrix shape")
            }
            EvalError::IncompatibleMatrixShapes => {
                format!("Error: incompatible matrix shapes")
            }
            EvalError::NoninvertableMatrix => {
                format!("Error: noninvertable matrix")
            }
            EvalError::ArityMismatch => {
                format!("Error: arity mismatch")
            }
            EvalError::TypeMismatch => {
                format!("Error: type mismatch")
            }
            EvalError::TypeConversionError => {
                format!("Error: type conversion error")
            }
            EvalError::UndefinedIdentifier(s) => {
                format!("Error: undefined identifier '{}'", s)
            }
            EvalError::OutOfRange => {
                format!("Error: out of range")
            }
            EvalError::InvalidSlice => {
                format!("Error: invalid slice")
            }
            EvalError::DivideByZero => {
                format!("Error: divide by zero")
            }
            EvalError::QuestionMarkMacroArg => {
                format!("Error: question mark syntax cannot be used when calling a macro")
            }
            EvalError::InvalidKeywordArgument => {
                format!("Error: invalid keyword argument")
            }
            EvalError::InResolvedExpr(innererr, f) => {
                format!("In resolved expression of {}\n{}", f, innererr.to_string())
            }
        }
    }
}
