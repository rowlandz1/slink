#[derive(Debug)]
pub enum EvalError {
    InvalidMatrixShape,
    IncompatibleMatrixShapes,
    NoninvertableMatrix,
    IndexOutOfBounds,
    ArityMismatch,
    TypeMismatch,
    UndefinedIdentifier(String),
    OutOfRange,
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
            EvalError::IndexOutOfBounds => {
                format!("Error: index out of bounds")
            }
            EvalError::ArityMismatch => {
                format!("Error: arity mismatch")
            }
            EvalError::TypeMismatch => {
                format!("Error: type mismatch")
            }
            EvalError::UndefinedIdentifier(s) => {
                format!("Error: undefined identifier '{}'", s)
            }
            EvalError::OutOfRange => {
                format!("Error: out of range")
            }
            EvalError::InResolvedExpr(innererr, f) => {
                format!("In resolved expression of {}\n{}", f, innererr.to_string())
            }
        }
    }
}