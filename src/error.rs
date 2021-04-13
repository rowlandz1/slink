#[derive(Debug)]
pub enum EvalError {
    InvalidMatrixShape,
    IncompatibleMatrixShapes,
    NoninvertableMatrix,
    IndexOutOfBounds,
    ArityMismatch,
    TypeMismatch,
    UndefinedIdentifier,
    OutOfRange,
}

pub type EvalResult<T> = std::result::Result<T, EvalError>;
