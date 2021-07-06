/* error.rs
 *
 * Defines parsing errors, evaluation errors, and the corresponding error messages
 */

use std::fmt;
use crate::ast::ExprA;

#[derive(Debug)]
pub enum ParserError {
    PestError(pest::error::Error<crate::parser::Rule>),
    NonRectangularMatrix,
    RepeatedParam,
    RepeatedTypeParam,
    InvalidTypeConstructor(String),
    InvalidAtomicType(String),
}
pub type ParserResult<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
pub enum TypeError {
    ExpectedOne,
    InferenceFailed(ExprA)
}
pub type TypeCheckResult<T> = std::result::Result<T, TypeError>;

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
    InvalidKeywordArgument,
    NothingToUnpack,
    InResolvedExpr(Box<EvalError>, String),
}
pub type EvalResult<T> = std::result::Result<T, EvalError>;

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parsing error: ")?;
        match self {
            Self::PestError(err) => write!(f, "{}", err),
            Self::NonRectangularMatrix => write!(f, "Non-rectangular matrix."),
            Self::RepeatedParam => write!(f, "Repeated parameter."),
            Self::RepeatedTypeParam => write!(f, "Repeated type parameter."),
            Self::InvalidTypeConstructor(t) => write!(f, "Invalid type constructor '{}'", t),
            Self::InvalidAtomicType(t) => write!(f, "Invalid atomic type '{}'", t),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type checking error: ")?;
        match self {
            Self::ExpectedOne => write!(f, "expected a single type"),
            Self::InferenceFailed(expr) => write!(f, "type inference failed on expression {:?}", expr)
        }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Evaluation error: ")?;
        match self {
            EvalError::InvalidMatrixShape => {
                write!(f, "Error: invalid matrix shape")
            }
            EvalError::IncompatibleMatrixShapes => {
                write!(f, "Error: incompatible matrix shapes")
            }
            EvalError::NoninvertableMatrix => {
                write!(f, "Error: noninvertable matrix")
            }
            EvalError::ArityMismatch => {
                write!(f, "Error: arity mismatch")
            }
            EvalError::TypeMismatch => {
                write!(f, "Error: type mismatch")
            }
            EvalError::TypeConversionError => {
                write!(f, "Error: type conversion error")
            }
            EvalError::UndefinedIdentifier(s) => {
                write!(f, "Error: undefined identifier '{}'", s)
            }
            EvalError::OutOfRange => {
                write!(f, "Error: out of range")
            }
            EvalError::InvalidSlice => {
                write!(f, "Error: invalid slice")
            }
            EvalError::DivideByZero => {
                write!(f, "Error: divide by zero")
            }
            EvalError::InvalidKeywordArgument => {
                write!(f, "Error: invalid keyword argument")
            }
            EvalError::NothingToUnpack => {
                write!(f, "Error: cannot unpack a non-tuple value")
            }
            EvalError::InResolvedExpr(innererr, func) => {
                write!(f, "In resolved expression of {}\n{}", func, innererr)
            }
        }
    }
}