/* error.rs
 *
 * Defines parsing errors, evaluation errors, and the corresponding error messages
 */

use std::fmt;

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
pub struct TypeError {
    msg: String,
    start: Option<(usize, usize)>,
    end: Option<(usize, usize)>,
    firstline: usize,
    lines: Vec<String>
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

impl TypeError {
    pub fn new(msg: String) -> TypeError {
        TypeError{msg, start: None, end: None, firstline: 0, lines: Vec::new()}
    }

    /// Sets `start` and `end` to the given values if previously unset.
    pub fn provide_span(mut self, start: (usize, usize), end: (usize,usize)) -> TypeError {
        if self.start.is_none() && self.end.is_none() {
            self.start = Some(start); self.end = Some(end);
        }
        self 
    }

    /// Fills in the `lines` and `firstline` fields based on the provided code source
    pub fn supply_src(mut self, src: &str) -> TypeError {
        let (start, end) = (self.start.unwrap(), self.end.unwrap());
        let numlines = end.0 - start.0 + 1;
        assert!(numlines > 0);
        let lines: Vec<String> = src.lines().skip(start.0 - 1).take(numlines).map(String::from).collect();
        self.firstline = start.0;
        self.lines = lines;
        self
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (start, end) = (self.start.unwrap(), self.end.unwrap());
        write!(f, "\x1B[1;31mType checking error\x1b[m: {}\n\n", self.msg)?;
        if self.start.unwrap().0 == self.end.unwrap().0 {
            let line = &self.lines[0];
            let a = &line[..start.1-1];
            let b = &line[start.1-1..end.1-1];
            let c = &line[end.1-1..];
            writeln!(f, "{:4} | {}\x1B[1;94m{}\x1B[m{}", self.firstline, a, b, c)?;
            writeln!(f, "     | {}\x1B[1;94m{}\x1B[m", " ".repeat(a.len()), "~".repeat(b.len()))?;
        } else {
            writeln!(f, "     |{}v", " ".repeat(start.1))?;
            let mut currentline = self.firstline;
            for line in &self.lines {
                if currentline == start.0 {
                    writeln!(f, "{:4} | {}\x1B[1;94m{}\x1B[m", currentline, &line[..start.1-1], &line[start.1-1..])?;
                } else if currentline == end.0 {
                    writeln!(f, "{:4} | \x1B[1;94m{}\x1B[m{}", currentline, &line[..end.1-1], &line[end.1-1..])?;
                } else {
                    writeln!(f, "{:4} | \x1B[1;94m{}\x1B[m", currentline, line)?;
                }
                currentline += 1;
            }
            writeln!(f, "     |{}^", " ".repeat(end.1))?;
        }
        Ok(())
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