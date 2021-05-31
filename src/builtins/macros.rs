/* macros.rs
 *
 * Defines evaluation of macros.
 */

use crate::error::{EvalError, EvalResult};
use crate::number::Number::Int;
use crate::value::SciVal;
use crate::callable::Callable::Macro;

pub fn get_macro(name: String) -> Option<SciVal> {
    match name.as_str() {
        "fst!" |
        "list!" |
        "snd!" |
        "sum!" |
        "zip!" => Some(SciVal::Callable(Macro(name, None))),
        _ => None
    }
}

pub fn apply_to_macro(name: String, mut args: Vec<SciVal>) -> EvalResult<SciVal> {
    match name.as_str() {
        "list!" => Ok(SciVal::List(args)),
        "sum!" => {
            let mut sum: SciVal = SciVal::Number(Int(0));
            for arg in args {
                sum = (sum + arg)?;
            }
            Ok(sum)
        }
        "zip!" => {
            let mut newargs: Vec<Vec<SciVal>> = Vec::new();
            let mut tuplelen: usize = usize::MAX;
            for arg in args {
                if let SciVal::List(v) = arg {
                    if tuplelen > v.len() { tuplelen = v.len(); }
                    newargs.push(v);
                }
                else { return Err(EvalError::TypeMismatch); }
            }
            let mut args = newargs;
    
            let mut ret: Vec<SciVal> = Vec::new();
            for _ in 0..tuplelen {
                let mut tuple: Vec<SciVal> = Vec::new();
                for arg in args.iter_mut() {
                    tuple.push(arg.remove(0));
                }
                ret.push(SciVal::Tuple(tuple));
            }
            Ok(SciVal::List(ret))
        }
        "fst!" => if args.len() == 0 { Err(EvalError::ArityMismatch) }
                  else { Ok(args.remove(0)) }
        "snd!" => if args.len() < 2 { Err(EvalError::ArityMismatch) }
                  else { Ok(args.remove(1)) }
        _ => Err(EvalError::UndefinedIdentifier(name))
    }
}
