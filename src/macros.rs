use crate::error::*;
use crate::ast::SciVal;
use crate::number;

pub fn apply_to_macro(name: String, mut args: Vec<SciVal>) -> EvalResult<SciVal> {
    if name.eq("list!") {
        Ok(SciVal::List(args))
    } else if name.eq("sum!") {
        let mut sum: SciVal = SciVal::Number(number::Number::Int(0));
        for arg in args {
            sum = (sum + arg)?;
        }
        Ok(sum)
    } else if name.eq("fst!") {
        if args.len() == 0 { Err(EvalError::ArityMismatch) }
        else { Ok(args.remove(0)) }
    } else if name.eq("snd!") {
        if args.len() < 2 { Err(EvalError::ArityMismatch) }
        else { Ok(args.remove(1)) }
    }
    else { Err(EvalError::UndefinedIdentifier(name)) }
}
