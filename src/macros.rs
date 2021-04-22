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
    } else if name.eq("zip!") {
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
    } else if name.eq("fst!") {
        if args.len() == 0 { Err(EvalError::ArityMismatch) }
        else { Ok(args.remove(0)) }
    } else if name.eq("snd!") {
        if args.len() < 2 { Err(EvalError::ArityMismatch) }
        else { Ok(args.remove(1)) }
    }
    else { Err(EvalError::UndefinedIdentifier(name)) }
}
