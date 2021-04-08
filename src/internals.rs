use crate::ast::SciVal;
use SciVal::*;


pub fn apply_to_internal(intfun: &String, mut args: Vec<SciVal>) -> Result<SciVal, &str> {
    if intfun.eq("index") {
        if args.len() < 3 { return Ok(Internal(intfun.clone(), args)); }

        let mut mshape: (usize, usize) = (0,0);
        let mut index: (usize, usize) = (0,0);
        let mut vals: Vec<f64> = vec![];

        if let Number(n) = args.pop().unwrap() {
            index.1 = n.round() as usize;
        }
        if let Number(n) = args.pop().unwrap() {
            index.0 = n.round() as usize;
        }
        if let Matrix(r, c, v) = args.pop().unwrap() {
            mshape = (r, c);
            vals = v;
        } else { return Err("First arg must be matrix"); }

        if index.0 >= mshape.0 || index.1 >= mshape.1 {
            return Err("Index out of bounds");
        }
        Ok(Number(vals[index.0 * mshape.1 + index.1]))
    } else if intfun.eq("op+") {
        if args.len() < 2 { return Ok(Internal(intfun.clone(), args)); }
        if args.len() > 2 { return Err("Arity mismatch. op+ takes two arguments"); }

        let rhs = args.pop().unwrap();
        let lhs = args.pop().unwrap();
        Ok(lhs + rhs)
    }
    else { Err("Function not recognized") }
}
