/* internals.rs
 *
 * Evaluates internal functions
 */

use std::collections::HashMap;
use crate::ast::SciVal;
use crate::ast::Arg;
use crate::error::EvalError;
use crate::error::EvalResult;
use crate::number;
use SciVal::*;

pub fn get_internal(name: String) -> EvalResult<SciVal> {
    let env = HashMap::new();
    let params: Vec<&str> = match name.as_str() {
        "det" |
        "inv" |
        "transpose" |
        "eye" |
        "sqrt" |
        "len" => vec!["0"],
        "op+" |
        "op-" |
        "op*" |
        "op/" |
        "map" |
        "range" |
        "push" |
        "zip" => vec!["0", "1"],
        "index" => vec!["0", "1", "2"],
        _ => { return Err(EvalError::UndefinedIdentifier(name)); }
    };
    let params: Vec<String> = params.iter().map(|x| x.to_string()).collect();

    Ok(Closure{
        env,
        name: None,
        params,
        expr: Err(name),
        next: None
    })
}

// Applies the arguments to the internal function.
// NOTE: Arity mismatches need to be checked BEFORE calling this function.
pub fn apply_to_internal(intfun: String, mut args: HashMap<String, SciVal>) -> EvalResult<SciVal> {
    if intfun.eq("index") {
        let index_c = match args.remove("2").unwrap() {
            Number(number::Number::Int(n)) => n as usize,
            _ => { return Err(EvalError::TypeMismatch); }
        };
        let index_r = match args.remove("1").unwrap() {
            Number(number::Number::Int(n)) => n as usize,
            _ => { return Err(EvalError::TypeMismatch); }
        };
        let (mrows, mcols, vals) = match args.remove("0").unwrap() {
            Matrix(r, c, v) => (r, c, v),
            _ => { return Err(EvalError::TypeMismatch); }
        };

        if index_r >= mrows || index_c >= mcols {
            return Err(EvalError::IndexOutOfBounds);
        }
        Ok(Number(vals[index_r * mcols + index_c]))
    } else if intfun.eq("det") {
        if let Matrix(r, c, v) = args.remove("0").unwrap() {
            if r != c { return Err(EvalError::InvalidMatrixShape); }
            Ok(Number(matrix_det(r, &v)))
        } else { return Err(EvalError::TypeMismatch) }
    } else if intfun.eq("inv") {
        if let Matrix(r, c, mut v) = args.remove("0").unwrap() {
            if r != c { return Err(EvalError::InvalidMatrixShape); }
            if matrix_det(r, &v) == number::Number::Float(0f64) { return Err(EvalError::NoninvertableMatrix); }
            let ret = matrix_inv(r, &mut v)?;
            Ok(Matrix(r, c, ret))
        } else { Err(EvalError::TypeMismatch) }
    } else if intfun.eq("transpose") {
        if let Matrix(r, c, v) = args.remove("0").unwrap() {
            let mut newv: Vec<number::Number> = vec![];
            for j in 0..c {
                for i in 0..r {
                    newv.push(v[i*c + j]);
                }
            }
            Ok(Matrix(c, r, newv))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("eye") {
        if let Number(number::Number::Int(n)) = args.remove("0").unwrap() {
            if n < 1 { return Err(EvalError::OutOfRange); }
            let n = n as usize;
            let mut v = vec![number::Number::Int(0); n*n];
            let mut i = 0;
            while i < v.len() {
                v[i] = number::Number::Int(1);
                i += n + 1;
            }
            Ok(Matrix(n, n, v))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("sqrt") {
        if let Number(n) = args.remove("0").unwrap() {
            Ok(Number(n.sqrt()?))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("len") {
        if let List(v) = args.remove("0").unwrap() {
            Ok(Number(number::Number::Int(v.len() as i32)))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("map") {
        let arg1 = args.remove("1").unwrap();
        let arg0 = args.remove("0").unwrap();
        if let List(v) = arg0 {
            let mut mappedv: Vec<SciVal> = Vec::new();
            for x in v {
                let x = arg1.clone().fun_app(vec![Arg::Val(Box::new(x))])?;
                mappedv.push(x);
            }
            Ok(List(mappedv))
        } else { panic!("Error, first argument to map must be a list"); }
    } else if intfun.eq("range") {
        let arg1 = args.remove("1").unwrap();
        let arg0 = args.remove("0").unwrap();
        if let (Number(number::Number::Int(arg0)), Number(number::Number::Int(arg1))) = (arg0, arg1) {
            let arg0 = arg0 as usize;
            let arg1 = arg1 as usize;
            if arg0 >= arg1 { return Ok(List(vec![])); }
            let mut retv: Vec<SciVal> = Vec::new();
            for n in arg0..arg1 {
                retv.push(Number(number::Number::Int(n as i32)));
            }
            return Ok(List(retv));
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("push") {
        let arg1 = args.remove("1").unwrap();
        let arg0 = args.remove("0").unwrap();
        if let List(mut v) = arg0 {
            v.push(arg1);
            Ok(List(v))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("zip") {
        let arg1 = match args.remove("1").unwrap() {
            List(v) => v,
            _ => { return Err(EvalError::TypeMismatch); }
        };
        let arg0 = match args.remove("0").unwrap() {
            List(v) => v,
            _ => { return Err(EvalError::TypeMismatch); }
        };
        let zipped = arg0.into_iter().zip(arg1.into_iter()).map(|(x, y)| {
            Tuple(vec![x, y])
        }).collect();
        Ok(List(zipped))
    } else if intfun.eq("op+") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs + rhs
    } else if intfun.eq("op-") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs + (Number(number::Number::Int(-1)) * rhs)?
    } else if intfun.eq("op*") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs * rhs
    } else if intfun.eq("op/") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs / rhs
    }
    else { Err(EvalError::UndefinedIdentifier(intfun)) }
}

fn matrix_det(d: usize, v: &Vec<number::Number>) -> number::Number {
    if d == 1 { return v[0]; }
    if d == 2 { return v[0]*v[3] - v[2]*v[1]; }

    let mut submatrix: Vec<number::Number> = vec![number::Number::Int(0); (d-1)*(d-1)];
    let mut j = 0;
    for i in (d+1)..v.len() {
        if i % d == 0 { continue; }
        submatrix[j] = v[i];
        j += 1;
    }

    let mut det = v[0] * matrix_det(d-1, &submatrix);
    let mut sign = number::Number::Int(1);
    for r in 1..d {
        sign = sign * number::Number::Int(-1);
        for c in 1..d {
            submatrix[(r-1)*(d-1) + c-1] = v[(r-1)*d + c];
        }
        det = det + sign * v[r*d] * matrix_det(d-1, &submatrix);
    }

    det
}

fn matrix_inv(d: usize, v: &mut Vec<number::Number>) -> EvalResult<Vec<number::Number>> {
    if d == 1 { return Ok(vec![v[0].recip()?]); }

    let mut ret = vec![number::Number::Int(0); d*d];
    let mut i = 0;
    while i < ret.len() {
        ret[i] = number::Number::Int(1);
        i += d + 1;
    }
    for r in 0..d {
        let e = v[r*d + r];
        for c in 0..d {
            v[r*d + c] = (v[r*d + c] / e)?;
            ret[r*d + c] = (ret[r*d + c] / e)?;
        }
        for r2 in 0..d {
            if r2 != r {
                let e2 = v[r2*d + r];
                for c2 in 0..d {
                    v[r2*d + c2] = v[r2*d + c2] - e2*v[r*d + c2];
                    ret[r2*d + c2] = ret[r2*d + c2] - e2*ret[r*d + c2];
                }
            }
        }
    }
    Ok(ret)
}
