/* internals.rs
 *
 * Evaluates internal functions
 */

use std::collections::HashMap;
use core::cmp;
use crate::ast::SciVal;
use crate::ast::Arg;
use crate::error::EvalError;
use crate::error::EvalResult;
use crate::number::Number;
use Number::*;
use crate::matrix::*;
use crate::callable::Callable;
use SciVal::*;
use Callable::*;

pub fn get_internal(name: String) -> EvalResult<SciVal> {
    match name.as_str() {
        "pi" => { return Ok(VNumber(Float(std::f64::consts::PI))); }
        "e" => { return Ok(VNumber(Float(std::f64::consts::E))); }
        _ => {}
    }

    let env = HashMap::new();
    let params: Vec<&str> = match name.as_str() {
        "det" |
        "inv" |
        "transpose" |
        "eye" |
        "abs" |
        "sqrt" |
        "conj" |
        "max" |
        "argmax" |
        "argmin" |
        "len" |
        "reverse" |
        "num" => vec!["0"],
        "op+" |
        "op-" |
        "op*" |
        "op/" |
        "op%" |
        "op==" |
        "op!=" |
        "op>=" |
        "op<=" |
        "op>" |
        "op<" |
        "map" |
        "filter" |
        "range" |
        "push" |
        "zip" |
        "split" |
        "join" => vec!["0", "1"],
        "reduce" => vec!["0", "1", "2"],
        _ => { return Err(EvalError::UndefinedIdentifier(name)); }
    };
    let params: Vec<String> = params.into_iter().map(|x| x.to_string()).collect();

    Ok(VCallable(Closure{
        env,
        name: None,
        params,
        app: HashMap::new(),
        expr: Err(name),
        next: None
    }))
}

// Applies the arguments to the internal function.
// NOTE: Arity mismatches need to be checked BEFORE calling this function.
pub fn apply_to_internal(intfun: String, mut args: HashMap<String, SciVal>) -> EvalResult<SciVal> {
    if intfun.eq("reduce") {
        let arg2 = args.remove("2").unwrap();
        let arg1 = args.remove("1").unwrap();
        let arg0 = if let List(v) = args.remove("0").unwrap() {v}
                   else { return Err(EvalError::TypeMismatch); };
        let mut ret: SciVal = arg1;
        for item in arg0 {
            let arglist = vec![Arg::Val(Box::new(ret)), Arg::Val(Box::new(item))];
            ret = arg2.clone().fun_app(arglist)?;
        }
        Ok(ret)
    } else if intfun.eq("det") {
        if let Matrix(r, c, v) = args.remove("0").unwrap() {
            if r != c { return Err(EvalError::InvalidMatrixShape); }
            Ok(VNumber(matrix_det(r, &v)))
        } else { return Err(EvalError::TypeMismatch) }
    } else if intfun.eq("inv") {
        args.remove("0").unwrap().inv()
    } else if intfun.eq("transpose") {
        if let Matrix(r, c, v) = args.remove("0").unwrap() {
            let mut newv: Vec<Number> = vec![];
            for j in 0..c {
                for i in 0..r {
                    newv.push(v[i*c + j]);
                }
            }
            Ok(Matrix(c, r, newv))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("eye") {
        if let VNumber(Int(n)) = args.remove("0").unwrap() {
            if n < 1 { return Err(EvalError::OutOfRange); }
            let n = n as usize;
            let mut v = vec![Int(0); n*n];
            let mut i = 0;
            while i < v.len() {
                v[i] = Int(1);
                i += n + 1;
            }
            Ok(Matrix(n, n, v))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("abs") {
        args.remove("0").unwrap().abs()
    } else if intfun.eq("sqrt") {
        if let VNumber(n) = args.remove("0").unwrap() {
            Ok(VNumber(n.sqrt()?))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("conj") {
        args.remove("0").unwrap().conjugate()
    } else if intfun.eq("max") {
        let arg = args.remove("0").unwrap();
        if let List(v) = arg {
            let mut m: SciVal = v[0].clone();
            for elem in v {
                if let Bool(true) = elem.gt(&m)? { m = elem; }
            }
            Ok(m)
        } else if let Matrix(_, _, v) = arg {
            let mut m = v[0];
            for elem in v {
                if let cmp::Ordering::Greater = elem.compare(&m)? { m = elem; }
            }
            Ok(VNumber(m))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("argmax") {
        let arg = args.remove("0").unwrap();
        if let List(v) = arg {
            let mut m: SciVal = v[0].clone();
            let mut i: usize = 0;
            let mut j: usize = 0;
            for elem in v {
                if let Bool(true) = elem.gt(&m)? { m = elem; i = j; }
                j += 1;
            }
            Ok(VNumber(Int(i as i32)))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("argmin") {
        let arg = args.remove("0").unwrap();
        if let List(v) = arg {
            let mut m: SciVal = v[0].clone();
            let mut i: usize = 0;
            let mut j: usize = 0;
            for elem in v {
                if let Bool(true) = elem.lt(&m)? { m = elem; i = j; }
                j += 1;
            }
            Ok(VNumber(Int(i as i32)))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("len") {
        match args.remove("0").unwrap() {
            List(v) => Ok(VNumber(Int(v.len() as i32))),
            Str(s) => Ok(VNumber(Int(s.len() as i32))),
            _ => Err(EvalError::TypeMismatch)
        }
    } else if intfun.eq("reverse") {
        match args.remove("0").unwrap() {
            List(mut v) => { v.reverse(); Ok(List(v)) }
            Str(s) => Ok(Str(s.chars().rev().collect())),
            _ => Err(EvalError::TypeMismatch)
        }
    } else if intfun.eq("num") {
        match args.remove("0").unwrap() {
            Str(s) => {
                if let Ok(n) = s.parse::<i32>() {
                    Ok(VNumber(Int(n)))
                } else if let Ok(n) = s.parse::<f64>() {
                    Ok(VNumber(Float(n)))
                } else { Err(EvalError::TypeConversionError) }
            }
            _ => Err(EvalError::TypeMismatch)
        }
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
        } else { Err(EvalError::TypeMismatch) }
    } else if intfun.eq("filter") {
        let arg1 = args.remove("1").unwrap();
        let arg0 = args.remove("0").unwrap();
        if let List(v) = arg0 {
            let mut filteredv: Vec<SciVal> = Vec::new();
            for x in v {
                let sat = arg1.clone().fun_app(vec![Arg::Val(Box::new(x.clone()))])?;
                if let Bool(b) = sat {
                    if b { filteredv.push(x); }
                } else { return Err(EvalError::TypeMismatch); }
            }
            Ok(List(filteredv))
        } else { Err(EvalError::TypeMismatch) }
    } else if intfun.eq("range") {
        let arg1 = args.remove("1").unwrap();
        let arg0 = args.remove("0").unwrap();
        if let (VNumber(Int(arg0)), VNumber(Int(arg1))) = (arg0, arg1) {
            let arg0 = arg0 as usize;
            let arg1 = arg1 as usize;
            if arg0 >= arg1 { return Ok(List(vec![])); }
            let mut retv: Vec<SciVal> = Vec::new();
            for n in arg0..arg1 {
                retv.push(VNumber(Int(n as i32)));
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
    } else if intfun.eq("split") {
        let arg1 = match args.remove("1").unwrap() {
            Str(v) => v,
            _ => { return Err(EvalError::TypeMismatch); }
        };
        let arg0 = match args.remove("0").unwrap() {
            Str(v) => v,
            _ => { return Err(EvalError::TypeMismatch); }
        };
        let mut ret: Vec<SciVal> = Vec::new();
        for part in arg0.split(arg1.as_str()) {
            ret.push(Str(part.to_string()));
        }
        Ok(List(ret))
    } else if intfun.eq("join") {
        let arg1 = if let Str(s) = args.remove("1").unwrap() { s }
                   else { return Err(EvalError::TypeMismatch); };
        let arg0 = if let List(v) = args.remove("0").unwrap() { v }
                   else { return Err(EvalError::TypeMismatch); };
        let mut strings: Vec<String> = Vec::new();
        for val in arg0 {
            if let Str(s) = val { strings.push(s); } else { return Err(EvalError::TypeMismatch); }
        }
        Ok(Str(strings.join(arg1.as_str())))
    } else if intfun.eq("op+") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs + rhs
    } else if intfun.eq("op-") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs + (VNumber(Int(-1)) * rhs)?
    } else if intfun.eq("op*") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs * rhs
    } else if intfun.eq("op/") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs / rhs
    } else if intfun.eq("op%") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs % rhs
    } else if intfun.eq("op==") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs.equals(&rhs)
    } else if intfun.eq("op!=") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs.not_equals(&rhs)
    } else if intfun.eq("op<=") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs.le(&rhs)
    } else if intfun.eq("op>=") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs.ge(&rhs)
    } else if intfun.eq("op<") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs.lt(&rhs)
    } else if intfun.eq("op>") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs.gt(&rhs)
    }
    else { Err(EvalError::UndefinedIdentifier(intfun)) }
}
