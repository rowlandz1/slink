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
use crate::number;
use crate::matrix::*;
use SciVal::*;

pub fn get_internal(name: String) -> EvalResult<SciVal> {
    match name.as_str() {
        "pi" => { return Ok(Number(number::Number::Float(std::f64::consts::PI))); }
        "e" => { return Ok(Number(number::Number::Float(std::f64::consts::E))); }
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
        "len" => vec!["0"],
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
        "zip" => vec!["0", "1"],
        "reduce" => vec!["0", "1", "2"],
        _ => { return Err(EvalError::UndefinedIdentifier(name)); }
    };
    let params: Vec<String> = params.into_iter().map(|x| x.to_string()).collect();

    Ok(Closure{
        env,
        name: None,
        params,
        app: HashMap::new(),
        expr: Err(name),
        next: None
    })
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
            Ok(Number(matrix_det(r, &v)))
        } else { return Err(EvalError::TypeMismatch) }
    } else if intfun.eq("inv") {
        args.remove("0").unwrap().inv()
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
    } else if intfun.eq("abs") {
        args.remove("0").unwrap().abs()
    } else if intfun.eq("sqrt") {
        if let Number(n) = args.remove("0").unwrap() {
            Ok(Number(n.sqrt()?))
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
            let mut m: number::Number = v[0];
            for elem in v {
                if let cmp::Ordering::Greater = elem.compare(&m)? { m = elem; }
            }
            Ok(Number(m))
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
            Ok(Number(number::Number::Int(i as i32)))
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
