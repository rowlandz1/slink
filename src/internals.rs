/* internals.rs
 *
 * Evaluates internal functions.
 * get_internal converts a string into the corresponding built-in value
 * apply_to_internal performs evaluation of built-in functions
 */

use std::collections::HashMap;
use core::cmp;
use crate::callable::Callable;
use crate::error::{EvalError, EvalResult};
use crate::matrix::matrix_det;
use crate::number::Number;
use crate::value::{Arg, SciVal as V};
use Callable::Closure;
use Number::{Float, Int};

pub fn get_internal(name: String) -> EvalResult<V> {
    match name.as_str() {
        "pi" => { return Ok(V::Number(Float(std::f64::consts::PI))); }
        "e" => { return Ok(V::Number(Float(std::f64::consts::E))); }
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

    Ok(V::Callable(Closure{
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
pub fn apply_to_internal(intfun: String, mut args: HashMap<String, V>) -> EvalResult<V> {
    if intfun.eq("reduce") {
        let arg2 = args.remove("2").unwrap();
        let arg1 = args.remove("1").unwrap();
        let arg0 = if let V::List(v) = args.remove("0").unwrap() {v}
                   else { return Err(EvalError::TypeMismatch); };
        let mut ret: V = arg1;
        for item in arg0 {
            let arglist = vec![Arg::Val(Box::new(ret)), Arg::Val(Box::new(item))];
            ret = arg2.clone().fun_app(arglist)?;
        }
        Ok(ret)
    } else if intfun.eq("det") {
        if let V::Matrix(r, c, v) = args.remove("0").unwrap() {
            if r != c { return Err(EvalError::InvalidMatrixShape); }
            Ok(V::Number(matrix_det(r, &v)))
        } else { return Err(EvalError::TypeMismatch) }
    } else if intfun.eq("inv") {
        args.remove("0").unwrap().inv()
    } else if intfun.eq("transpose") {
        if let V::Matrix(r, c, v) = args.remove("0").unwrap() {
            let mut newv: Vec<Number> = vec![];
            for j in 0..c {
                for i in 0..r {
                    newv.push(v[i*c + j]);
                }
            }
            Ok(V::Matrix(c, r, newv))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("eye") {
        if let V::Number(Int(n)) = args.remove("0").unwrap() {
            if n < 1 { return Err(EvalError::OutOfRange); }
            let n = n as usize;
            let mut v = vec![Int(0); n*n];
            let mut i = 0;
            while i < v.len() {
                v[i] = Int(1);
                i += n + 1;
            }
            Ok(V::Matrix(n, n, v))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("abs") {
        args.remove("0").unwrap().abs()
    } else if intfun.eq("sqrt") {
        if let V::Number(n) = args.remove("0").unwrap() {
            Ok(V::Number(n.sqrt()?))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("conj") {
        args.remove("0").unwrap().conjugate()
    } else if intfun.eq("max") {
        let arg = args.remove("0").unwrap();
        if let V::List(v) = arg {
            let mut m: V = v[0].clone();
            for elem in v {
                if let V::Bool(true) = elem.gt(&m)? { m = elem; }
            }
            Ok(m)
        } else if let V::Matrix(_, _, v) = arg {
            let mut m = v[0];
            for elem in v {
                if let cmp::Ordering::Greater = elem.compare(&m)? { m = elem; }
            }
            Ok(V::Number(m))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("argmax") {
        let arg = args.remove("0").unwrap();
        if let V::List(v) = arg {
            let mut m: V = v[0].clone();
            let mut i: usize = 0;
            let mut j: usize = 0;
            for elem in v {
                if let V::Bool(true) = elem.gt(&m)? { m = elem; i = j; }
                j += 1;
            }
            Ok(V::Number(Int(i as i32)))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("argmin") {
        let arg = args.remove("0").unwrap();
        if let V::List(v) = arg {
            let mut m: V = v[0].clone();
            let mut i: usize = 0;
            let mut j: usize = 0;
            for elem in v {
                if let V::Bool(true) = elem.lt(&m)? { m = elem; i = j; }
                j += 1;
            }
            Ok(V::Number(Int(i as i32)))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("len") {
        match args.remove("0").unwrap() {
            V::List(v) => Ok(V::Number(Int(v.len() as i32))),
            V::Str(s) => Ok(V::Number(Int(s.len() as i32))),
            _ => Err(EvalError::TypeMismatch)
        }
    } else if intfun.eq("reverse") {
        match args.remove("0").unwrap() {
            V::List(mut v) => { v.reverse(); Ok(V::List(v)) }
            V::Str(s) => Ok(V::Str(s.chars().rev().collect())),
            _ => Err(EvalError::TypeMismatch)
        }
    } else if intfun.eq("num") {
        match args.remove("0").unwrap() {
            V::Str(s) => {
                if let Ok(n) = s.parse::<i32>() {
                    Ok(V::Number(Int(n)))
                } else if let Ok(n) = s.parse::<f64>() {
                    Ok(V::Number(Float(n)))
                } else { Err(EvalError::TypeConversionError) }
            }
            _ => Err(EvalError::TypeMismatch)
        }
    } else if intfun.eq("map") {
        let arg1 = args.remove("1").unwrap();
        let arg0 = args.remove("0").unwrap();
        if let V::List(v) = arg0 {
            let mut mappedv: Vec<V> = Vec::new();
            for x in v {
                let x = arg1.clone().fun_app(vec![Arg::Val(Box::new(x))])?;
                mappedv.push(x);
            }
            Ok(V::List(mappedv))
        } else { Err(EvalError::TypeMismatch) }
    } else if intfun.eq("filter") {
        let arg1 = args.remove("1").unwrap();
        let arg0 = args.remove("0").unwrap();
        if let V::List(v) = arg0 {
            let mut filteredv: Vec<V> = Vec::new();
            for x in v {
                let sat = arg1.clone().fun_app(vec![Arg::Val(Box::new(x.clone()))])?;
                if let V::Bool(b) = sat {
                    if b { filteredv.push(x); }
                } else { return Err(EvalError::TypeMismatch); }
            }
            Ok(V::List(filteredv))
        } else { Err(EvalError::TypeMismatch) }
    } else if intfun.eq("range") {
        let arg1 = args.remove("1").unwrap();
        let arg0 = args.remove("0").unwrap();
        if let (V::Number(Int(arg0)), V::Number(Int(arg1))) = (arg0, arg1) {
            let arg0 = arg0 as usize;
            let arg1 = arg1 as usize;
            if arg0 >= arg1 { return Ok(V::List(vec![])); }
            let mut retv: Vec<V> = Vec::new();
            for n in arg0..arg1 {
                retv.push(V::Number(Int(n as i32)));
            }
            return Ok(V::List(retv));
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("push") {
        let arg1 = args.remove("1").unwrap();
        let arg0 = args.remove("0").unwrap();
        if let V::List(mut v) = arg0 {
            v.push(arg1);
            Ok(V::List(v))
        } else { return Err(EvalError::TypeMismatch); }
    } else if intfun.eq("zip") {
        let arg1 = match args.remove("1").unwrap() {
            V::List(v) => v,
            _ => { return Err(EvalError::TypeMismatch); }
        };
        let arg0 = match args.remove("0").unwrap() {
            V::List(v) => v,
            _ => { return Err(EvalError::TypeMismatch); }
        };
        let zipped = arg0.into_iter().zip(arg1.into_iter()).map(|(x, y)| {
            V::Tuple(vec![x, y])
        }).collect();
        Ok(V::List(zipped))
    } else if intfun.eq("split") {
        let arg1 = match args.remove("1").unwrap() {
            V::Str(v) => v,
            _ => { return Err(EvalError::TypeMismatch); }
        };
        let arg0 = match args.remove("0").unwrap() {
            V::Str(v) => v,
            _ => { return Err(EvalError::TypeMismatch); }
        };
        let mut ret: Vec<V> = Vec::new();
        for part in arg0.split(arg1.as_str()) {
            ret.push(V::Str(part.to_string()));
        }
        Ok(V::List(ret))
    } else if intfun.eq("join") {
        let arg1 = if let V::Str(s) = args.remove("1").unwrap() { s }
                   else { return Err(EvalError::TypeMismatch); };
        let arg0 = if let V::List(v) = args.remove("0").unwrap() { v }
                   else { return Err(EvalError::TypeMismatch); };
        let mut strings: Vec<String> = Vec::new();
        for val in arg0 {
            if let V::Str(s) = val { strings.push(s); } else { return Err(EvalError::TypeMismatch); }
        }
        Ok(V::Str(strings.join(arg1.as_str())))
    } else if intfun.eq("op+") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs + rhs
    } else if intfun.eq("op-") {
        let rhs = args.remove("1").unwrap();
        let lhs = args.remove("0").unwrap();
        lhs + (V::Number(Int(-1)) * rhs)?
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
