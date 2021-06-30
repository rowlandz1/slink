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
use crate::value::{SciVal as V};
use Number::{Float, Int};

pub fn get_builtin_function(name: &String) -> Option<V> {
    let params: Vec<String> = match name.as_str() {
        "abs"       => vec!["x"],
        "argmax"    => vec!["l"],
        "argmin"    => vec!["l"],
        "conj"      => vec!["c"],
        "det"       => vec!["m"],
        "eye"       => vec!["n"],
        "filter"    => vec!["l", "p"],
        "inv"       => vec!["m"],
        "join"      => vec!["l", "s"],
        "len"       => vec!["l"],
        "map"       => vec!["l", "f"],
        "map2"      => vec!["l", "f"],
        "max"       => vec!["l"],
        "num"       => vec!["v"],
        "op+"       => vec!["x", "y"],
        "op-"       => vec!["x", "y"],
        "op*"       => vec!["x", "y"],
        "op/"       => vec!["x", "y"],
        "op%"       => vec!["x", "y"],
        "op=="      => vec!["x", "y"],
        "op!="      => vec!["x", "y"],
        "op>="      => vec!["x", "y"],
        "op<="      => vec!["x", "y"],
        "op>"       => vec!["x", "y"],
        "op<"       => vec!["x", "y"],
        "push"      => vec!["l", "v"],
        "range"     => vec!["n1", "n2"],
        "reduce"    => vec!["l", "v", "f"],
        "reverse"   => vec!["l"],
        "shape"     => vec!["m"],
        "split"     => vec!["s1", "s2"],
        "sqrt"      => vec!["x"],
        "transpose" => vec!["m"],
        "zip"       => vec!["l1", "l2"],
        _ => { return None; }
    }.into_iter().map(|x| x.to_string()).collect();

    Some(V::Callable(Callable::closure(HashMap::new(), None, params, HashMap::new(), Err(name.clone()))))
}

/// NOTE: Arity mismatches need to be checked BEFORE calling this function.
pub fn apply_to_internal(intfun: String, mut args: HashMap<String, V>) -> EvalResult<V> {
    match intfun.as_str() {
        "abs" => args.remove("x").unwrap().abs(),
        "argmax" => if let V::List(v) = args.remove("l").unwrap() {
            let start = match v.get(0) {
                Some(first) => first,
                None => return Err(EvalError::EmptyList),
            };
            let res = v.iter().try_fold((0, 0, start), |(i, im, max), elem| {
                if let V::Bool(true) = elem.gt(max)? { Ok((i+1, i, elem)) } else { Ok((i+1, im, max)) }
            })?;
            Ok(V::Number(Int(res.1 as i32)))
        } else { Err(EvalError::TypeMismatch) }
        "argmin" => if let V::List(v) = args.remove("l").unwrap() {
            let start = match v.get(0) {
                Some(first) => first,
                None => return Err(EvalError::EmptyList),
            };
            let res = v.iter().try_fold((0, 0, start), |(i, im, min), elem| {
                if let V::Bool(true) = elem.lt(min)? { Ok((i+1, i, elem)) } else { Ok((i+1, im, min)) }
            })?;
            Ok(V::Number(Int(res.1 as i32)))
        } else { Err(EvalError::TypeMismatch) }
        "conj" => args.remove("c").unwrap().conjugate(),
        "det" => if let V::Matrix(r, c, v) = args.remove("m").unwrap() {
            if r != c { return Err(EvalError::InvalidMatrixShape); }
            Ok(V::Number(matrix_det(r, &v)))
        } else { return Err(EvalError::TypeMismatch) }
        "eye" => if let V::Number(Int(n)) = args.remove("n").unwrap() {
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
        "filter" => if let V::List(v) = args.remove("l").unwrap() {
            let p = args.remove("p").unwrap();
            let mut filteredv: Vec<V> = Vec::new();
            for x in v {
                let sat = p.clone().fun_app(vec![x.clone()])?;
                if let V::Bool(b) = sat {
                    if b { filteredv.push(x); }
                } else { return Err(EvalError::TypeMismatch); }
            }
            Ok(V::List(filteredv))
        } else { Err(EvalError::TypeMismatch) }
        "inv" => args.remove("m").unwrap().inv(),
        "join" => {
            let arg1 = if let V::Str(s) = args.remove("s").unwrap() { s }
                    else { return Err(EvalError::TypeMismatch); };
            let arg0 = if let V::List(v) = args.remove("l").unwrap() { v }
                    else { return Err(EvalError::TypeMismatch); };
            let mut strings: Vec<String> = Vec::new();
            for val in arg0 {
                if let V::Str(s) = val { strings.push(s); } else { return Err(EvalError::TypeMismatch); }
            }
            Ok(V::Str(strings.join(arg1.as_str())))
        }
        "len" => match args.remove("l").unwrap() {
            V::List(v) => Ok(V::Number(Int(v.len() as i32))),
            V::Str(s) => Ok(V::Number(Int(s.len() as i32))),
            _ => Err(EvalError::TypeMismatch)
        }
        "map" => if let V::List(v) = args.remove("l").unwrap() {
            let f = args.remove("f").unwrap();
            let mut mappedv: Vec<V> = Vec::new();
            for x in v {
                let x = f.clone().fun_app(vec![x])?;
                mappedv.push(x);
            }
            Ok(V::List(mappedv))
        } else { Err(EvalError::TypeMismatch) }
        "map2" => if let V::List(v) = args.remove("l").unwrap() {
            let f = args.remove("f").unwrap();
            let mut left: Vec<V> = Vec::new();
            let mut right: Vec<V> = Vec::new();
            for x in v {
                if let V::Tuple(mut inner) = x {
                    if inner.len() == 2 {
                        right.push(inner.pop().unwrap());
                        left.push(inner.pop().unwrap());
                    } else { return Err(EvalError::TypeMismatch); }
                } else { return Err(EvalError::TypeMismatch); }
            }
            let mut res: Vec<V> = Vec::new();
            for (l, r) in left.into_iter().zip(right) {
                res.push(f.clone().fun_app(vec![l, r])?);
            }
            Ok(V::List(res))
        } else { Err(EvalError::TypeMismatch) }
        "max" => match args.remove("l").unwrap() {
            V::List(v) => {
                let mut m: V = v[0].clone();
                for elem in v {
                    if let V::Bool(true) = elem.gt(&m)? { m = elem; }
                }
                Ok(m)
            }
            V::Matrix(_, _, v) => {
                let mut m = v[0];
                for elem in v {
                    if let cmp::Ordering::Greater = elem.compare(&m)? { m = elem; }
                }
                Ok(V::Number(m))
            }
            _ => Err(EvalError::TypeMismatch)
        }
        "num" => match args.remove("v").unwrap() {
            V::Str(s) => {
                if let Ok(n) = s.parse::<i32>() {
                    Ok(V::Number(Int(n)))
                } else if let Ok(n) = s.parse::<f64>() {
                    Ok(V::Number(Float(n)))
                } else { Err(EvalError::TypeConversionError) }
            }
            _ => Err(EvalError::TypeMismatch)
        }
        "op+" => {
            let lhs = args.remove("x").unwrap();
            lhs + args.remove("y").unwrap()
        }
        "op-" => {
            let lhs = args.remove("x").unwrap();
            lhs - args.remove("y").unwrap()
        }
        "op*" => {
            let lhs = args.remove("x").unwrap();
            lhs * args.remove("y").unwrap()
        }
        "op/" => {
            let lhs = args.remove("x").unwrap();
            lhs / args.remove("y").unwrap()
        }
        "op%" => {
            let lhs = args.remove("x").unwrap();
            lhs % args.remove("y").unwrap()
        }
        "op==" => args.get("x").unwrap().equals(args.get("y").unwrap()),
        "op!=" => args.get("x").unwrap().not_equals(args.get("y").unwrap()),
        "op>=" => args.get("x").unwrap().ge(args.get("y").unwrap()),
        "op<=" => args.get("x").unwrap().le(args.get("y").unwrap()),
        "op>" => args.get("x").unwrap().gt(args.get("y").unwrap()),
        "op<" => args.get("x").unwrap().lt(args.get("y").unwrap()),
        "push" => if let V::List(mut l) = args.remove("l").unwrap() {
            l.push(args.remove("v").unwrap());
            Ok(V::List(l))
        } else { Err(EvalError::TypeMismatch) }
        "range" => if let (V::Number(Int(n1)), V::Number(Int(n2))) = (args.get("n1").unwrap(), args.get("n2").unwrap()) {
            let (n1, n2) = (*n1 as usize, *n2 as usize);
            if n1 >= n2 { return Ok(V::List(vec![])); }
            let mut retv: Vec<V> = Vec::new();
            for n in n1..n2 {
                retv.push(V::Number(Int(n as i32)));
            }
            Ok(V::List(retv))
        } else { Err(EvalError::TypeMismatch) }
        "reduce" => if let V::List(l) = args.remove("l").unwrap() {
            let mut v = args.remove("v").unwrap();
            let f = args.remove("f").unwrap();
            for item in l {
                let arglist = vec![v, item];
                v = f.clone().fun_app(arglist)?;
            }
            Ok(v)
        } else { Err(EvalError::TypeMismatch) }
        "reverse" => match args.remove("l").unwrap() {
            V::List(mut v) => { v.reverse(); Ok(V::List(v)) }
            V::Str(s) => Ok(V::Str(s.chars().rev().collect())),
            _ => Err(EvalError::TypeMismatch)
        }
        "shape" => if let V::Matrix(r, c, _) = args.remove("m").unwrap() {
            Ok(V::Tuple(vec![V::Number(Int(r as i32)), V::Number(Int(c as i32))]))
        } else { Err(EvalError::TypeMismatch) }
        "split" => {
            let s1 = if let V::Str(v) = args.remove("s1").unwrap() { v }
                     else { return Err(EvalError::TypeMismatch) };
            let s2 = if let V::Str(v) = args.remove("s2").unwrap() { v }
                     else { return Err(EvalError::TypeMismatch) };
            let mut ret: Vec<V> = Vec::new();
            for part in s1.split(s2.as_str()) {
                ret.push(V::Str(part.to_string()));
            }
            Ok(V::List(ret))
        }
        "sqrt" => if let V::Number(x) = args.remove("x").unwrap() {
            Ok(V::Number(x.sqrt()?))
        } else { Err(EvalError::TypeMismatch) }
        "transpose" => if let V::Matrix(r, c, v) = args.remove("m").unwrap() {
            let mut newv: Vec<Number> = vec![];
            for j in 0..c {
                for i in 0..r {
                    newv.push(v[i*c + j]);
                }
            }
            Ok(V::Matrix(c, r, newv))
        } else { Err(EvalError::TypeMismatch) }
        "zip" => {
            let l1 = if let V::List(v) = args.remove("l1").unwrap() { v }
                     else { return Err(EvalError::TypeMismatch); };
            let l2 = if let V::List(v) = args.remove("l2").unwrap() { v }
                     else { return Err(EvalError::TypeMismatch); };
            let zipped = l1.into_iter().zip(l2.into_iter()).map(|(x, y)| {
                V::Tuple(vec![x, y])
            }).collect();
            Ok(V::List(zipped))
        }
        _ => Err(EvalError::UndefinedIdentifier(intfun))
    }
}
