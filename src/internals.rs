/* internals.rs
 *
 * Evaluates internal functions
 */

use std::collections::HashMap;
use crate::ast::SciVal;
use crate::ast::Arg;
use SciVal::*;

pub fn get_internal(name: String) -> SciVal {
    let env = HashMap::new();
    if name.eq("index") { Internal(env, vec![0, 1, 2], name) }
    else if name.eq("det")
         || name.eq("inv")
         || name.eq("transpose")
         || name.eq("eye")
         || name.eq("sqrt")
         || name.eq("len")
         { Internal(env, vec![0], name) }
    else if name.eq("op+")
         || name.eq("op-")
         || name.eq("op*")
         || name.eq("map")
         { Internal(env, vec![0, 1], name) }
    else { panic!("Error, unknown internal function"); }
}

pub fn apply_to_internal(intfun: &String, mut args: HashMap<usize, SciVal>) -> Result<SciVal, &str> {
    if intfun.eq("index") {
        if args.len() != 3 { return Err("Arity mismatch on function 'index'"); }

        let mut mshape: (usize, usize) = (0,0);
        let mut index: (usize, usize) = (0,0);
        let mut vals: Vec<f64> = vec![];

        if let Number(n) = args.remove(&2).unwrap() {
            index.1 = n.round() as usize;
        }
        if let Number(n) = args.remove(&1).unwrap() {
            index.0 = n.round() as usize;
        }
        if let Matrix(r, c, v) = args.remove(&0).unwrap() {
            mshape = (r, c);
            vals = v;
        } else { return Err("First arg must be matrix"); }

        if index.0 >= mshape.0 || index.1 >= mshape.1 {
            return Err("Index out of bounds");
        }
        Ok(Number(vals[index.0 * mshape.1 + index.1]))
    } else if intfun.eq("det") {
        if args.len() != 1 { return Err("Arity mismatch on function 'det'"); }

        if let Matrix(r, c, v) = args.remove(&0).unwrap() {
            if r != c { return Err("Error, determinant of a non-square matrix is undefined."); }
            Ok(Number(matrix_det(r, &v)))
        } else { return Err("Error, determinant only defined for matrices.") }
    } else if intfun.eq("inv") {
        if args.len() != 1 { return Err("Arity mismatch on function 'inv'"); }

        if let Matrix(r, c, mut v) = args.remove(&0).unwrap() {
            if r != c { return Err("Error, inverse of a non-square matrix is undefined."); }
            if matrix_det(r, &v) == 0f64 { return Err("Error, matrix is not invertible"); }
            let ret = matrix_inv(r, &mut v);
            Ok(Matrix(r, c, ret))
        } else { Err("Error, inverse only defined for matrices") }
    } else if intfun.eq("transpose") {
        if args.len() != 1 { return Err("Arity mismatch on function 'transpose'"); }

        if let Matrix(r, c, v) = args.remove(&0).unwrap() {
            let mut newv: Vec<f64> = vec![];
            for j in 0..c {
                for i in 0..r {
                    newv.push(v[i*c + j]);
                }
            }
            Ok(Matrix(c, r, newv))
        } else { return Err("Error, transpose only defined for matrices"); }
    } else if intfun.eq("eye") {
        if args.len() != 1 { return Err("Arity mismatch on function 'eye'"); }

        if let Number(n) = args.remove(&0).unwrap() {
            if n.round() < 1f64 { return Err("Error, argument to eye must be >= 1"); }
            let n = n.round() as usize;
            let mut v = vec![0f64; n*n];
            let mut i = 0;
            while i < v.len() {
                v[i] = 1f64;
                i += n + 1;
            }
            Ok(Matrix(n, n, v))
        } else { return Err("Error, eye accepts a number"); }
    } else if intfun.eq("sqrt") {
        if args.len() != 1 { return Err("Arity mismatch on function 'sqrt'"); }

        if let Number(n) = args.remove(&0).unwrap() {
            if n < 0f64 { return Err("Error, sqrt is undefined for negative numbers") }

            Ok(Number(n.sqrt()))
        } else { return Err("Error, sqrt accepts a number"); }
    } else if intfun.eq("len") {
        if args.len() != 1 { return Err("Arity mismatch on function 'len'"); }

        if let List(v) = args.remove(&0).unwrap() {
            Ok(Number(v.len() as f64))
        } else { return Err("Error, len is undefined for non-list values"); }
    } else if intfun.eq("map") {
        if args.len() != 2 { return Err("Arity mismatch on function 'map'"); }

        let arg1 = args.remove(&1).unwrap();
        let arg0 = args.remove(&0).unwrap();
        if let List(v) = arg0 {
            let mappedv = v.into_iter().map(|x| arg1.clone().fun_app(vec![Arg::Val(Box::new(x))])).collect();
            Ok(List(mappedv))
        } else { panic!("Error, first argument to map must be a list"); }
    } else if intfun.eq("op+") {
        if args.len() != 2 { return Err("Arity mismatch on function 'op+'"); }

        let rhs = args.remove(&1).unwrap();
        let lhs = args.remove(&0).unwrap();
        Ok(lhs + rhs)
    } else if intfun.eq("op-") {
        if args.len() != 2 { return Err("Arity mismatch on function 'op-'"); }

        let rhs = args.remove(&1).unwrap();
        let lhs = args.remove(&0).unwrap();
        Ok(lhs + (Number(-1f64) * rhs))
    } else if intfun.eq("op*") {
        if args.len() != 2 { return Err("Arity mismatch on function 'op*'"); }

        let rhs = args.remove(&1).unwrap();
        let lhs = args.remove(&0).unwrap();
        Ok(lhs * rhs)
    }
    else { Err("Function not recognized") }
}

fn matrix_det(d: usize, v: &Vec<f64>) -> f64 {
    if d == 1 { return v[0]; }
    if d == 2 { return v[0]*v[3] - v[2]*v[1]; }

    let mut submatrix: Vec<f64> = vec![0f64; (d-1)*(d-1)];
    let mut j = 0;
    for i in (d+1)..v.len() {
        if i % d == 0 { continue; }
        submatrix[j] = v[i];
        j += 1;
    }

    let mut det = v[0] * matrix_det(d-1, &submatrix);
    let mut sign = 1f64;
    for r in 1..d {
        sign = sign * -1f64;
        for c in 1..d {
            submatrix[(r-1)*(d-1) + c-1] = v[(r-1)*d + c];
        }
        det += sign * v[r*d] * matrix_det(d-1, &submatrix);
    }

    det
}

// TODO: fix
fn matrix_inv(d: usize, v: &mut Vec<f64>) -> Vec<f64> {
    if d == 1 { return vec![1f64 / v[0]]; }

    let mut ret = vec![0f64; d*d];
    let mut i = 0;
    while i < ret.len() {
        ret[i] = 1f64;
        i += d + 1;
    }
    for r in 0..d {
        let e = v[r*d + r];
        for c in 0..d {
            v[r*d + c] /= e;
            ret[r*d + c] = ret[r*d + c] / e;
        }
        for r2 in 0..d {
            if r2 != r {
                let e2 = v[r2*d + r];
                for c2 in 0..d {
                    v[r2*d + c2] -= e2 * v[r*d + c2];
                    ret[r2*d + c2] -= e2 * ret[r*d + c2];
                }
            }
        }
    }
    ret
}
