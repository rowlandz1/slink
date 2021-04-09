/* internals.rs
 *
 * Evaluates internal functions
 */

use crate::ast::SciVal;
use SciVal::*;


pub fn apply_to_internal(intfun: &String, mut args: Vec<SciVal>) -> Result<SciVal, &str> {
    if intfun.eq("index") {
        if args.len() < 3 { return Ok(Internal(intfun.clone(), args)); }
        if args.len() > 3 { return Err("Arity mismatch on function 'index'"); }

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
    } else if intfun.eq("det") {
        if args.len() < 1 { return Ok(Internal(intfun.clone(), args)); }
        if args.len() > 1 { return Err("Arity mismatch on function 'det'"); }

        if let Matrix(r, c, v) = args.pop().unwrap() {
            if r != c { return Err("Error, determinant of a non-square matrix is undefined."); }
            Ok(Number(matrix_det(r, &v)))
        } else { return Err("Error, determinant only defined for matrices.") }

    } else if intfun.eq("eye") {
        if args.len() < 1 { return Ok(Internal(intfun.clone(), args)); }
        if args.len() > 1 { return Err("Arity mismatch on function 'eye'"); }

        if let Number(n) = args.pop().unwrap() {
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
    } else if intfun.eq("op+") {
        if args.len() < 2 { return Ok(Internal(intfun.clone(), args)); }
        if args.len() > 2 { return Err("Arity mismatch on function 'op+'"); }

        let rhs = args.pop().unwrap();
        let lhs = args.pop().unwrap();
        Ok(lhs + rhs)
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
