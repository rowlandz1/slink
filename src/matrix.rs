/* matrix.rs
 *
 * Definition of matrix operations
 */

use crate::number;
use crate::error::*;

pub fn matrix_det(d: usize, v: &Vec<number::Number>) -> number::Number {
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

pub fn matrix_inv(d: usize, v: &mut Vec<number::Number>) -> EvalResult<Vec<number::Number>> {
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
