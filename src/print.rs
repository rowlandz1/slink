/* print.rs
 *
 * Includes functions for printing values to stdout
 */

use crate::ast::*;

impl ToString for SciVal {
    fn to_string(&self) -> String {
        match self {
            SciVal::Number(n) => n.to_string(),
            SciVal::Bool(b) => b.to_string(),
            SciVal::Matrix(_, c, v) => {
                let mut vs: Vec<String> = vec![];
                let mut elemlen = 1;
                for n in v {
                    let s = format!("{:.2}", n);
                    elemlen = elemlen.max(s.len());
                    vs.push(s);
                }
                let mut ret = String::new();
                for i in 0..v.len() {
                    if i % c == 0 { ret.push_str("{") }
                    ret = format!("{} {:>w$}", ret, vs[i], w=elemlen);
                    if i % c == c-1 { ret.push_str(" }\n") }
                }
                ret
            }
            SciVal::List(v) => {
                let vstrings: Vec<String> = v.iter().map(|x| x.to_string()).collect();
                format!("[ {} ]", vstrings.join(", "))
            }
            SciVal::Tuple(v) => {
                let vstrings: Vec<String> = v.iter().map(|x| x.to_string()).collect();
                format!("({})", vstrings.join(", "))
            }
            SciVal::Closure{params, ..} => {
                format!("lam {} -> *", params.join(" "))
            }
            SciVal::Macro(name, _) => {
                format!("<macro: {}>", name)
            }
        }
    }
}
