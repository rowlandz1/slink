/* print.rs
 *
 * Includes functions for printing values to stdout.
 */

use crate::callable::Callable;
use crate::typechecker::Type;
use crate::value::SciVal;

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
            SciVal::Str(s) => {
                format!("'{}'", s.clone())
            }
            SciVal::Callable(f) => f.to_string(),
            SciVal::Any => String::from("_")
        }
    }
}

impl ToString for Callable {
    fn to_string(&self) -> String {
        match self {
            Callable::Closure{params, app, ..} => {
                let mut appstring = String::new();
                for (key, val) in app {
                    appstring = format!("{}, {}={}", appstring, key, val.to_string());
                }
                format!("lam ({}{}) => *", params.join(", "), appstring)
            }
            Callable::ListSlice(_, ..) => {
                format!("<slice>")
            }
            Callable::MatrixSlice(_, _, ..) => {
                format!("<matrix slice>")
            }
        }
    }
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Num => String::from("Num"),
            Type::Mat => String::from("Mat"),
            Type::Bool => String::from("Bool"),
            Type::Str => String::from("Str"),
            Type::List(a) => format!("List<{}>", a.to_string()),
            Type::TVar(s) => String::from(s),
            Type::Tuple(ts) => {
                format!("({})", ts.iter().map(Type::to_string).collect::<Vec<String>>().join(", "))
            }
            Type::Func(v, r) => {
                let params: Vec<String> = v.iter().map(Type::to_string).collect();
                format!("({}) -> {}", params.join(", "), r.to_string())
            }
            Type::Any => String::from("_"),
            Type::Unknown => String::from("Unknown"),
        }
    }
}