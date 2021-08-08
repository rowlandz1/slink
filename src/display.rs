/* display.rs
 *
 * Functions for displaying values and types to stdout.
 */

use std::fmt;
use crate::callable::Callable;
use crate::types::Type;
use crate::value::SciVal;

impl fmt::Display for SciVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SciVal::Number(n) => write!(f, "{}", n),
            SciVal::Bool(b) => write!(f, "{}", b),
            SciVal::Matrix(r, c, nums) => {
                let entries = nums.iter().map(|n| format!("{:.2}", n));
                let elemlen = entries.clone().map(|s| s.len()).max().unwrap();
                let mut entries = entries.map(|e| format!("{:>w$}", e, w=elemlen));

                for _ in 0..(*r-1) {
                    writeln!(f, "{{{}}}", entries.by_ref().take(*c).collect::<Vec<String>>().join(" "))?;
                }
                write!(f, "{{{}}}", entries.collect::<Vec<String>>().join(" "))
            }
            SciVal::List(v) =>
                write!(f, "[{}]", v.iter().map(SciVal::to_string).collect::<Vec<String>>().join(", ")),
            SciVal::Tuple(v) => 
                write!(f, "({})", v.iter().map(SciVal::to_string).collect::<Vec<String>>().join(", ")),
            SciVal::Str(s) => write!(f, "\"{}\"", s),
            SciVal::Callable(func) => write!(f, "{}", func),
            SciVal::Any => write!(f, "_")
        }
    }
}

impl fmt::Display for Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Callable::Closure{params, app, ..} => {
                write!(f, "lam ({}", params.join(", "))?;
                for (key, val) in app { write!(f, ", {}={}", key, val)?; }
                write!(f, ") => *")
            }
            Callable::ListSlice(..) => write!(f, "<slice>"),
            Callable::MatrixSlice(..) => write!(f, "<matrix slice>")
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Num => write!(f, "Num"),
            Type::Mat => write!(f, "Mat"),
            Type::Bool => write!(f, "Bool"),
            Type::Str => write!(f, "Str"),
            Type::List(a) => write!(f, "[{}]", a),
            Type::TVar(s) => write!(f, "{}", s),
            Type::Tuple(ts) => write!(f, "({})", ts.iter().map(Type::to_string).collect::<Vec<String>>().join(", ")),
            Type::Func(v, r) => {
                let params: Vec<String> = v.iter().map(Type::to_string).collect();
                write!(f, "({}) -> {}", params.join(", "), *r)
            }
            Type::Any => write!(f, "_")
        }
    }
}