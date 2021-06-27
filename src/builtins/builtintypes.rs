/* builtintypes.rs
 *
 * Function for getting the type of builtin functions.
 */

use crate::typechecker::Type as T;

pub fn get_builtin_type(name: &str) -> Option<T> {
    Some(match name {
        "abs"       => T::func1(T::Num, T::Num),
        "argmax"    => T::func1(T::list(T::Num), T::Num),
        "argmin"    => T::func1(T::list(T::Num), T::Num),
        "conj"      => T::func1(T::Num, T::Num),
        "det"       => T::func1(T::Mat, T::Num),
        "e"         => T::Num,
        "eye"       => T::func1(T::Num, T::Mat),
        "filter"    => T::func2(T::list(T::var("A")), T::func1(T::var("A"), T::Bool), T::list(T::var("A"))),
        "inv"       => T::func1(T::Mat, T::Mat),
        "join"      => T::func2(T::list(T::Str), T::Str, T::Str),
        "len"       => T::func1(T::list(T::var("A")), T::Num),
        "map"       => T::func2(T::list(T::var("A")), T::func1(T::var("A"), T::var("B")), T::list(T::var("B"))),
        "map2"      => T::func2(T::list(T::tup2(T::var("A"), T::var("B"))), T::func2(T::var("A"), T::var("B"), T::var("C")), T::list(T::var("C"))),
        "max"       => T::func1(T::list(T::Num), T::Num),
        "num"       => T::func1(T::Str, T::Num),
        "op+"       => T::func2(T::Num, T::Num, T::Num),
        "op-"       => T::func2(T::Num, T::Num, T::Num),
        "op*"       => T::func2(T::Num, T::Num, T::Num),
        "op/"       => T::func2(T::Num, T::Num, T::Num),
        "op%"       => T::func2(T::Num, T::Num, T::Num),
        "op=="      => T::func2(T::Num, T::Num, T::Bool),
        "op!="      => T::func2(T::Num, T::Num, T::Bool),
        "op>="      => T::func2(T::Num, T::Num, T::Bool),
        "op<="      => T::func2(T::Num, T::Num, T::Bool),
        "op>"       => T::func2(T::Num, T::Num, T::Bool),
        "op<"       => T::func2(T::Num, T::Num, T::Bool),
        "pi"        => T::Num,
        "push"      => T::func2(T::list(T::var("A")), T::var("A"), T::list(T::var("A"))),
        "range"     => T::func2(T::Num, T::Num, T::list(T::Num)),
        "reduce"    => T::func3(T::list(T::var("A")), T::var("B"), T::func2(T::var("B"), T::var("A"), T::var("B")), T::var("B")),
        "reverse"   => T::func1(T::list(T::var("A")), T::list(T::var("A"))),
        "split"     => T::func2(T::Str, T::Str, T::list(T::Str)),
        "sqrt"      => T::func1(T::Num, T::Num),
        "transpose" => T::func1(T::Mat, T::Mat),
        "zip"       => T::func2(T::list(T::var("A")), T::list(T::var("B")), T::list(T::tup2(T::var("A"), T::var("B")))),
        _ => { return None; }
    })
}

pub fn _get_builtin_type_mult(name: &str) -> Vec<T> {
    match name {
        "abs"       => vec![T::func1(T::Num, T::Num)],
        "argmax"    => vec![T::func1(T::list(T::Num), T::Num)],
        "argmin"    => vec![T::func1(T::list(T::Num), T::Num)],
        "conj"      => vec![T::func1(T::Num, T::Num)],
        "det"       => vec![T::func1(T::Mat, T::Num)],
        "e"         => vec![T::Num],
        "eye"       => vec![T::func1(T::Num, T::Mat)],
        "filter"    => vec![T::func2(T::list(T::var("A")), T::func1(T::var("A"), T::Bool), T::list(T::var("A")))],
        "inv"       => vec![T::func1(T::Mat, T::Mat)],
        "join"      => vec![T::func2(T::list(T::Str), T::Str, T::Str)],
        "len"       => vec![T::func1(T::list(T::var("A")), T::Num),
                            T::func1(T::Str, T::Num)],
        "map"       => vec![T::func2(T::list(T::var("A")), T::func1(T::var("A"), T::var("B")), T::list(T::var("B")))],
        "map2"      => vec![T::func2(T::list(T::tup2(T::var("A"), T::var("B"))), T::func2(T::var("A"), T::var("B"), T::var("C")), T::list(T::var("C")))],
        "max"       => vec![T::func1(T::list(T::Num), T::Num)],
        "num"       => vec![T::func1(T::Str, T::Num)],
        "op+"       => vec![T::func2(T::Num, T::Num, T::Num),
                            T::func2(T::Str, T::Num, T::Str),
                            T::func2(T::Str, T::Str, T::Str)],
        "op-"       => vec![T::func2(T::Num, T::Num, T::Num)],
        "op*"       => vec![T::func2(T::Num, T::Num, T::Num)],
        "op/"       => vec![T::func2(T::Num, T::Num, T::Num)],
        "op%"       => vec![T::func2(T::Num, T::Num, T::Num)],
        "op=="      => vec![T::func2(T::Num, T::Num, T::Bool),
                            T::func2(T::Str, T::Str, T::Bool),
                            T::func2(T::Bool, T::Bool, T::Bool)],
        "op!="      => vec![T::func2(T::Num, T::Num, T::Bool),
                            T::func2(T::Str, T::Str, T::Bool),
                            T::func2(T::Bool, T::Bool, T::Bool)],
        "op>="      => vec![T::func2(T::Num, T::Num, T::Bool)],
        "op<="      => vec![T::func2(T::Num, T::Num, T::Bool)],
        "op>"       => vec![T::func2(T::Num, T::Num, T::Bool)],
        "op<"       => vec![T::func2(T::Num, T::Num, T::Bool)],
        "pi"        => vec![T::Num],
        "push"      => vec![T::func2(T::list(T::var("A")), T::var("A"), T::list(T::var("A")))],
        "range"     => vec![T::func2(T::Num, T::Num, T::list(T::Num))],
        "reduce"    => vec![T::func3(T::list(T::var("A")), T::var("B"), T::func2(T::var("B"), T::var("A"), T::var("B")), T::var("B"))],
        "reverse"   => vec![T::func1(T::list(T::var("A")), T::list(T::var("A")))],
        "split"     => vec![T::func2(T::Str, T::Str, T::list(T::Str))],
        "sqrt"      => vec![T::func1(T::Num, T::Num)],
        "transpose" => vec![T::func1(T::Mat, T::Mat)],
        "zip"       => vec![T::func2(T::list(T::var("A")), T::list(T::var("B")), T::list(T::tup2(T::var("A"), T::var("B"))))],
        _           => vec![]
    }
}