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
        "det"       => T::func1(T::Matrix, T::Num),
        "e"         => T::Num,
        "eye"       => T::func1(T::Num, T::Matrix),
        "filter"    => T::func2(T::list(T::var("A")), T::func1(T::var("A"), T::Bool), T::list(T::var("A"))),
        "inv"       => T::func1(T::Matrix, T::Matrix),
        "join"      => T::func2(T::list(T::String), T::String, T::String),
        "len"       => T::func1(T::list(T::var("A")), T::Num),
        "map"       => T::func2(T::list(T::var("A")), T::func1(T::var("A"), T::var("B")), T::list(T::var("B"))),
        "max"       => T::func1(T::list(T::Num), T::Num),
        "num"       => T::func1(T::String, T::Num),
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
        "split"     => T::func2(T::String, T::String, T::list(T::String)),
        "sqrt"      => T::func1(T::Num, T::Num),
        "transpose" => T::func1(T::Matrix, T::Matrix),
        "zip"       => T::func2(T::list(T::var("A")), T::list(T::var("B")), T::list(T::tup2(T::var("A"), T::var("B")))),
        _ => { return None; }
    })
}