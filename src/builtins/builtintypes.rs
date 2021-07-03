/* builtintypes.rs
 *
 * Function for getting the type of builtin functions.
 */

use crate::parser::parse_type;
use crate::types::Type as T;

macro_rules! ts {
    ( $( $x:literal ),* ) => (vec![$(parse_type($x.to_owned(), &vec!["A".to_owned(), "B".to_owned(), "C".to_owned()])),*])
}

pub fn type_check_builtin(name: &str) -> Vec<T> {
    match name {
        "abs"       => ts!["(Num) -> Num"],
        "argmax"    => ts!["(List<Num>) -> Num"],
        "argmin"    => ts!["(List<Num>) -> Num"],
        "conj"      => ts!["(Num) -> Num"],
        "det"       => ts!["(Mat) -> Num"],
        "e"         => ts!["Num"],
        "eye"       => ts!["(Num) -> Mat"],
        "filter"    => ts!["(List<A>, (A) -> Bool) -> List<A>"],
        "inv"       => ts!["(Mat) -> Mat"],
        "join"      => ts!["(List<Str>, Str) -> Str"],
        "len"       => ts!["(List<A>) -> Num", "(Str) -> Num"],
        "map"       => ts!["(List<A>, (A) -> B) -> List<B>"],
        "map2"      => ts!["(List<(A, B)>, (A, B) -> C) -> List<C>"],
        "max"       => ts!["(List<Num>) -> Num"],
        "num"       => ts!["(Str) -> Num"],
        "op+"       => ts!["(Num, Num) -> Num", "(Str, Str) -> Str"],
        "op-"       => ts!["(Num, Num) -> Num"],
        "op*"       => ts!["(Num, Num) -> Num", "(Mat, Num) -> Mat", "(Num, Mat) -> Mat", "(Mat, Mat) -> Mat"],
        "op/"       => ts!["(Num, Num) -> Num"],
        "op%"       => ts!["(Num, Num) -> Num"],
        "op=="      => ts!["(Num, Num) -> Bool", "(Str, Str) -> Bool", "(Bool, Bool) -> Bool"],
        "op!="      => ts!["(Num, Num) -> Bool", "(Str, Str) -> Bool", "(Bool, Bool) -> Bool"],
        "op>="      => ts!["(Num, Num) -> Bool"],
        "op<="      => ts!["(Num, Num) -> Bool"],
        "op>"       => ts!["(Num, Num) -> Bool"],
        "op<"       => ts!["(Num, Num) -> Bool"],
        "op&&"      => ts!["(Bool, Bool) -> Bool"],
        "op||"      => ts!["(Bool, Bool) -> Bool"],
        "pi"        => ts!["Num"],
        "push"      => ts!["(List<A>, A) -> List<A>"],
        "range"     => ts!["(Num, Num) -> List<Num>"],
        "reduce"    => ts!["(List<A>, B, (B, A) -> B) -> B"],
        "reverse"   => ts!["(List<A>) -> List<A>"],
        "shape"     => ts!["(Mat) -> (Num, Num)"],
        "split"     => ts!["(Str, Str) -> List<Str>"],
        "sqrt"      => ts!["(Num) -> Num"],
        "transpose" => ts!["(Mat) -> Mat"],
        "zip"       => ts!["(List<A>, List<B>) -> List<(A, B)>"],
        _           => ts![]
    }
}