/* builtintypes.rs
 *
 * Function for getting the type of builtin functions.
 */

use crate::parser::parse_type;
use crate::types::Type as T;

macro_rules! ts {
    ( $( $x:literal ),* ) => (vec![$(parse_type($x.to_owned(), &vec!["A".to_owned(), "B".to_owned(), "C".to_owned()]).unwrap()),*])
}

pub fn type_check_builtin(name: &str) -> Vec<T> {
    match name {
        "abs"       => ts!["(Num) -> Num", "(Mat) -> Mat"],
        "argmax"    => ts!["([Num]) -> Num"],
        "argmin"    => ts!["([Num]) -> Num"],
        "conj"      => ts!["(Num) -> Num", "(Mat) -> Mat"],
        "det"       => ts!["(Mat) -> Num"],
        "e"         => ts!["Num"],
        "eye"       => ts!["(Num) -> Mat"],
        "filter"    => ts!["([A], (A) -> Bool) -> [A]"],
        "inv"       => ts!["(Num) -> Num", "(Mat) -> Mat"],
        "join"      => ts!["([Str], Str) -> Str"],
        "len"       => ts!["([A]) -> Num", "(Str) -> Num"],
        "map"       => ts!["([A], (A) -> B) -> [B]"],
        "map2"      => ts!["([(A, B)], (A, B) -> C) -> [C]"],
        "max"       => ts!["([Num]) -> Num"],
        "num"       => ts!["(Str) -> Num"],
        "op+"       => ts!["(Num, Num) -> Num", "(Str, Str) -> Str", "(Mat, Mat) -> Mat",
                           "(Num, Mat) -> Mat", "(Mat, Num) -> Mat", "([A], [A]) -> [A]"],
        "op-"       => ts!["(Num, Num) -> Num", "(Mat, Mat) -> Mat", "(Num, Mat) -> Mat", "(Mat, Num) -> Mat"],
        "op*"       => ts!["(Num, Num) -> Num", "(Mat, Mat) -> Mat", "(Mat, Num) -> Mat",
                           "(Num, Mat) -> Mat", "(Str, Num) -> Str", "([A], Num) -> [A]"],
        "op/"       => ts!["(Num, Num) -> Num", "(Num, Mat) -> Mat", "(Mat, Num) -> Mat", "(Mat, Mat) -> Mat"],
        "op%"       => ts!["(Num, Num) -> Num"],
        "op**"      => ts!["(Num, Num) -> Num"],
        "op=="      => ts!["(Num, Num) -> Bool", "(Mat, Mat) -> Bool", "(Bool, Bool) -> Bool",
                           "(Str, Str) -> Bool", "([A], [A]) -> Bool"],
        "op!="      => ts!["(Num, Num) -> Bool", "(Mat, Mat) -> Bool", "(Bool, Bool) -> Bool",
                           "(Str, Str) -> Bool", "([A], [A]) -> Bool"],
        "op>="      => ts!["(Num, Num) -> Bool"],
        "op<="      => ts!["(Num, Num) -> Bool"],
        "op>"       => ts!["(Num, Num) -> Bool"],
        "op<"       => ts!["(Num, Num) -> Bool"],
        "op&&"      => ts!["(Bool, Bool) -> Bool"],
        "op||"      => ts!["(Bool, Bool) -> Bool"],
        "pi"        => ts!["Num"],
        "push"      => ts!["([A], A) -> [A]"],
        "range"     => ts!["(Num, Num) -> [Num]"],
        "reduce"    => ts!["([A], B, (B, A) -> B) -> B"],
        "reverse"   => ts!["(Str) -> Str", "([A]) -> [A]"],
        "shape"     => ts!["(Mat) -> (Num, Num)"],
        "split"     => ts!["(Str, Str) -> [Str]"],
        "sqrt"      => ts!["(Num) -> Num"],
        "transpose" => ts!["(Mat) -> Mat"],
        "zip"       => ts!["([A], [B]) -> [(A, B)]"],
        _           => ts![]
    }
}