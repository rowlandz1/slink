/* parser.rs
 *
 * Converts Pest abstract syntax into the abstract structures defined in ast.rs
 * get_ast_stmt: parses into a AstStmt
 * get_ast_expr: parses into a AstExpr
 */

use std::collections::HashMap;
use pest::iterators::Pair;
use crate::ast::{AstExpr as E, AstStmt, AstSlice};
use crate::typechecker::Type;
use crate::Rule;

pub fn parse_stmt(stmt: Pair<Rule>) -> AstStmt {
    match stmt.as_rule() {
        Rule::stmt => {
            parse_stmt(stmt.into_inner().next().unwrap())
        }
        Rule::stmt_assign => {
            let mut inner_rules = stmt.into_inner();
            let id = inner_rules.next().unwrap().as_str().to_string();
            let expr = parse_expr(inner_rules.next().unwrap());
            AstStmt::Assign(id, Box::new(expr))
        }
        Rule::expr => {
            let expr = parse_expr(stmt);
            AstStmt::Display(Box::new(expr))
        }
        _ => unreachable!()
    }
}

pub fn parse_expr(expr: Pair<Rule>) -> E {
    match expr.as_rule() {
        Rule::expr |
        Rule::expr1 |
        Rule::expr2 |
        Rule::expr3 |
        Rule::expr4 |
        Rule::expr7 => {
            let mut inner_rules = expr.into_inner();
            let mut ret = parse_expr(inner_rules.next().unwrap());
            loop {
                if let Some(op) = inner_rules.next() {
                    let op = op.as_str().to_string();
                    let operand2 = parse_expr(inner_rules.next().unwrap());
                    ret = E::binop(op, ret, operand2);
                } else { break; }
            }
            ret
        }
        Rule::expr5 => {
            let mut inner_rules = expr.into_inner().rev();
            let mut ret = parse_expr(inner_rules.next().unwrap());
            loop {
                if let Some(op) = inner_rules.next() {
                    let op = op.as_str().to_string();
                    let operand2 = parse_expr(inner_rules.next().unwrap());
                    ret = E::binop(op, operand2, ret);
                } else { break; }
            }
            ret
        }
        Rule::expr6 => {
            let mut inner_rules = expr.into_inner().rev();
            let mut ret = parse_expr(inner_rules.next().unwrap());

            loop {
                if let Some(op) = inner_rules.next() {
                    let op = op.as_str().to_string();
                    ret = E::unop(op, ret);
                } else { break; }
            }
            ret
        }
        Rule::expr8 => { // Function application & indexing
            let mut inner_rules = expr.into_inner();
            let mut ret = parse_expr(inner_rules.next().unwrap());

            loop {
                if let Some(paren_operator) = inner_rules.next() {
                    let mut paren_inner_rules = paren_operator.into_inner();
                    let inside = paren_inner_rules.next().unwrap();
                    match inside.as_rule() {
                        Rule::arg_list => {
                            ret = E::fun_app(ret, parse_arg_list(inside));
                        }
                        Rule::kwarg_list => {
                            ret = E::fun_kw_app(ret, parse_kwarg_list(inside));
                        }
                        Rule::slice => {
                            let slice1 = parse_slice(inside);
                            if let Some(inside) = paren_inner_rules.next() {
                                let slice2 = parse_slice(inside);
                                ret = E::matrix_idx(ret, slice1, slice2)
                            } else {
                                ret = E::list_idx(ret, slice1);
                            }
                        }
                        _ => unreachable!()
                    }
                } else { break; }
            }
            ret
        }
        Rule::expr9 |
        Rule ::expr_base => {
            parse_expr(expr.into_inner().next().unwrap())
        }
        Rule::matrix => {
            let mut firstrow: Vec<E> = vec![];
            let mut inner_rules = expr.into_inner();
            for pair in inner_rules.next().unwrap().into_inner() {
                firstrow.push(parse_expr(pair));
            }
            let numcols: usize = firstrow.len();
            let mut numrows = 1;
            loop {
                let nextrow = inner_rules.next();
                match nextrow {
                    Some(pair) => {
                        numrows += 1;
                        let mut rowlen: usize = 0;
                        for pair2 in pair.into_inner() {
                            firstrow.push(parse_expr(pair2));
                            rowlen += 1;
                        }
                        if rowlen != numcols {
                            panic!("Error, non-rectangular matrix");
                        }
                    }
                    None => break,
                }
            }
            E::matrix(numrows, numcols, firstrow)
        }
        Rule::list => E::list(expr.into_inner().map(|x| parse_expr(x)).collect()),
        Rule::lam_expr => {
            let mut inner_rules = expr.into_inner();
            let (params, paramtypes) = parse_param_list(inner_rules.next().unwrap());
            let ret_type = match inner_rules.next().unwrap().into_inner().next() {
                Some(t) => parse_type_expr(t),
                None => Type::Any,
            };
            let inner_expr = parse_expr(inner_rules.next().unwrap());
            E::Lambda(params, Box::new(inner_expr), paramtypes, ret_type)
            //E::lambda(param_list, inner_expr)
        }
        Rule::tuple => E::tuple(expr.into_inner().map(|x| parse_expr(x)).collect()),
        Rule::bool => E::bool(expr.as_str().eq("true")),
        Rule::string => {
            let s = expr.as_str();
            let s = handle_escape_sequences(&s[1..s.len()-1]);
            E::str(s)
        }
        Rule::ID |
        Rule::OPID => {
            E::id(expr.as_str().to_string())
        }
        Rule::MACROID => {
            E::macro_expr(expr.as_str().to_string())
        }
        Rule::FLOATIMAG => {
            let value: f64 = expr.as_str().trim_end_matches("i").parse()
                .expect("Cannot parse float imaginary");
            E::float_imag(value)
        }
        Rule::INTIMAG => {
            let value: i32 = expr.as_str().trim_end_matches("i").parse()
                .expect("Cannot parse int imaginary");
            E::int_imag(value)
        }
        Rule::FLOAT => {
            let value: f64 = expr.as_str().parse()
                .expect("Cannot parse number");
            E::num(value)
        }
        Rule::INT => {
            let value: i32 = expr.as_str().parse()
                .expect("Cannot parse number");
            E::int(value)
        }
        _ => unreachable!()
    }
}

fn parse_param_list(param_list: Pair<Rule>) -> (Vec<String>, Vec<Type>) {
    let mut ids: Vec<String> = Vec::new();
    let mut types: Vec<Type> = Vec::new();
    for param in param_list.into_inner() {
        let mut inner_rules = param.into_inner();
        let name = inner_rules.next().unwrap().as_str().to_string();
        if ids.contains(&name) {
            panic!("Cannot bind same variable twice");
        }
        let typ = match inner_rules.next() {
            Some(t) => parse_type_expr(t),
            None => Type::Any
        };
        ids.push(name);
        types.push(typ);
    }
    (ids, types)
}

fn parse_arg_list(arg_list: Pair<Rule>) -> Vec<E> {
    let mut arg_vec: Vec<E> = vec![];
    for arg in arg_list.into_inner() {
        let a = arg.into_inner().next().unwrap();
        match a.as_rule() {
            Rule::QUEST => arg_vec.push(E::Id(String::from("_"))),
            Rule::expr => arg_vec.push(parse_expr(a)),
            _ => unreachable!()
        }
    }
    arg_vec
}

fn parse_kwarg_list(kwarg_list: Pair<Rule>) -> HashMap<String, E> {
    let mut kwarg_map: HashMap<String, E> = HashMap::new();
    for arg in kwarg_list.into_inner() {
        let mut inner_rules = arg.into_inner();
        let id = inner_rules.next().unwrap().as_str().to_string();
        let expr = parse_expr(inner_rules.next().unwrap());
        kwarg_map.insert(id, expr);
    }
    kwarg_map
}

fn parse_slice(slice: Pair<Rule>) -> AstSlice {
    let a = slice.into_inner().next().unwrap();
    match a.as_rule() {
        Rule::slice_ny => {
            let upper = parse_expr(a.into_inner().next().unwrap());
            AstSlice::Range(None, Some(Box::new(upper)))
        }
        Rule::slice_nn => {
            AstSlice::Range(None, None)
        }
        Rule::slice_yy => {
            let mut inner_rules = a.into_inner();
            let lower = parse_expr(inner_rules.next().unwrap());
            let upper = parse_expr(inner_rules.next().unwrap());
            AstSlice::Range(Some(Box::new(lower)), Some(Box::new(upper)))
        }
        Rule::slice_yn => {
            let lower = parse_expr(a.into_inner().next().unwrap());
            AstSlice::Range(Some(Box::new(lower)), None)
        }
        Rule::expr => {
            AstSlice::Single(Box::new(parse_expr(a)))
        }
        _ => unreachable!()
    }
}

fn handle_escape_sequences(s: &str) -> String {
    s.replace("\\t", "\t")
    .replace("\\n", "\n")
    .replace("\\\"", "\"")
    .replace("\\\'", "\'")
}

fn parse_type_expr(pair: Pair<Rule>) -> Type {
    let mut inner_rules = pair.into_inner();
    let constructor = inner_rules.next().unwrap();
    let mut parameters: Vec<Type> = Vec::new();
    loop {
        match inner_rules.next() {
            Some(t) => parameters.push(parse_type_expr(t)),
            None => break,
        }
    }
    match parameters.len() {
        0 => match constructor.as_str() {
            "Num" => Type::Num,
            "Str" => Type::Str,
            "Bool" => Type::Bool,
            "Mat" => Type::Mat,
            t => panic!("Unrecognized type: {}", t)
        },
        1 => match constructor.as_str() {
            "List" => Type::list(parameters.pop().unwrap()),
            t => panic!("Unrecognized type: {}", t)
        }
        _ => match constructor.as_str() {
            "Tup" => Type::Tuple(parameters),
            "Fun" => {
                let rettype = parameters.pop().unwrap();
                Type::Func(parameters, Box::new(rettype))
            }
            t => panic!("Unrecognized type constructor: {}", t)
        }
    }
}