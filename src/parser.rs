/* parser.rs
 *
 * Converts Pest abstract syntax into the abstract structures defined in ast.rs
 * get_ast_stmt: parses into a AstStmt
 * get_ast_expr: parses into a AstExpr
 */

use std::collections::HashMap;
use pest::iterators::Pair;
use crate::ast::{AstExpr as E, AstStmt, AstArg, AstSlice};
use crate::Rule;

pub fn get_ast_stmt(stmt: Pair<Rule>) -> AstStmt {
    match stmt.as_rule() {
        Rule::stmt => {
            get_ast_stmt(stmt.into_inner().next().unwrap())
        }
        Rule::stmt_assign => {
            let mut inner_rules = stmt.into_inner();
            let id = inner_rules.next().unwrap().as_str().to_string();
            let expr = get_ast_expr(inner_rules.next().unwrap());
            AstStmt::Assign(id, Box::new(expr))
        }
        Rule::expr => {
            let expr = get_ast_expr(stmt);
            AstStmt::Display(Box::new(expr))
        }
        _ => unreachable!()
    }
}

pub fn get_ast_expr(expr: Pair<Rule>) -> E {
    match expr.as_rule() {
        Rule::expr |
        Rule::expr1 |
        Rule::expr2 |
        Rule::expr3 |
        Rule::expr4 |
        Rule::expr7 => {
            let mut inner_rules = expr.into_inner();
            let mut ret = get_ast_expr(inner_rules.next().unwrap());
            loop {
                if let Some(op) = inner_rules.next() {
                    let op = op.as_str().to_string();
                    let operand2 = get_ast_expr(inner_rules.next().unwrap());
                    ret = E::binop(op, ret, operand2);
                } else { break; }
            }
            ret
        }
        Rule::expr5 => {
            let mut inner_rules = expr.into_inner().rev();
            let mut ret = get_ast_expr(inner_rules.next().unwrap());
            loop {
                if let Some(op) = inner_rules.next() {
                    let op = op.as_str().to_string();
                    let operand2 = get_ast_expr(inner_rules.next().unwrap());
                    ret = E::binop(op, operand2, ret);
                } else { break; }
            }
            ret
        }
        Rule::expr6 => {
            let mut inner_rules = expr.into_inner().rev();
            let mut ret = get_ast_expr(inner_rules.next().unwrap());

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
            let mut ret = get_ast_expr(inner_rules.next().unwrap());

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
            get_ast_expr(expr.into_inner().next().unwrap())
        }
        Rule::matrix => {
            let mut firstrow: Vec<E> = vec![];
            let mut inner_rules = expr.into_inner();
            for pair in inner_rules.next().unwrap().into_inner() {
                firstrow.push(get_ast_expr(pair));
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
                            firstrow.push(get_ast_expr(pair2));
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
        Rule::list => E::list(expr.into_inner().map(|x| get_ast_expr(x)).collect()),
        Rule::lam_expr => {
            let mut param_vec: Vec<String> = vec![];
            let mut inner_rules = expr.into_inner();
            for pair in inner_rules.next().unwrap().into_inner() {
                let s = pair.as_str().to_string();
                if param_vec.contains(&s) {
                    panic!("Cannot bind same variable twice");
                }
                param_vec.push(pair.as_str().to_string());
            }
            let inner_expr = get_ast_expr(inner_rules.next().unwrap());
            E::lambda(param_vec, inner_expr)
        }
        // Rule::let_expr => {
        //     let mut inner_rules = expr.into_inner();
        //     let mut bindings: Vec<(String, E)> = vec![];
        //     let mut inner1 = inner_rules.next().unwrap().into_inner();
        //     loop {
        //         if let Some(id) = inner1.next() {
        //             let id = id.as_str().to_string();
        //             let boundexpr = get_ast_expr(inner1.next().unwrap());
        //             bindings.push((id, boundexpr));
        //         } else { break; }
        //     }
        //     let inner_expr = get_ast_expr(inner_rules.next().unwrap());
        //     E::let_expr(bindings, inner_expr)
        // }
        Rule::tuple => E::tuple(expr.into_inner().map(|x| get_ast_expr(x)).collect()),
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

fn parse_arg_list(arg_list: Pair<Rule>) -> Vec<AstArg> {
    let mut arg_vec: Vec<AstArg> = vec![];
    for arg in arg_list.into_inner() {
        let a = arg.into_inner().next().unwrap();
        match a.as_rule() {
            Rule::QUEST => arg_vec.push(AstArg::Question),
            Rule::expr => arg_vec.push(AstArg::Expr(Box::new(get_ast_expr(a)))),
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
        let expr = get_ast_expr(inner_rules.next().unwrap());
        kwarg_map.insert(id, expr);
    }
    kwarg_map
}

fn parse_slice(slice: Pair<Rule>) -> AstSlice {
    let a = slice.into_inner().next().unwrap();
    match a.as_rule() {
        Rule::slice_ny => {
            let upper = get_ast_expr(a.into_inner().next().unwrap());
            AstSlice::Range(None, Some(Box::new(upper)))
        }
        Rule::slice_nn => {
            AstSlice::Range(None, None)
        }
        Rule::slice_yy => {
            let mut inner_rules = a.into_inner();
            let lower = get_ast_expr(inner_rules.next().unwrap());
            let upper = get_ast_expr(inner_rules.next().unwrap());
            AstSlice::Range(Some(Box::new(lower)), Some(Box::new(upper)))
        }
        Rule::slice_yn => {
            let lower = get_ast_expr(a.into_inner().next().unwrap());
            AstSlice::Range(Some(Box::new(lower)), None)
        }
        Rule::expr => {
            AstSlice::Single(Box::new(get_ast_expr(a)))
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
