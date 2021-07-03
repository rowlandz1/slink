/* parser.rs
 *
 * Parses a String into the abstract structures defined in ast.rs
 * parse_stmt: parses into a AstStmt
 * parse_type: parses into a typechecker::Type
 */

use pest::Parser;
use pest::iterators::Pair;
use std::collections::HashMap;
use crate::ast::{AstExpr as E, AstStmt, AstSlice};
use crate::types::Type;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct SciLangParser;

pub fn parse_stmt(mut text: String) -> AstStmt {
    let stmt: Pair<Rule> = SciLangParser::parse(Rule::stmt, &mut text)
        .expect("Unsuccessful parse")
        .next().unwrap();
    parse_stmt_pair(stmt)
}

pub fn parse_type(mut text: String, tvars: &Vec<String>) -> Type {
    let typ: Pair<Rule> = SciLangParser::parse(Rule::type_expr, &mut text)
        .expect("Unsuccessful parse of type")
        .next().unwrap();
    parse_type_pair(typ, tvars)
}

fn parse_stmt_pair(stmt: Pair<Rule>) -> AstStmt {
    match stmt.as_rule() {
        Rule::stmt => {
            parse_stmt_pair(stmt.into_inner().next().unwrap())
        }
        Rule::stmt_assign => {
            let mut inner_rules = stmt.into_inner();
            let id = inner_rules.next().unwrap().as_str().to_string();
            let expr = parse_expr_pair(inner_rules.next().unwrap());
            AstStmt::Assign(id, Box::new(expr))
        }
        Rule::expr => {
            let expr = parse_expr_pair(stmt);
            AstStmt::Display(Box::new(expr))
        }
        _ => unreachable!()
    }
}

fn parse_expr_pair(expr: Pair<Rule>) -> E {
    match expr.as_rule() {
        Rule::expr |
        Rule::expr1 |
        Rule::expr2 |
        Rule::expr3 |
        Rule::expr4 |
        Rule::expr7 => {
            let mut inner_rules = expr.into_inner();
            let mut ret = parse_expr_pair(inner_rules.next().unwrap());
            loop {
                if let Some(op) = inner_rules.next() {
                    let op = op.as_str().to_string();
                    let operand2 = parse_expr_pair(inner_rules.next().unwrap());
                    ret = E::binop(op, ret, operand2);
                } else { break; }
            }
            ret
        }
        Rule::expr5 => {
            let mut inner_rules = expr.into_inner().rev();
            let mut ret = parse_expr_pair(inner_rules.next().unwrap());
            loop {
                if let Some(op) = inner_rules.next() {
                    let op = op.as_str().to_string();
                    let operand2 = parse_expr_pair(inner_rules.next().unwrap());
                    ret = E::binop(op, operand2, ret);
                } else { break; }
            }
            ret
        }
        Rule::expr6 => {
            let mut inner_rules = expr.into_inner().rev();
            let mut ret = parse_expr_pair(inner_rules.next().unwrap());

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
            let mut ret = parse_expr_pair(inner_rules.next().unwrap());

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
            parse_expr_pair(expr.into_inner().next().unwrap())
        }
        Rule::matrix => {
            let mut firstrow: Vec<E> = Vec::new();
            let mut inner_rules = expr.into_inner();
            for pair in inner_rules.next().unwrap().into_inner() {
                firstrow.push(parse_expr_pair(pair));
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
                            firstrow.push(parse_expr_pair(pair2));
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
        Rule::list => E::list(expr.into_inner().map(|x| parse_expr_pair(x)).collect()),
        Rule::lam_expr => {
            let mut inner_rules = expr.into_inner();
            let type_param_list = parse_type_param_list(inner_rules.next().unwrap());
            let (params, paramtypes) = parse_param_list(inner_rules.next().unwrap(), &type_param_list);
            let ret_type = match inner_rules.next().unwrap().into_inner().next() {
                Some(t) => parse_type_pair(t, &type_param_list),
                None => Type::Any,
            };
            let inner_expr = parse_expr_pair(inner_rules.next().unwrap());
            E::Lambda(params, Box::new(inner_expr), type_param_list, paramtypes, ret_type)
        }
        Rule::lam_basic => {
            let mut inner_rules = expr.into_inner();
            let id = inner_rules.next().unwrap().as_str().to_string();
            let body = parse_expr_pair(inner_rules.next().unwrap());
            E::Lambda(vec![id], Box::new(body), Vec::new(), vec![Type::Any], Type::Any)
        }
        Rule::tuple => E::tuple(expr.into_inner().map(|x| parse_expr_pair(x)).collect()),
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

fn parse_type_param_list(pair: Pair<Rule>) -> Vec<String> {
    let mut ids: Vec<String> = Vec::new();
    for type_param in pair.into_inner() {
        let s = type_param.as_str().to_string();
        if ids.contains(&s) { panic!("Cannot bind same type parameter twice"); }
        ids.push(s);
    }
    ids
}

fn parse_param_list(param_list: Pair<Rule>, type_params: &Vec<String>) -> (Vec<String>, Vec<Type>) {
    let mut ids: Vec<String> = Vec::new();
    let mut types: Vec<Type> = Vec::new();
    for param in param_list.into_inner() {
        let mut inner_rules = param.into_inner();
        let name = inner_rules.next().unwrap().as_str().to_string();
        if ids.contains(&name) { panic!("Cannot bind same variable twice"); }
        let typ = match inner_rules.next() {
            Some(t) => parse_type_pair(t, type_params),
            None => Type::Any
        };
        ids.push(name);
        types.push(typ);
    }
    (ids, types)
}

fn parse_arg_list(arg_list: Pair<Rule>) -> Vec<E> {
    arg_list.into_inner().map(parse_expr_pair).collect()
}

fn parse_kwarg_list(kwarg_list: Pair<Rule>) -> HashMap<String, E> {
    let mut kwarg_map: HashMap<String, E> = HashMap::new();
    for arg in kwarg_list.into_inner() {
        let mut inner_rules = arg.into_inner();
        let id = inner_rules.next().unwrap().as_str().to_string();
        let expr = parse_expr_pair(inner_rules.next().unwrap());
        kwarg_map.insert(id, expr);
    }
    kwarg_map
}

fn parse_slice(slice: Pair<Rule>) -> AstSlice {
    let slice = slice.into_inner().next().unwrap();
    match slice.as_rule() {
        Rule::slice_ny => {
            let upper = parse_expr_pair(slice.into_inner().next().unwrap());
            AstSlice::Range(None, Some(Box::new(upper)))
        }
        Rule::slice_nn => {
            AstSlice::Range(None, None)
        }
        Rule::slice_yy => {
            let mut inner_rules = slice.into_inner();
            let lower = parse_expr_pair(inner_rules.next().unwrap());
            let upper = parse_expr_pair(inner_rules.next().unwrap());
            AstSlice::Range(Some(Box::new(lower)), Some(Box::new(upper)))
        }
        Rule::slice_yn => {
            let lower = parse_expr_pair(slice.into_inner().next().unwrap());
            AstSlice::Range(Some(Box::new(lower)), None)
        }
        Rule::expr => {
            AstSlice::Single(Box::new(parse_expr_pair(slice)))
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

fn parse_type_pair(pair: Pair<Rule>, type_params: &Vec<String>) -> Type {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::function_type => {
            let mut inner_rules = pair.into_inner();
            let mut params: Vec<Type> = Vec::new();
            loop {
                if let Some(t) = inner_rules.next() {
                    params.push(parse_type_pair(t, type_params));
                } else { break; }
            }
            let rettype = params.pop().unwrap();
            Type::Func(params, Box::new(rettype))
        }
        Rule::tuple_type => {
            let mut inner_rules = pair.into_inner();
            let mut params: Vec<Type> = Vec::new();
            loop {
                if let Some(t) = inner_rules.next() {
                    params.push(parse_type_pair(t, type_params));
                } else { break; }
            }
            Type::Tuple(params)
        }
        Rule::compound_type => {
            let mut inner_rules = pair.into_inner();
            let constructor = inner_rules.next().unwrap();
            let mut params: Vec<Type> = Vec::new();
            loop {
                if let Some(t) = inner_rules.next() {
                    params.push(parse_type_pair(t, type_params));
                } else { break; }
            }
            match (constructor.as_str(), params.len()) {
                ("List", 1) => Type::list(params.pop().unwrap()),
                _ => panic!("Parser error, invalid type {}(...)", constructor.as_str())
            }
            
        }
        Rule::atomic_type => match pair.as_str() {
            "Num" => Type::Num,
            "Bool" => Type::Bool,
            "Str" => Type::Str,
            "Mat" => Type::Mat,
            t => if type_params.contains(&t.to_string()) { Type::var(t) }
                 else { panic!("Unrecognized atomic type {}", t) }
        }
        _ => unreachable!()
    }
}