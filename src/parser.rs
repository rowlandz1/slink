/* parser.rs
 *
 * Converts Pest abstract syntax into the abstract structures defined in ast.rs
 * get_ast_stmt: parses into a AstStmt
 * get_ast_expr: parses into a AstExpr
 */

use std::collections::HashMap;
use crate::Rule;
use pest::iterators::Pair;
use crate::ast::*;
use {AstStmt::*, AstExpr::*};

pub fn get_ast_stmt(stmt: Pair<Rule>) -> AstStmt {
    match stmt.as_rule() {
        Rule::stmt => {
            get_ast_stmt(stmt.into_inner().next().unwrap())
        }
        Rule::stmt_assign => {
            let mut inner_rules = stmt.into_inner();
            let id = inner_rules.next().unwrap().as_str().to_string();
            let expr = get_ast_expr(inner_rules.next().unwrap());
            Assign(id, Box::new(expr))
        }
        Rule::expr => {
            let expr = get_ast_expr(stmt);
            Display(Box::new(expr))
        }
        _ => unreachable!()
    }
}

pub fn get_ast_expr(expr: Pair<Rule>) -> AstExpr {
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
                    ret = Binop(op, Box::new(ret), Box::new(operand2));
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
                    ret = Binop(op, Box::new(operand2), Box::new(ret));
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
                    ret = Unop(op, Box::new(ret));
                } else { break; }
            }
            ret
        }
        Rule::expr8 => { // Function application
            let mut inner_rules = expr.into_inner();
            let mut ret = get_ast_expr(inner_rules.next().unwrap());

            loop {
                if let Some(funargs) = inner_rules.next() {
                    let funargs = funargs.into_inner().next().unwrap();
                    match funargs.as_rule() {
                        Rule::arg_list => {
                            ret = FunApp(Box::new(ret), parse_arg_list(funargs));
                        }
                        Rule::kwarg_list => {
                            ret = FunKwApp(Box::new(ret), parse_kwarg_list(funargs));
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
            let mut firstrow: Vec<AstExpr> = vec![];
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
            Matrix(numrows, numcols, firstrow)
        }
        Rule::list => List(expr.into_inner().map(|x| get_ast_expr(x)).collect()),
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
            Lambda(param_vec, Box::new(inner_expr))
        }
        Rule::let_expr => {
            let mut inner_rules = expr.into_inner();
            let mut bindings: Vec<(String, AstExpr)> = vec![];
            let mut inner1 = inner_rules.next().unwrap().into_inner();
            loop {
                if let Some(id) = inner1.next() {
                    let id = id.as_str().to_string();
                    let boundexpr = get_ast_expr(inner1.next().unwrap());
                    bindings.push((id, boundexpr));
                } else { break; }
            }
            let inner_expr = get_ast_expr(inner_rules.next().unwrap());
            Let(bindings, Box::new(inner_expr))
        }
        Rule::tuple => Tuple(expr.into_inner().map(|x| get_ast_expr(x)).collect()),
        Rule::bool => Bool(expr.as_str().eq("true")),
        Rule::ID |
        Rule::OPID => {
            Id(expr.as_str().to_string())
        }
        Rule::MACROID => {
            Macro(expr.as_str().to_string())
        }
        Rule::FLOATIMAG => {
            let value: f64 = expr.as_str().trim_end_matches("i").parse()
                .expect("Cannot parse float imaginary");
            FloatImag(value)
        }
        Rule::INTIMAG => {
            let value: i32 = expr.as_str().trim_end_matches("i").parse()
                .expect("Cannot parse int imaginary");
            IntImag(value)
        }
        Rule::FLOAT => {
            let value: f64 = expr.as_str().parse()
                .expect("Cannot parse number");
            Num(value)
        }
        Rule::INT => {
            let value: i32 = expr.as_str().parse()
                .expect("Cannot parse number");
            Int(value)
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

fn parse_kwarg_list(kwarg_list: Pair<Rule>) -> HashMap<String, AstExpr> {
    let mut kwarg_map: HashMap<String, AstExpr> = HashMap::new();
    for arg in kwarg_list.into_inner() {
        let mut inner_rules = arg.into_inner();
        let id = inner_rules.next().unwrap().as_str().to_string();
        let expr = get_ast_expr(inner_rules.next().unwrap());
        kwarg_map.insert(id, expr);
    }
    kwarg_map
}
