/* parser.rs
 *
 * Converts Pest abstract syntax into the abstract structures defined in ast.rs
 * get_ast_stmt: parses into a AstStmt
 * get_ast_expr: parses into a AstExpr
 */

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
        Rule::expr2 => {
            let mut inner_rules = expr.into_inner();
            let mut ret = get_ast_expr(inner_rules.next().unwrap());

            loop {
                let op = inner_rules.next();
                if let Some(op) = op {
                    let op = op.as_str().to_string();
                    let operand2 = get_ast_expr(inner_rules.next().unwrap());
                    ret = Binop(op, Box::new(ret), Box::new(operand2));
                } else { break; }
            }

            ret
        }
        Rule::expr3 => { // Function application
            let mut inner_rules = expr.into_inner();
            let mut ret = get_ast_expr(inner_rules.next().unwrap());

            loop {
                if let Some(arglist) = inner_rules.next() {
                    let mut arg_vec: Vec<AstArg> = vec![];
                    for arg in arglist.into_inner() {
                        let a = arg.into_inner().next().unwrap();
                        match a.as_rule() {
                            Rule::QUEST => arg_vec.push(AstArg::Question),
                            Rule::expr => arg_vec.push(AstArg::Expr(Box::new(get_ast_expr(a)))),
                            _ => unreachable!()
                        }
                    }
                    ret = FunApp(Box::new(ret), arg_vec);
                } else { break; }
            }
            ret
        }
        Rule::expr4 |
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
        Rule::tuple => Tuple(expr.into_inner().map(|x| get_ast_expr(x)).collect()),
        Rule::lam_expr => {
            let mut param_vec: Vec<String> = vec![];
            let mut inner_rules = expr.into_inner();
            for pair in inner_rules.next().unwrap().into_inner() {
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
        Rule::ID |
        Rule::OPID => {
            Id(expr.as_str().to_string())
        }
        Rule::NUM => {
            let value: f64 = expr.as_str().parse()
                .expect("Cannot parse number");
            Num(value)
        }
        _ => unreachable!()
    }
}
