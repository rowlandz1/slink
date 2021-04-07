use crate::Rule;
//use std::result::Result;
//use pest::error::Error;
use pest::iterators::Pair;

use AstStmt::*;
use AstExpr::*;

#[derive(Debug)]
pub enum AstStmt {
    Assign(String, Box<AstExpr>),
    Display(Box<AstExpr>),
}

#[derive(Debug)]
pub enum AstExpr {
    Binop(String, Box<AstExpr>, Box<AstExpr>),
    Lambda(Vec<String>, Box<AstExpr>),
    FunApp(Box<AstExpr>, Vec<AstExpr>),
    Matrix(usize, usize, Vec<AstExpr>),
    Num(f64),
    Id(String),
}

impl ToOwned for AstExpr {
    type Owned = AstExpr;

    fn to_owned(&self) -> AstExpr {
        match self {
            Binop(op, lhs, rhs) => Binop(op.to_owned(), Box::new((*lhs).to_owned()), Box::new((*rhs).to_owned())),
            Lambda(params, inner_expr) => Lambda(params.to_vec(), Box::new((*inner_expr).to_owned())),
            FunApp(f, args) => {
                let mut newargs: Vec<AstExpr> = vec![];
                for i in 0..args.len() {
                    newargs.push(args[i].to_owned());
                }
                FunApp(Box::new((*f).to_owned()), newargs)
            }
            Matrix(r, c, v) => {
                let mut newv: Vec<AstExpr> = vec![];
                for i in 0..v.len() {
                    newv.push(v[i].to_owned());
                }
                Matrix(*r, *c, newv)
            }
            Num(n) => Num(*n),
            Id(x) => Id(x.to_owned()),
        }
    }
}

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
        Rule::expr1 => {
            let mut inner_rules = expr.into_inner();
            let mut ret = get_ast_expr(inner_rules.next().unwrap());

            loop {
                let op = inner_rules.next();
                match op {
                    Some(op) => {
                        let op = op.as_str().to_string();
                        let operand2 = get_ast_expr(inner_rules.next().unwrap());
                        ret = Binop(op, Box::new(ret), Box::new(operand2));
                    }
                    None => break,
                }
            }

            ret
        }
        Rule::expr2 |
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
        Rule::lam_expr => {
            let mut param_vec: Vec<String> = vec![];
            let mut inner_rules = expr.into_inner();
            for pair in inner_rules.next().unwrap().into_inner() {
                param_vec.push(pair.as_str().to_string());
            }
            let inner_expr = get_ast_expr(inner_rules.next().unwrap());
            Lambda(param_vec, Box::new(inner_expr))
        }
        Rule::fun_app => {
            let mut arg_vec: Vec<AstExpr> = vec![];
            let mut inner_rules = expr.into_inner();
            let f = get_ast_expr(inner_rules.next().unwrap());
            for pair in inner_rules.next().unwrap().into_inner() {
                arg_vec.push(get_ast_expr(pair));
            }
            FunApp(Box::new(f), arg_vec)
        }
        Rule::ID => {
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
