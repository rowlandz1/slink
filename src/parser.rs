/* parser.rs
 *
 * Parses a String into the abstract structures defined in ast.rs
 * parse_stmt: parses into a ast::StmtA
 * parse_type: parses into a typechecker::Type
 */

use pest::Parser;
use pest::iterators::Pair;
use std::collections::HashMap;
use crate::ast::{Expr as E, ExprA, Stmt, StmtA, AstSlice};
use crate::error::{ParserError, ParserResult};
use crate::types::Type;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct SlinkParser;

pub fn parse_stmt(text: &String) -> ParserResult<StmtA> {
    match SlinkParser::parse(Rule::stmt, text) {
        Ok(mut pairs) => parse_stmt_pair(pairs.next().unwrap()),
        Err(err) => { Err(ParserError::PestError(err)) }
    }
}

pub fn parse_type(text: String, tvars: &Vec<String>) -> ParserResult<Type> {
    let typ: Pair<Rule> = SlinkParser::parse(Rule::type_expr, &text)
        .expect("Unsuccessful parse of type")
        .next().unwrap();
    parse_type_pair(typ, tvars)
}

fn parse_stmt_pair(stmt: Pair<Rule>) -> ParserResult<StmtA> {
    let start = stmt.as_span().start_pos().line_col();
    let end = stmt.as_span().end_pos().line_col();
    let stmt = match stmt.as_rule() {
        Rule::stmt => {
            return parse_stmt_pair(stmt.into_inner().next().unwrap());
        }
        Rule::stmt_assign => {
            let mut inner_rules = stmt.into_inner();
            let id = inner_rules.next().unwrap().as_str().to_string();
            let expr = parse_expr_pair(inner_rules.next().unwrap())?;
            Stmt::Assign(id, expr)
        }
        Rule::expr => {
            let expr = parse_expr_pair(stmt)?;
            Stmt::Display(expr)
        }
        _ => unreachable!()
    };
    Ok(StmtA{start, end, stmt: Box::new(stmt)})
}

fn parse_expr_pair(expr: Pair<Rule>) -> ParserResult<ExprA> {
    let start = expr.as_span().start_pos().line_col();
    let end = expr.as_span().end_pos().line_col();
    let expr = match expr.as_rule() {
        Rule::expr |
        Rule::expr1 |
        Rule::expr2 |
        Rule::expr3 |
        Rule::expr4 |
        Rule::expr7 => {
            let mut inner_rules = expr.into_inner();
            let mut ret = parse_expr_pair(inner_rules.next().unwrap())?;
            loop {
                if let Some(op) = inner_rules.next() {
                    let op = op.as_str().to_string();
                    let operand2 = parse_expr_pair(inner_rules.next().unwrap())?;
                    let (start, end) = (ret.start, operand2.end);
                    ret = ExprA{start, end, expr: Box::new(E::Binop(op, ret, operand2))};
                } else { return Ok(ret); }
            }
        }
        Rule::expr5 => {
            let mut inner_rules = expr.into_inner().rev();
            let mut ret = parse_expr_pair(inner_rules.next().unwrap())?;
            loop {
                if let Some(op) = inner_rules.next() {
                    let op = op.as_str().to_string();
                    let operand2 = parse_expr_pair(inner_rules.next().unwrap())?;
                    let (start, end) = (operand2.start, ret.end);
                    ret = ExprA{start, end, expr: Box::new(E::Binop(op, operand2, ret))};
                } else { return Ok(ret); }
            }
        }
        Rule::expr6 => {
            let mut inner_rules = expr.into_inner().rev();
            let mut ret = parse_expr_pair(inner_rules.next().unwrap())?;

            loop {
                if let Some(op) = inner_rules.next() {
                    let start = op.as_span().start_pos().line_col();
                    let end = op.as_span().end_pos().line_col();
                    let op = op.as_str().to_string();
                    ret = ExprA{start, end, expr: Box::new(E::Unop(op, ret))};
                } else { return Ok(ret); }
            }
        }
        Rule::expr8 => { // Function application & indexing
            let mut inner_rules = expr.into_inner();
            let mut ret = parse_expr_pair(inner_rules.next().unwrap())?;

            loop {
                if let Some(paren_operator) = inner_rules.next() {
                    let end = paren_operator.as_span().end_pos().line_col();
                    let mut paren_inner_rules = paren_operator.into_inner();
                    let inside = paren_inner_rules.next().unwrap();
                    let inner_expr = match inside.as_rule() {
                        Rule::arg_list => E::FunApp(ret, parse_arg_list(inside)?),
                        Rule::kwarg_list => E::FunKwApp(ret, parse_kwarg_list(inside)?),
                        Rule::slice => {
                            let slice1 = parse_slice(inside)?;
                            if let Some(inside) = paren_inner_rules.next() {
                                let slice2 = parse_slice(inside)?;
                                E::MatrixIdx(ret, slice1, slice2)
                            } else {
                                E::ListIdx(ret, slice1)
                            }
                        }
                        _ => unreachable!()
                    };
                    ret = ExprA{start, end, expr: Box::new(inner_expr)};
                } else { return Ok(ret); }
            }
        }
        Rule::expr9 |
        Rule ::expr_base => return parse_expr_pair(expr.into_inner().next().unwrap()),
        Rule::matrix => {
            let mut inner_rules = expr.into_inner();
            let mut mat_data = inner_rules.next().unwrap().into_inner().map(parse_expr_pair).collect::<ParserResult<Vec<ExprA>>>()?;
            let mut numrows = 1;
            let numcols = mat_data.len();
            loop {
                if let Some(row) = inner_rules.next() {
                    numrows += 1;
                    let mut row = row.into_inner().map(parse_expr_pair).collect::<ParserResult<Vec<ExprA>>>()?;
                    if row.len() != numcols { return Err(ParserError::NonRectangularMatrix); }
                    mat_data.append(&mut row);
                } else { break; }
            }
            E::Matrix(numrows, numcols, mat_data)
        }
        Rule::list => E::List(expr.into_inner().map(parse_expr_pair).collect::<ParserResult<Vec<ExprA>>>()?),
        Rule::lam_expr => {
            let mut inner_rules = expr.into_inner();
            let type_param_list = parse_type_param_list(inner_rules.next().unwrap())?;
            let (params, paramtypes) = parse_param_list(inner_rules.next().unwrap(), &type_param_list)?;
            let ret_type = match inner_rules.next().unwrap().into_inner().next() {
                Some(t) => parse_type_pair(t, &type_param_list)?,
                None => Type::Any,
            };
            let inner_expr = parse_expr_pair(inner_rules.next().unwrap())?;
            E::Lambda(params, inner_expr, type_param_list, paramtypes, ret_type)
        }
        Rule::lam_basic => {
            let mut inner_rules = expr.into_inner();
            let id = inner_rules.next().unwrap().as_str().to_string();
            let body = parse_expr_pair(inner_rules.next().unwrap())?;
            E::Lambda(vec![id], body, Vec::new(), vec![Type::Any], Type::Any)
        }
        Rule::tuple => E::Tuple(expr.into_inner().map(|x| parse_expr_pair(x)).collect::<ParserResult<Vec<ExprA>>>()?),
        Rule::bool => E::Bool(expr.as_str().eq("true")),
        Rule::string => {
            let s = expr.as_str();
            let s = handle_escape_sequences(&s[1..s.len()-1]);
            E::Str(s)
        }
        Rule::ID |
        Rule::OPID => E::Id(expr.as_str().to_string()),
        Rule::FLOATIMAG => {
            let value: f64 = expr.as_str().trim_end_matches("i").parse()
                .expect("Cannot parse float imaginary");
            E::FloatImag(value)
        }
        Rule::INTIMAG => {
            let value: i32 = expr.as_str().trim_end_matches("i").parse()
                .expect("Cannot parse int imaginary");
            E::IntImag(value)
        }
        Rule::FLOAT => {
            let value: f64 = expr.as_str().parse()
                .expect("Cannot parse number");
            E::Num(value)
        }
        Rule::INT => {
            let value: i32 = expr.as_str().parse()
                .expect("Cannot parse number");
            E::Int(value)
        }
        _ => unreachable!()
    };
    Ok(ExprA{start, end, expr: Box::new(expr)})
}

fn parse_type_param_list(pair: Pair<Rule>) -> ParserResult<Vec<String>> {
    let mut ids: Vec<String> = Vec::new();
    for type_param in pair.into_inner() {
        let s = type_param.as_str().to_string();
        if ids.contains(&s) { return Err(ParserError::RepeatedTypeParam); }
        ids.push(s);
    }
    Ok(ids)
}

fn parse_param_list(param_list: Pair<Rule>, type_params: &Vec<String>) -> ParserResult<(Vec<String>, Vec<Type>)> {
    let mut ids: Vec<String> = Vec::new();
    let mut types: Vec<Type> = Vec::new();
    for param in param_list.into_inner() {
        let mut inner_rules = param.into_inner();
        let name = inner_rules.next().unwrap().as_str().to_string();
        if ids.contains(&name) { return Err(ParserError::RepeatedParam); }
        let typ = match inner_rules.next() {
            Some(t) => parse_type_pair(t, type_params)?,
            None => Type::Any
        };
        ids.push(name);
        types.push(typ);
    }
    Ok((ids, types))
}

fn parse_arg_list(arg_list: Pair<Rule>) -> ParserResult<Vec<ExprA>> {
    arg_list.into_inner().map(parse_expr_pair).collect()
}

fn parse_kwarg_list(kwarg_list: Pair<Rule>) -> ParserResult<HashMap<String, ExprA>> {
    let mut kwarg_map: HashMap<String, ExprA> = HashMap::new();
    for arg in kwarg_list.into_inner() {
        let mut inner_rules = arg.into_inner();
        let id = inner_rules.next().unwrap().as_str().to_string();
        let expr = parse_expr_pair(inner_rules.next().unwrap())?;
        kwarg_map.insert(id, expr);
    }
    Ok(kwarg_map)
}

fn parse_slice(slice: Pair<Rule>) -> ParserResult<AstSlice> {
    let slice = slice.into_inner().next().unwrap();
    match slice.as_rule() {
        Rule::slice_ny => {
            let upper = parse_expr_pair(slice.into_inner().next().unwrap())?;
            Ok(AstSlice::Range(None, Some(upper)))
        }
        Rule::slice_nn => {
            Ok(AstSlice::Range(None, None))
        }
        Rule::slice_yy => {
            let mut inner_rules = slice.into_inner();
            let lower = parse_expr_pair(inner_rules.next().unwrap())?;
            let upper = parse_expr_pair(inner_rules.next().unwrap())?;
            Ok(AstSlice::Range(Some(lower), Some(upper)))
        }
        Rule::slice_yn => {
            let lower = parse_expr_pair(slice.into_inner().next().unwrap())?;
            Ok(AstSlice::Range(Some(lower), None))
        }
        Rule::expr => {
            Ok(AstSlice::Single(parse_expr_pair(slice)?))
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

fn parse_type_pair(pair: Pair<Rule>, type_params: &Vec<String>) -> ParserResult<Type> {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::function_type => {
            let mut inner_rules = pair.into_inner();
            let mut params: Vec<Type> = Vec::new();
            loop {
                if let Some(t) = inner_rules.next() {
                    params.push(parse_type_pair(t, type_params)?);
                } else { break; }
            }
            let rettype = params.pop().unwrap();
            Ok(Type::Func(params, Box::new(rettype)))
        }
        Rule::tuple_type => {
            let mut inner_rules = pair.into_inner();
            let mut params: Vec<Type> = Vec::new();
            loop {
                if let Some(t) = inner_rules.next() {
                    params.push(parse_type_pair(t, type_params)?);
                } else { break; }
            }
            Ok(Type::Tuple(params))
        }
        Rule::compound_type => {
            let mut inner_rules = pair.into_inner();
            let constructor = inner_rules.next().unwrap();
            let mut params: Vec<Type> = Vec::new();
            loop {
                if let Some(t) = inner_rules.next() {
                    params.push(parse_type_pair(t, type_params)?);
                } else { break; }
            }
            match (constructor.as_str(), params.len()) {
                ("List", 1) => Ok(Type::list(params.pop().unwrap())),
                _ => Err(ParserError::InvalidTypeConstructor(constructor.as_str().to_string()))
            }
        }
        Rule::atomic_type => match pair.as_str() {
            "Num" => Ok(Type::Num),
            "Bool" => Ok(Type::Bool),
            "Str" => Ok(Type::Str),
            "Mat" => Ok(Type::Mat),
            t => if type_params.contains(&t.to_string()) { Ok(Type::var(t)) }
                 else { Err(ParserError::InvalidAtomicType(t.to_string())) }
        }
        _ => unreachable!()
    }
}