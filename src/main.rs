mod parser;
mod exec;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::io::{stdin};
use pest::{error::Error, iterators::Pair};
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct SciLangParser;

fn main() {
    let mut environ = exec::Environ::new();
    let mut userinput = String::new();

    loop {
        userinput.clear();
        stdin().read_line(&mut userinput).unwrap();
        let stmt: Pair<Rule> = SciLangParser::parse(Rule::stmt, &mut userinput)
            .expect("Unsuccessful parse")
            .next().unwrap();
        //println!("DEBUG: STMT: {:?}", stmt);
        let ast = parser::get_ast_stmt(stmt);
        //println!("DEBUG: AST: {:?}", ast);
        environ.execute(ast);
    }
}
