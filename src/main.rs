mod parser;
mod exec;
mod ast;
mod internals;
mod print;
mod replhelper;
mod error;
mod number;
mod matrix;
mod value;
mod macros;

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate rustyline;

use pest::iterators::Pair;
use pest::Parser;
use rustyline::error::ReadlineError;
use rustyline::Editor;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct SciLangParser;

fn main() {
    let mut environ = exec::Environ::new();

    // Setup rustyline
    let mut rl = Editor::<replhelper::MyHelper>::new();
    rl.set_helper(Some(replhelper::MyHelper::new()));
    //if rl.load_history("history.txt").is_err() {
    //    println!("No previous history.");
    //}

    // REPL
    loop {
        match rl.readline(">> ") {
            Ok(mut line) => {
                rl.add_history_entry(line.as_str());

                let stmt: Pair<Rule> = SciLangParser::parse(Rule::stmt, &mut line)
                    .expect("Unsuccessful parse")
                    .next().unwrap();
                //println!("DEBUG: STMT: {:?}", stmt);
                let ast = parser::get_ast_stmt(stmt);
                //println!("DEBUG: AST: {:?}", ast);
                environ.execute(ast);
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
    //rl.save_history("history.txt").unwrap();
}
