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
mod callable;
mod types;

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate rustyline;

use pest::iterators::Pair;
use pest::Parser;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::env;
use std::fs;
use std::io::Read;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct SciLangParser;

fn main() {

    // command line arguments
    let command_line_args: Vec<String> = env::args().collect();
    if command_line_args.len() == 2 {
        interpret_file(&command_line_args[1]);
        return;
    }

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

fn interpret_file(srcfile: &str) {
    let mut environ = exec::Environ::new();
    let mut srcfile = fs::File::open(srcfile)
        .expect("Cannot open requested file");
    let mut contents = String::new();
    srcfile.read_to_string(&mut contents).unwrap();
    let mut prog = SciLangParser::parse(Rule::prog, &mut contents)
        .expect("unsuccessful parse");

    let mut inner_rules = prog.next().unwrap().into_inner();
    loop {
        if let Some(stmt) = inner_rules.next() {
            let ast = parser::get_ast_stmt(stmt);
            //println!("DEBUG: AST: {:?}", ast);
            environ.execute(ast);
        } else { break; }
    }
}
