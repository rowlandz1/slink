mod ast;
mod builtins;
mod callable;
mod error;
mod exec;
mod matrix;
mod number;
mod parser;
mod print;
mod replhelper;
mod types;
mod value;

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::env;

fn main() {

    // command line arguments
    let command_line_args: Vec<String> = env::args().collect();
    if command_line_args.len() == 2 {
        interpret_file(&command_line_args[1]);
        return;
    }

    let mut environ = exec::Environ::new();
    let mut typenv = types::TypeEnv::new();

    // Setup rustyline
    let mut rl = Editor::<replhelper::MyHelper>::new();
    rl.set_helper(Some(replhelper::MyHelper::new()));
    if rl.load_history("history.txt").is_err() {};

    // REPL
    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                let ast = match parser::parse_stmt(&line) {
                    Ok(ast) => ast,
                    Err(err) => { eprintln!("{:?}", err); continue; }
                };

                match *ast.stmt {
                    ast::Stmt::Assign(_, _) => {
                        match typenv.type_check_stmt(&ast) {
                            Ok(_) => {}
                            Err(err) => eprintln!("{}", err.to_string()),
                        }
                        match environ.execute(ast) {
                            Ok(_) => {}
                            Err(err) => eprintln!("{}", err.to_string()),
                        }
                    }
                    ast::Stmt::Display(_) => {
                        let typ = match typenv.type_check_stmt(&ast) {
                            Ok(typ) => typ,
                            Err(err) => { eprintln!("{}", err.to_string()); continue; }
                        };
                        match environ.execute(ast) {
                            Ok(output) => println!("{}: {}", output, typ),
                        //    Ok(output) => println!("{}", output),
                            Err(err) => eprintln!("{}", err.to_string())
                        }
                    }
                }
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
    rl.save_history("history.txt").unwrap();
}

fn interpret_file(_srcfile: &str) {
    todo!()
    // let mut environ = exec::Environ::new();
    // let mut srcfile = fs::File::open(srcfile)
    //     .expect("Cannot open requested file");
    // let mut contents = String::new();
    // srcfile.read_to_string(&mut contents).unwrap();
    // let mut prog = SciLangParser::parse(Rule::prog, &mut contents)
    //     .expect("unsuccessful parse");

    // let mut inner_rules = prog.next().unwrap().into_inner();
    // loop {
    //     if let Some(stmt) = inner_rules.next() {
    //         let ast = parser::parse_stmt(stmt);
    //         //println!("DEBUG: AST: {:?}", ast);
    //         match environ.execute(ast) {
    //             Ok(output) => println!("{}", output),
    //             Err(err) => { eprintln!("{}", err); break; }
    //         }
    //     } else { break; }
    // }
}