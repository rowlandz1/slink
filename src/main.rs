mod ast;
mod builtins;
mod callable;
mod display;
mod error;
mod exec;
mod matrix;
mod number;
mod parser;
mod replhelper;
mod types;
mod value;

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::{env, fs};

fn main() {
    let command_line_args: Vec<String> = env::args().collect();
    if command_line_args.len() == 2 {
        interpret_file(&command_line_args[1]);
    } else {
        repl();
    }
}

/// Read-evaluate-print-loop
fn repl() {
    let mut environ = exec::Environ::new();
    let mut typenv = types::TypeEnv::new();

    // Setup rustyline
    let mut rl = Editor::<replhelper::MyHelper>::new();
    rl.set_helper(Some(replhelper::MyHelper::new()));
    if rl.load_history("history.txt").is_err() {};

    // REPL
    loop {
        match rl.readline(">> ") {
            Ok(src) => {
                rl.add_history_entry(&src);

                let ast = match parser::parse_stmt(&src) {
                    Ok(ast) => ast,
                    Err(err) => { eprintln!("{:?}", err); continue; }
                };

                let types = match typenv.typecheck_stmt(&ast) {
                    Ok(types) => types,
                    Err(err) => { eprintln!("{}", err.supply_src(&src)); continue; }
                };

                let output = match environ.execute(ast) {
                    Ok(output) => output,
                    Err(err) => { eprintln!("{}", err); continue; }
                };

                if let Some(output) = output {
                    if types.len() == 1 {
                        println!("{}: {}", output, types[0]);
                    } else {
                        println!("{}:\n    {}", output, types.join("\n    "));
                    }
                }
            },
            Err(ReadlineError::Interrupted) |
            Err(ReadlineError::Eof) => break,
            Err(err) => { eprintln!("Readline error: {:?}", err); break; }
        }
    }
    rl.save_history("history.txt").unwrap();
}

fn interpret_file(srcfile: &str) {
    let contents = String::from_utf8_lossy(&fs::read(srcfile).expect("Couldn't read file")).to_string();
    let program = match parser::parse_program(&contents) {
        Ok(stmts) => stmts,
        Err(err) => { eprintln!("{}", err); return; }
    };

    let mut typenv = types::TypeEnv::new();
    for stmt in &program {
        match typenv.typecheck_stmt(stmt) {
            Ok(_) => {}
            Err(err) => { eprintln!("{}", err.supply_src(&contents)); return; }
        }
    }

    let mut environ = exec::Environ::new();
    for stmt in program {
        match environ.execute(stmt) {
            Ok(Some(output)) => println!("{}", output),
            Ok(None) => {}
            Err(err) => { println!("{}", err); return; }
        }
    }
}