mod lex;
mod ast;
mod eval;
mod error;
mod span;

use std::{
    io, fs 
};

use lex::lexer::Lexer;
use ast::parser::Parser;
use eval::evaluator::Engine;
use error::reporter::Repoter;

fn run_from_file(path: &String) -> io::Result<()> {
    let file = fs::read_to_string(path).expect("Error reading a file.");
    let mut reporter = Repoter::new(path, &file);
    
    let tokens = match Lexer::new(&file).collect() {
        Ok(tokens) => tokens,
        Err(err)   => {
            reporter.report(err, "tokenizing");
            return Ok(())
        },
    };

    let stmts: Vec<_> = match Parser::new(tokens).collect() {
        Ok(stmts) => stmts,
        Err(err)  => {
            reporter.report(err, "parsing");
            return Ok(())
        },
    };

    let mut engine = Engine::new();
    match engine.collect_definitions(&stmts) {
        Ok(_)    => (),
        Err(err) => {
            reporter.report(err, "runtime");
            return Ok(());
        }
    }
    for stmt in &stmts {
        match engine.run(&stmt) {
            Ok(_)    => (),
            Err(err) => {
                reporter.report(err, "runtime");
                return Ok(())
            }
        }
    };

    Ok(())
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect(); 
    if args.len() > 1 {
        return run_from_file(&args[1]);
    }
    eprintln!("Usage: ./testi <file>");
    Ok(())
}
