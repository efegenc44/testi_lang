use crate::{
    error::{ reporter::Repoter, error::Res }, 
    lex::lexer::Lexer, 
    ast::parser::Parser, 
    eval::evaluator::Engine
};


pub fn run_from_file(path: &str) -> Result<(), ()>{
    let mut reporter = Repoter::new();
    let lexer = Lexer::from_file(path)
        .map_err(|err| Repoter::report_file_error(err, path))?;
    let tokens = lexer.collect::<Res<_>>()
        .map_err(|err| reporter.report(err, "tokenizing"))?;
    let stmts = Parser::new(tokens).collect::<Res<_>>()
        .map_err(|err| reporter.report(err, "parsing"))?;
    let _ = Engine::new().run_module(&stmts)
        .map_err(|err| reporter.report(err, "runtime"))?;
    Ok(())
}