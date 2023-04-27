mod lex;
mod ast;
mod eval;
mod error;
mod span;
mod pipeline;

fn main() {
    let args: Vec<String> = std::env::args().collect(); 

    match args.len() {
        1 => {
            todo!("REPL");
        }

        2 => {
            let _ = pipeline::run_from_file(&args[1]);
        } 

        3.. => {
            todo!("Options")
        }
        _ => unreachable!()
    }
}
