use std::{ops::Range, collections::HashMap};

use crate::span::Span;

use super::error::Error;

pub struct Repoter {
    lineses: HashMap<String, String>
}

impl Repoter {
    pub fn new() -> Self {
        Self { lineses: HashMap::new() }
    }
    
    fn print_multiple(s: &str, range: Range<usize>) {
        for _ in range {
            eprint!("{s}");
        }
    }

    // TODO: Refactor 
    pub fn report(&mut self, err: Error, stage: &str) {
        let Span { source_name, first_line, last_line, start, end } = err.span;

        let lines = if self.lineses.contains_key(&source_name) {
            self.lineses.get(&source_name).unwrap().lines()
        } else {
            let file = std::fs::read_to_string(&source_name).expect("Error reading a file.");
            self.lineses.insert(source_name.clone(), file);
            self.lineses.get(&source_name).unwrap().lines()
        };

        eprintln!("\n  Error | [{source_name}:{first_line}:{start}] (at {stage})"); 
        eprintln!("        |");
        
        let fline = lines.clone().nth(first_line - 1).unwrap();
        eprintln!("   {first_line:>4} | {fline}"); 
        eprint!  ("        | ");
        
        Self::print_multiple(" ", 1..start);
        
        if last_line == first_line {
            Self::print_multiple("^", start..end);
            eprintln!();
            eprintln!("        | {msg}\n", msg = err.msg); 
            match *err.origin {
                Some(err) => {
                    eprintln!("      ! | Originates from");
                    self.report(err, stage)
                },
                None => (),
            }
    
            return
        }
        
        // First Line
        Self::print_multiple("^", start..fline.len()+1);
        eprintln!();

        // Middle Lines if there is
        for line_no2 in first_line+1..last_line {
            let line = lines.clone().nth(line_no2 - 1).unwrap();
            eprintln!("   {line_no2:>4} | {line}");
            eprint!  ("        | ");
            Self::print_multiple("^", 1..line.len()+1);
            eprintln!();
        }
        
        // Last Line
        let lline = lines.clone().nth(last_line - 1).unwrap();
        eprintln!("   {last_line:>4} | {lline}"); 
        eprint!  ("        | ");
        Self::print_multiple("^", 1..end);
        eprintln!();
        eprintln!("        | {msg}\n", msg = err.msg);

        match *err.origin {
            Some(err) => {
                eprintln!("      ! | Originates from");
                self.report(err, stage)
            },
            None => (),
        }
    }
}