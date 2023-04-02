use std::ops::Range;

use crate::span::Span;

use super::error::Error;

pub struct Repoter<'a> 
{
    source_name: &'a str,
    lines: std::str::Lines<'a>
}

impl<'a> Repoter<'a> 
{
    pub fn new(source_name: &'a str, source: &'a str) -> Self {
        Self { source_name, lines: source.lines() }
    }
    
    fn print_multiple(s: &str, range: Range<usize>) {
        for _ in range {
            eprint!("{s}");
        }
    }

    // TODO: Refactor 
    pub fn report(&mut self, err: Error, stage: &str) {
        let Span { first_line, last_line, start, end } = err.span;

        eprintln!("\n  Error | [{source_name}:{first_line}:{start}] (at {stage})", source_name = self.source_name); 
        eprintln!("        |");
        
        let fline = self.lines.clone().nth(first_line - 1).unwrap();
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
            let line = self.lines.clone().nth(line_no2 - 1).unwrap();
            eprintln!("   {line_no2:>4} | {line}");
            eprint!  ("        | ");
            Self::print_multiple("^", 1..line.len()+1);
            eprintln!();
        }
        
        // Last Line
        let lline = self.lines.clone().nth(last_line - 1).unwrap();
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