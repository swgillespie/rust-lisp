#![feature(if_let)]
#![feature(while_let)]
#![feature(globs)]
#![feature(box_syntax, box_patterns)]

// This feature is enabled to use `bench` feature, requires nigthly rust
#![feature(test)]

use std::io;
use std::ops::Deref;

use std::io::Write;

use reader::SexpReader;
use interpreter::Interpreter;

mod reader;
mod interpreter;
mod intrinsics;
mod environment;

#[allow(dead_code)]
fn main() {
    println!("Rust Lisp interpreter, by Sean Gillespie 2014");
    println!("Licensed under the MIT license");
    println!("Control+D to exit");
    let mut stdin = io::stdin();
    let mut interpreter = Interpreter::new();
    loop {
        print!("lisp> ");
        io::stdout().flush().ok().expect("Could not flush stdout"); // to print prompt before user input
        let mut input_line = String::new();
        if let Ok(n) = stdin.read_line(&mut input_line) {
            process_line(input_line, &mut interpreter);
        } else {
            println!("\nBye!");
            break;
        }
    }
}


fn process_line(input: String, interpreter: &mut Interpreter) {
    let mut reader = SexpReader::new();
    match reader.parse_str_all(input.as_str()) {
        Ok(e) => for (idx, sexp) in e.iter().enumerate() {
            match interpreter.eval(sexp)  {
                Ok(ref val) => match val.deref() {
                    v => println!("${} = {}", idx, v),
                },
                Err(e) => println!("error: {}", e)
            }
        },
        Err(e) => println!("error: {}", e)
    };
}






