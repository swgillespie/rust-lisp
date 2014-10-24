#![feature(if_let)]
#![feature(while_let)]
#![feature(globs)]

use std::io::stdio;
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
    let mut stdin = stdio::stdin();
    let mut interpreter = Interpreter::new();
    loop {
        print!("lisp> ");
        if let Ok(line) = stdin.read_line() {
            process_line(line, &mut interpreter);
        } else {
            println!("\nBye!");
            break;
        }
    }
}


fn process_line(input: String, interpreter: &mut Interpreter) {
    let mut reader = SexpReader::new();
    match reader.parse_str_all(input.as_slice()) {
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






