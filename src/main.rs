#![feature(if_let)]
#![feature(globs)]

use std::io::stdio;
use reader::SexpReader;

mod reader;

#[allow(dead_code)]
fn main() {
    let mut stdin = stdio::stdin();
    loop {
        print!("scheme> ");
        if let Ok(line) = stdin.read_line() {
            process_line(line);
        } else {
            println!("\nUh oh - bye!");
            break;
        }
    }
}

fn process_line(input: String) {
    let mut reader = SexpReader::new();
    let result = reader.parse_str(input.as_slice());
    match result {
        Ok(e) => println!("==> {}", e),
        Err(e) => println!("error: {}", e)
    };
}







