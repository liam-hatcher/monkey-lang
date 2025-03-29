mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

use std::{env, fs::File};
use std::io::{self, Read};

use evaluator::eval;
use lexer::Lexer;
use object::environment::Environment;
use parser::Parser;
use repl::start_repl;

fn run_file(file_contents: String) {
    let mut l = Lexer::new(file_contents);
    let mut p = Parser::new(&mut l);
    let program = p.parse_program();

    let env = Environment::new();

    eval(program, env);
}

fn main() -> io::Result<()>  {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        start_repl();
    }

    let file_name = &args[1];

    println!("File: {:?}", file_name);

    let mut file = match File::open(file_name) {
        Ok(file) => file,
        Err(error) => panic!("Failed to open file {:?}", error),
    };

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    
    run_file(contents);

    Ok(())
}
