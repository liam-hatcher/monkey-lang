use repl::start_repl;

mod token;
mod lexer;
mod repl;
mod ast;
mod parser;


fn main() {
    // println!("Monkey REPL");
    start_repl();
}
