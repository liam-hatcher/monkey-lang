mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

use repl::start_repl;

fn main() {
    

    start_repl();
}
