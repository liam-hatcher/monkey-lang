mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

use lexer::Lexer;
use parser::Parser;
use repl::start_repl;

fn main() {
    let input = "3 + 4 * 5 == 3 * 1 + 4 * 5";
    let mut l = Lexer::new(input.into());
    let mut p = Parser::new(&mut l);

    let prog = p.parse_program();

    start_repl();
}
