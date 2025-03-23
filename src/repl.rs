use std::io::{self, Write};

use crate::{evaluator::eval, lexer::Lexer, object::environment::Environment, parser::Parser};

const MONKEY_FACE: &str = r#"            
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start_repl() {
    let environment = Environment::new();
    print!("{MONKEY_FACE}");
    println!("Welcome to the Monkey Programming Language, enter some commands: \n");
    loop {
        print!("monkey >>  ");

        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin().read_line(&mut input).unwrap();

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            println!("{MONKEY_FACE}");
            println!("Woops! We ran into some monkey business here!\n");
            println!(" parser errors:");
            for e in parser.errors.iter() {
                println!("\t {}", e);
            }
            continue;
        }

        let evaluated = eval(program, environment.clone());
        let output = evaluated.inspect();

        // environment.borrow().print_environment();

        if output != "null" {
            println!("{}", output);
        }
    }
}
