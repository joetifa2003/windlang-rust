use std::{error::Error, fs};

use windlang_rust::{
    compiler::Compiler,
    interpreter::{env::Env, Interpreter},
    lexer::Lexer,
    parser::Parser,
    vm::{opcode::Opcode, value::Value, VM},
};

macro_rules! value_or_exit {
    ($x:expr) => {
        match $x {
            Ok(value) => value,
            Err(err) => {
                println!("{}", err);
                ::std::process::exit(1);
            }
        }
    };
}

fn main() -> Result<(), Box<dyn Error>> {
    let code = fs::read_to_string("main.wind")?.parse()?;

    let mut lexer = Lexer::new(code);
    let tokens = value_or_exit!(lexer.lex());

    let mut parser = Parser::new(tokens);
    let ast = value_or_exit!(parser.parse_program());

    // let interpreter = Interpreter::new();
    // let env = Env::new();
    // let out = value_or_exit!(interpreter.eval_program(ast, env));

    // println!("{:?}", out);

    let mut compiler = Compiler::new();
    let opcode = compiler.compile(ast);
    dbg!(&opcode);

    let mut vm = VM::new();
    vm.interpret(opcode);
    dbg!(&vm.stack);

    Ok(())
}
