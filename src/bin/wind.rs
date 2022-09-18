use std::{
    error::Error,
    fs::{self},
    process,
};

use clap::{Parser, Subcommand};
use mimalloc::MiMalloc;
use windlang_rust::{compiler::Compiler, lexer::Lexer, parser, vm::VM};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Parser)]
#[clap(author="joetifa2003", version="0.1.0", about, long_about = None)]
#[clap(propagate_version = true)]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Runs a wind script using the VM
    Run { file_name: String },
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { file_name } => {
            let code = value_or_exit(fs::read_to_string(file_name));

            let mut lexer = Lexer::new(code);
            let tokens = value_or_exit(lexer.lex());

            let mut parser = parser::Parser::new(tokens);
            let ast = value_or_exit(parser.parse_program());

            let mut compiler = Compiler::new();
            let opcode = value_or_exit(compiler.compile_stmts(ast));
            for op in &opcode {
                println!("{}", op);
            }
            println!("========================");

            let mut vm = VM::new(opcode);
            value_or_exit(vm.interpret());
        }
    }

    Ok(())
}

fn value_or_exit<T, E: Error>(input: Result<T, E>) -> T {
    match input {
        Ok(val) => val,
        Err(err) => {
            eprintln!("{}", err);
            process::exit(1)
        }
    }
}
