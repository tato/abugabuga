use std::{env, fs, process};
use vm::{free_vm, init_vm, interpret, InterpretResult};

#[macro_use]
mod memory;
mod chunk;
mod compiler;
#[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
mod debug;
mod object;
mod scanner;
mod table;
mod value;
mod vm;

unsafe fn repl() {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();
    use std::io::{BufRead, Write};
    loop {
        print!("> ");
        stdout.flush().unwrap();
        let mut line = String::new();
        let mut reader = stdin.lock();
        if reader.read_line(&mut line).unwrap() == 0 {
            println!();
            break;
        }
        interpret(&line);
    }
}

unsafe fn run_file(path: &str) {
    let source = fs::read_to_string(path).expect(&format!("Could not read file \"{}\"", path));
    let result = interpret(&source);

    if result == InterpretResult::CompileError {
        process::exit(65);
    }
    if result == InterpretResult::RuntimeError {
        process::exit(70);
    }
}

fn main() {
    unsafe {
        init_vm();

        let args = env::args().collect::<Vec<_>>();
        if args.len() == 1 {
            repl();
        } else if args.len() == 2 {
            run_file(&args[1]);
        } else {
            eprintln!("Usage: abugabuga [path]");
            process::exit(64);
        }

        // let mut chunk: Chunk = mem::zeroed();
        // init_chunk(&mut chunk);

        // let constant = add_constant(&mut chunk, 1.2);
        // write_chunk(&mut chunk, OpCode::Constant as u8, 123);
        // write_chunk(&mut chunk, constant as u8, 123);
        // let constant = add_constant(&mut chunk, 3.4);
        // write_chunk(&mut chunk, OpCode::Constant as u8, 123);
        // write_chunk(&mut chunk, constant as u8, 123);

        // write_chunk(&mut chunk, OpCode::Add as u8, 123);

        // let constant = add_constant(&mut chunk, 5.6);
        // write_chunk(&mut chunk, OpCode::Constant as u8, 123);
        // write_chunk(&mut chunk, constant as u8, 123);

        // write_chunk(&mut chunk, OpCode::Divide as u8, 123);
        // write_chunk(&mut chunk, OpCode::Negate as u8, 123);

        // write_chunk(&mut chunk, OpCode::Return as u8, 123);
        // disassemble_chunk(&mut chunk, "test chunk");

        // interpret(&mut chunk);

        free_vm();
        // free_chunk(&mut chunk);
    }
}
