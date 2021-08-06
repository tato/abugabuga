use std::{env, fs, process};
use vm::{free_vm, init_vm, interpret, InterpretResult};

pub const UINT8_COUNT: usize = u8::MAX as usize + 1;

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

#[cfg(test)]
mod test;

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

        free_vm();
    }
}
