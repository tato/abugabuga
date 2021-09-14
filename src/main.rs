use memory::gc_track_vm;
use std::{env, fs, process};
use vm::{InterpretResult, VM};

pub const UINT8_COUNT: usize = u8::MAX as usize + 1;

#[macro_use]
mod memory;
mod array;
mod chunk;
mod compiler;
#[cfg(any(feature = "debug_trace_execution", feature = "debug_print_code"))]
mod debug;
mod object;
mod scanner;
mod value;
mod vm;

#[cfg(test)]
mod test;

unsafe fn repl(vm: &mut VM) {
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
        vm.interpret(&line);
    }
}

unsafe fn run_file(vm: &mut VM, path: &str) {
    let source = fs::read_to_string(path).expect(&format!("Could not read file \"{}\"", path));
    let result = vm.interpret(&source);

    if result == InterpretResult::CompileError {
        process::exit(65);
    }
    if result == InterpretResult::RuntimeError {
        process::exit(70);
    }
}

fn main() {
    unsafe {
        let mut vm = VM::new();
        gc_track_vm(&mut vm);
        vm.init();

        let args = env::args().collect::<Vec<_>>();
        if args.len() == 1 {
            repl(&mut vm);
        } else if args.len() == 2 {
            run_file(&mut vm, &args[1]);
        } else {
            eprintln!("Usage: abugabuga [path]");
            process::exit(64);
        }
    }
}
