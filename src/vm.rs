use std::{ptr};

use crate::{chunk::{Chunk, OpCode}, debug::disassemble_instruction, value::{print_value, Value}};

pub const STACK_MAX: usize = 256;

pub struct VM {
    pub chunk: *mut Chunk,
    pub ip: *mut u8,
    pub stack: [Value; STACK_MAX],
    pub stack_top: *mut Value,
}

pub enum InterpretResult {
    Ok,
    _CompileError,
    RuntimeError,
}

#[allow(non_upper_case_globals)]
pub static mut vm: VM = VM {
    chunk: ptr::null_mut(),
    ip: ptr::null_mut(),
    stack: [0.0; STACK_MAX],
    stack_top: ptr::null_mut(),
};

unsafe fn reset_stack() {
    vm.stack_top = vm.stack.as_mut_ptr();
}

pub unsafe fn init_vm() {
    reset_stack();
}

pub unsafe fn free_vm() {}

pub unsafe fn interpret(chunk: *mut Chunk) -> InterpretResult {
    vm.chunk = chunk;
    vm.ip = (*vm.chunk).code;
    run()
}

pub unsafe fn push(value: Value) {
    *vm.stack_top = value;
    vm.stack_top = vm.stack_top.add(1);
}

pub unsafe fn pop() -> Value {
    vm.stack_top = vm.stack_top.sub(1);
    *vm.stack_top
}

unsafe fn run() -> InterpretResult {
    macro_rules! read_byte {
        () => {{
            let v = *vm.ip;
            vm.ip = vm.ip.add(1);
            v
        }};
    }
    macro_rules! read_constant {
        () => {
            *(*vm.chunk).constants.values.add(read_byte!() as usize)
        };
    }
    macro_rules! binary_op {
        ($op:tt) => {{
            let b = pop();
            let a = pop();
            push(a $op b);
        }}
    }

    loop {
        #[cfg(feature = "debug_trace_execution")]
        {
            print!("          ");
            let mut slot = vm.stack.as_ptr();
            while slot < vm.stack_top {
                print!("[ ");
                print_value(*slot);
                print!(" ]");
                slot = slot.add(1);
            }
            println!("");
            disassemble_instruction(vm.chunk, vm.ip.sub((*vm.chunk).code as usize) as i32);
        }

        let instruction = read_byte!();
        match instruction {
            i if i == OpCode::Constant as u8 => {
                let constant = read_constant!();
                push(constant);
            }
            i if i == OpCode::Negate as u8 => push(-pop()),
            i if i == OpCode::Add as u8 => binary_op!(+),
            i if i == OpCode::Subtract as u8 => binary_op!(-),
            i if i == OpCode::Multiply as u8 => binary_op!(*),
            i if i == OpCode::Divide as u8 => binary_op!(/),
            i if i == OpCode::Return as u8 => {
                print_value(pop());
                println!("");
                return InterpretResult::Ok;
            }
            _ => break,
        }
    }

    InterpretResult::RuntimeError
}
