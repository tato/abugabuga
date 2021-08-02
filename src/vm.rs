use std::{mem, ptr};

use crate::{chunk::{free_chunk, init_chunk, Chunk, OpCode}, compiler::compile, debug::disassemble_instruction, memory::free_objects, object::{Obj, as_string, is_string, take_string}, value::{Value, as_bool, as_number, bool_val, is_bool, is_nil, is_number, nil_val, number_val, obj_val, print_value, values_equal}};

pub const STACK_MAX: usize = 256;

pub struct VM {
    pub chunk: *mut Chunk,
    pub ip: *mut u8,
    pub stack: [Value; STACK_MAX],
    pub stack_top: *mut Value,
    pub objects: *mut Obj,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

#[allow(non_upper_case_globals)]
pub static mut vm: VM = VM {
    chunk: ptr::null_mut(),
    ip: ptr::null_mut(),
    stack: [nil_val(); STACK_MAX],
    stack_top: ptr::null_mut(),
    objects: ptr::null_mut(),
};

unsafe fn reset_stack() {
    vm.stack_top = vm.stack.as_mut_ptr();
}

macro_rules! runtime_error {
    ($args:tt) => {
        eprintln!($args);
        let instruction = vm.ip.sub((*vm.chunk).code.sub(1) as usize);
        let line = *(*vm.chunk).lines.offset(instruction as isize);
        eprintln!("[line {}] in script", line);
    };
}

pub unsafe fn init_vm() {
    reset_stack();
}

pub unsafe fn free_vm() {
    free_objects();
}

pub unsafe fn interpret(source: &str) -> InterpretResult {
    let mut chunk = mem::zeroed();
    init_chunk(&mut chunk);

    if !compile(source, &mut chunk) {
        free_chunk(&mut chunk);
        return InterpretResult::CompileError;
    }

    vm.chunk = &mut chunk;
    vm.ip = (*vm.chunk).code;

    let result = run();

    free_chunk(&mut chunk);
    result
}

pub unsafe fn push(value: Value) {
    *vm.stack_top = value;
    vm.stack_top = vm.stack_top.add(1);
}

pub unsafe fn pop() -> Value {
    vm.stack_top = vm.stack_top.sub(1);
    *vm.stack_top
}

unsafe fn peek(distance: isize) -> Value {
    *vm.stack_top.offset(-1 - distance)
}

unsafe fn is_falsey(value: Value) -> bool {
    is_nil(value) || (is_bool(value) && !as_bool(value))
}

unsafe fn concatenate() {
    let b = &*as_string(pop());
    let a = &*as_string(pop());

    let length = a.length + b.length;
    let chars = allocate!(u8, length + 1);
    ptr::copy_nonoverlapping(a.chars, chars, a.length as usize);
    ptr::copy_nonoverlapping(b.chars, chars.offset(a.length as isize), b.length as usize);
    *chars.offset(length as isize) = 0;

    let result = take_string(chars, length);
    push(obj_val(mem::transmute(result)));
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
        ($value_type:ident, $op:tt) => {{
            if !is_number(peek(0)) || !is_number(peek(1)) {
                runtime_error!("Operands must be numbers.");
                return InterpretResult::RuntimeError;
            }
            let b = as_number(pop());
            let a = as_number(pop());
            push($value_type(a $op b));
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
            i if i == OpCode::Nil as u8 => push(nil_val()),
            i if i == OpCode::True as u8 => push(bool_val(true)),
            i if i == OpCode::False as u8 => push(bool_val(false)),
            i if i == OpCode::Equal as u8 => {
                let b = pop();
                let a  = pop();
                push(bool_val(values_equal(a, b)));
            }
            i if i == OpCode::Greater as u8 => binary_op!(bool_val, >),
            i if i == OpCode::Less as u8 => binary_op!(bool_val, <),
            i if i == OpCode::Not as u8 => push(bool_val(is_falsey(pop()))),
            i if i == OpCode::Negate as u8 => {
                if !is_number(peek(0)) {
                    runtime_error!("Operand must be a number.");
                    return InterpretResult::RuntimeError;
                }
                push(number_val(-as_number(pop())));
            },
            i if i == OpCode::Add as u8 => {
                if is_string(peek(0)) && is_string(peek(1)) {
                    concatenate();
                } else if is_number(peek(0)) && is_number(peek(1)) {
                    let b = as_number(pop());
                    let a = as_number(pop());
                    push(number_val(a + b));
                } else {
                    runtime_error!("Operands must be two numbers or two strings.");
                    return InterpretResult::RuntimeError;
                }
            },
            i if i == OpCode::Subtract as u8 => binary_op!(number_val, -),
            i if i == OpCode::Multiply as u8 => binary_op!(number_val, *),
            i if i == OpCode::Divide as u8 => binary_op!(number_val, /),
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
