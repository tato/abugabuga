use std::{mem, ptr};

use crate::{
    chunk::{free_chunk, init_chunk, Chunk, OpCode},
    compiler::compile,
    memory::free_objects,
    object::{as_string, is_string, take_string, Obj},
    table::{free_table, init_table, table_delete, table_get, table_set, Table},
    value::{
        as_bool, as_number, bool_val, is_bool, is_nil, is_number, nil_val, number_val, obj_val,
        print_value, values_equal, Value,
    },
};

#[cfg(feature = "debug_trace_execution")]
use crate::debug::disassemble_instruction;

pub const STACK_MAX: usize = 256;

pub struct VM {
    pub chunk: *mut Chunk,
    pub ip: *mut u8,
    pub stack: [Value; STACK_MAX],
    pub stack_top: *mut Value,
    pub globals: Table,
    pub strings: Table,
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
    globals: Table {
        count: 0,
        capacity: 0,
        entries: ptr::null_mut(),
    },
    strings: Table {
        count: 0,
        capacity: 0,
        entries: ptr::null_mut(),
    },
    objects: ptr::null_mut(),
};

unsafe fn reset_stack() {
    vm.stack_top = vm.stack.as_mut_ptr();
}

macro_rules! runtime_error {
    ($($args:expr),*) => {
        eprintln!($($args),*);
        let instruction = vm.ip.sub((*vm.chunk).code.sub(1) as usize);
        let line = *(*vm.chunk).lines.offset(instruction as isize);
        eprintln!("[line {}] in script", line);
    };
}

pub unsafe fn init_vm() {
    reset_stack();
    init_table(&mut vm.globals);
    init_table(&mut vm.strings);
}

pub unsafe fn free_vm() {
    free_table(&mut vm.globals);
    free_table(&mut vm.strings);
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
    macro_rules! read_string {
        () => {
            as_string(read_constant!())
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
            i if i == OpCode::Pop as u8 => {
                pop();
            }
            i if i == OpCode::GetLocal as u8 => {
                let slot = read_byte!();
                push(vm.stack[slot as usize]);
            }
            i if i == OpCode::SetLocal as u8 => {
                let slot = read_byte!();
                vm.stack[slot as usize] = peek(0);
            }
            i if i == OpCode::GetGlobal as u8 => {
                let name = read_string!();
                let mut value = nil_val(); // @todo uninitialized
                if !table_get(&mut vm.globals, name, &mut value) {
                    runtime_error!(
                        "Undefined variable '{}'.",
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                            (*name).chars,
                            (*name).length as usize
                        ))
                    );
                    return InterpretResult::RuntimeError;
                }
                push(value);
            }
            i if i == OpCode::DefineGlobal as u8 => {
                let name = read_string!();
                table_set(&mut vm.globals, name, peek(0));
                pop();
            }
            i if i == OpCode::SetGlobal as u8 => {
                let name = read_string!();
                if table_set(&mut vm.globals, name, peek(0)) {
                    // true means key wasn't in table
                    table_delete(&mut vm.globals, name);
                    runtime_error!(
                        "Undefined variable '{}'.",
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                            (*name).chars,
                            (*name).length as usize
                        ))
                    );
                    return InterpretResult::RuntimeError;
                }
            }
            i if i == OpCode::Equal as u8 => {
                let b = pop();
                let a = pop();
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
            }
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
            }
            i if i == OpCode::Subtract as u8 => binary_op!(number_val, -),
            i if i == OpCode::Multiply as u8 => binary_op!(number_val, *),
            i if i == OpCode::Divide as u8 => binary_op!(number_val, /),
            i if i == OpCode::Print as u8 => {
                print_value(pop());
                println!("");
            }
            i if i == OpCode::Return as u8 => {
                return InterpretResult::Ok;
            }
            _ => break,
        }
    }

    InterpretResult::RuntimeError
}
