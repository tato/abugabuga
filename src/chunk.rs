use std::ptr;

use crate::{
    value::{free_value_array, init_value_array, write_value_array, Value, ValueArray},
    vm::{pop, push},
};

pub enum OpCode {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    SetLocal,
    GetGlobal,
    DefineGlobal,
    SetGlobal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    GetSuper,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Invoke,
    SuperInvoke,
    Closure,
    CloseUpvalue,
    Return,
    Class,
    Inherit,
    Method,
}

pub struct Chunk {
    pub count: i32,
    pub capacity: i32,
    pub code: *mut u8,
    pub lines: *mut i32,
    pub constants: ValueArray,
}

pub unsafe fn init_chunk(chunk: *mut Chunk) {
    let chunk = &mut *chunk;
    chunk.count = 0;
    chunk.capacity = 0;
    chunk.code = ptr::null_mut();
    chunk.lines = ptr::null_mut();
    init_value_array(&mut chunk.constants);
}

pub unsafe fn free_chunk(chunk: *mut Chunk) {
    {
        let chunk = &mut *chunk;
        free_array!(u8, chunk.code, chunk.capacity);
        free_array!(i32, chunk.lines, chunk.capacity);
        free_value_array(&mut chunk.constants);
    }
    init_chunk(chunk);
}

pub unsafe fn write_chunk(chunk: *mut Chunk, byte: u8, line: i32) {
    let chunk = &mut *chunk;

    if chunk.capacity < chunk.count + 1 {
        let old_capacity = chunk.capacity;
        chunk.capacity = grow_capacity!(old_capacity);
        chunk.code = grow_array!(u8, chunk.code, old_capacity, chunk.capacity);
        chunk.lines = grow_array!(i32, chunk.lines, old_capacity, chunk.capacity);
    }

    *chunk.code.offset(chunk.count as isize) = byte;
    *chunk.lines.offset(chunk.count as isize) = line;
    chunk.count += 1;
}

pub unsafe fn add_constant(chunk: *mut Chunk, value: Value) -> i32 {
    push(value);
    let chunk = &mut *chunk;
    write_value_array(&mut chunk.constants, value);
    pop();
    chunk.constants.count - 1
}
