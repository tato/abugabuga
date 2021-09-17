use crate::{
    array::Array,
    value::Value,
};

pub enum OpCode {
    Constant,
    Nil,
    True,
    False,
    List,
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
    Index,
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
    pub code: Array<u8>,
    pub lines: Array<u16>,
    pub constants: Array<Value>,
}

pub unsafe fn init_chunk(chunk: *mut Chunk) {
    let chunk = &mut *chunk;
    chunk.code = Array::new();
    chunk.lines = Array::new();
    chunk.constants = Array::new();
}

pub unsafe fn free_chunk(chunk: *mut Chunk) {
    {
        let chunk = &mut *chunk;
        chunk.code.free(); // TODO: drop
        chunk.lines.free(); // TODO: drop
        chunk.constants.free(); // TODO: drop
    }
    init_chunk(chunk);
}

pub unsafe fn write_chunk(chunk: *mut Chunk, byte: u8, line: u16) {
    let chunk = &mut *chunk;
    chunk.code.append(byte);
    chunk.lines.append(line);
}

// pub unsafe fn gc_track_constant_for_chunk_or_strings_table(value: Value) {
//     let vm = &mut *GC.vm;
//     vm.push(value);
// }

// pub unsafe fn gc_untrack_constant_for_chunk_or_strings_table() {
//     let vm = &mut *GC.vm;
//     vm.pop();
// }

pub unsafe fn add_constant(chunk: *mut Chunk, value: Value) -> i32 {
    todo!("add_constant");
    // gc_track_constant_for_chunk_or_strings_table(value);
    let chunk = &mut *chunk;
    chunk.constants.append(value);
    // gc_untrack_constant_for_chunk_or_strings_table();
    chunk.constants.count() as i32 - 1
}
