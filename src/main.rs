use std::mem::{self};

use chunk::{Chunk, OpCode, add_constant, free_chunk, init_chunk, write_chunk};
use debug::disassemble_chunk;
use vm::{free_vm, init_vm, interpret};

#[macro_use]
mod memory;
mod chunk;
mod debug;
mod value;
mod vm;

fn main() {
    unsafe {
        init_vm();

        let mut chunk: Chunk = mem::zeroed();
        init_chunk(&mut chunk);

        let constant = add_constant(&mut chunk, 1.2);
        write_chunk(&mut chunk, OpCode::Constant as u8, 123);
        write_chunk(&mut chunk, constant as u8, 123);
        let constant = add_constant(&mut chunk, 3.4);
        write_chunk(&mut chunk, OpCode::Constant as u8, 123);
        write_chunk(&mut chunk, constant as u8, 123);

        write_chunk(&mut chunk, OpCode::Add as u8, 123);

        let constant = add_constant(&mut chunk, 5.6);
        write_chunk(&mut chunk, OpCode::Constant as u8, 123);
        write_chunk(&mut chunk, constant as u8, 123);

        write_chunk(&mut chunk, OpCode::Divide as u8, 123);
        write_chunk(&mut chunk, OpCode::Negate as u8, 123);

        write_chunk(&mut chunk, OpCode::Return as u8, 123);
        disassemble_chunk(&mut chunk, "test chunk");

        interpret(&mut chunk);

        free_vm();
        free_chunk(&mut chunk);
    }
}
