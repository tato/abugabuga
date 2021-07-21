use std::mem::{self};

use chunk::{Chunk, OP_CONSTANT, OP_RETURN, add_constant, free_chunk, init_chunk, write_chunk};
use debug::disassemble_chunk;

#[macro_use]
mod memory;
mod chunk;
mod value;
mod debug;

fn main() {
    unsafe {
        let mut chunk: Chunk = mem::zeroed();
        init_chunk(&mut chunk);

        let constant = add_constant(&mut chunk, 1.2);
        write_chunk(&mut chunk, OP_CONSTANT, 123);
        write_chunk(&mut chunk, constant as u8, 123);

        write_chunk(&mut chunk, OP_RETURN, 123);
        disassemble_chunk(&mut chunk, "test chunk");

        free_chunk(&mut chunk);
    }
}
