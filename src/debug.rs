use std::usize;

use crate::{chunk::{self, Chunk}, value::print_value};

pub unsafe fn disassemble_chunk(chunk: *mut Chunk, name: &str) {
    println!("== {} ==", name);
    let chunk = &mut *chunk;
    let mut offset = 0i32;
    while offset < chunk.count {
        offset = disassemble_instruction(chunk, offset);
    }
}

pub unsafe fn disassemble_instruction(chunk: *mut Chunk, offset: i32) -> i32 {
    print!("{:04} ", offset);

    let chunk = &mut *chunk;

    if offset > 0 && *chunk.lines.offset(offset as isize) == *chunk.lines.offset(offset as isize - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", *chunk.lines.offset(offset as isize));
    }

    let instruction = *chunk.code.offset(offset as isize);
    match instruction {
        chunk::OP_CONSTANT => constant_instruction("OP_CONSTANT", chunk, offset),
        chunk::OP_RETURN => simple_instruction("OP_RETURN", offset),
        _ => {
            println!("Unknown opcode {}", instruction);
            offset + 1
        }
    }
}

fn simple_instruction(name: &str, offset: i32) -> i32 {
    println!("{}", name);
    offset + 1
}

unsafe fn constant_instruction(name: &str, chunk: *mut Chunk, offset: i32) -> i32 {
    let chunk = &mut *chunk;
    let constant = *chunk.code.offset(offset as isize + 1);
    print!("{:-16} {:4} '", name, constant);
    print_value(*chunk.constants.values.add(constant as usize));
    println!("'");
    offset + 2
}