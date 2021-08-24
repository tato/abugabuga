use std::usize;

use crate::{
    chunk::{Chunk, OpCode},
    object::as_function,
    value::print_value,
};

#[cfg(feature = "debug_print_code")]
pub unsafe fn disassemble_chunk(chunk: *mut Chunk, name: &str) {
    println!("== {} ==", name);
    let chunk = &mut *chunk;
    let mut offset = 0i32;
    while offset < chunk.count {
        offset = disassemble_instruction(chunk, offset);
    }
}

pub unsafe fn disassemble_instruction(chunk: *mut Chunk, mut offset: i32) -> i32 {
    print!("{:04} ", offset);

    let chunk = &mut *chunk;

    if offset > 0
        && *chunk.lines.offset(offset as isize) == *chunk.lines.offset(offset as isize - 1)
    {
        print!("   | ");
    } else {
        print!("{:4} ", *chunk.lines.offset(offset as isize));
    }

    let instruction = *chunk.code.offset(offset as isize);
    match instruction {
        i if i == OpCode::Constant as u8 => constant_instruction("OP_CONSTANT", chunk, offset),
        i if i == OpCode::Nil as u8 => simple_instruction("OP_NIL", offset),
        i if i == OpCode::True as u8 => simple_instruction("OP_TRUE", offset),
        i if i == OpCode::False as u8 => simple_instruction("OP_FALSE", offset),
        i if i == OpCode::List as u8 => constant_instruction("OP_LIST", chunk, offset),
        i if i == OpCode::Pop as u8 => simple_instruction("OP_POP", offset),
        i if i == OpCode::GetLocal as u8 => byte_instruction("OP_GET_LOCAL", chunk, offset),
        i if i == OpCode::SetLocal as u8 => byte_instruction("OPSET_LOCAL", chunk, offset),
        i if i == OpCode::GetGlobal as u8 => constant_instruction("OP_GET_GLOBAL", chunk, offset),
        i if i == OpCode::DefineGlobal as u8 => {
            constant_instruction("OP_DEFINE_GLOBAL", chunk, offset)
        }
        i if i == OpCode::SetGlobal as u8 => constant_instruction("OP_SET_GLOBAL", chunk, offset),
        i if i == OpCode::GetUpvalue as u8 => byte_instruction("OP_GET_UPVALUE", chunk, offset),
        i if i == OpCode::SetUpvalue as u8 => byte_instruction("OP_SET_UPVALUE", chunk, offset),
        i if i == OpCode::GetProperty as u8 => {
            constant_instruction("OP_GET_PROPERTY", chunk, offset)
        }
        i if i == OpCode::SetProperty as u8 => {
            constant_instruction("OP_SET_PROPERTY", chunk, offset)
        }
        i if i == OpCode::GetSuper as u8 => constant_instruction("OP_GET_SUPER", chunk, offset),
        i if i == OpCode::Index as u8 => simple_instruction("OP_INDEX", offset),
        i if i == OpCode::Equal as u8 => simple_instruction("OP_EQUAL", offset),
        i if i == OpCode::Greater as u8 => simple_instruction("OP_GREATER", offset),
        i if i == OpCode::Less as u8 => simple_instruction("OP_LESS", offset),
        i if i == OpCode::Add as u8 => simple_instruction("OP_ADD", offset),
        i if i == OpCode::Subtract as u8 => simple_instruction("OP_SUBTRACT", offset),
        i if i == OpCode::Multiply as u8 => simple_instruction("OP_MULTIPLY", offset),
        i if i == OpCode::Divide as u8 => simple_instruction("OP_DIVIDE", offset),
        i if i == OpCode::Not as u8 => simple_instruction("OP_NOT", offset),
        i if i == OpCode::Negate as u8 => simple_instruction("OP_NEGATE", offset),
        i if i == OpCode::Jump as u8 => jump_instruction("OP_JUMP", 1, chunk, offset),
        i if i == OpCode::JumpIfFalse as u8 => {
            jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset)
        }
        i if i == OpCode::Loop as u8 => jump_instruction("OP_LOOP", -1, chunk, offset),
        i if i == OpCode::Print as u8 => simple_instruction("OP_PRINT", offset),
        i if i == OpCode::Call as u8 => byte_instruction("OP_CALL", chunk, offset),
        i if i == OpCode::Invoke as u8 => invoke_instruction("OP_INVOKE", chunk, offset),
        i if i == OpCode::SuperInvoke as u8 => invoke_instruction("OP_SUPER_INVOKE", chunk, offset),
        i if i == OpCode::Closure as u8 => {
            offset += 1;
            let constant = *(*chunk).code.offset(offset as isize);
            offset += 1;
            print!("{:-16} {:4} ", "OP_CLOSURE", constant);
            print_value(*(*chunk).constants.values.offset(constant as isize));
            println!();

            let function = as_function(*(*chunk).constants.values.offset(constant as isize));
            for _j in 0..(*function).upvalue_count {
                let is_local = *(*chunk).code.offset(offset as isize);
                offset += 1;
                let index = *(*chunk).code.offset(offset as isize);
                offset += 1;
                println!(
                    "{:04}      |                     {} {}",
                    offset - 2,
                    if is_local == 1 { "local" } else { "upvalue" },
                    index
                );
            }

            offset
        }
        i if i == OpCode::CloseUpvalue as u8 => simple_instruction("OP_CLOSE_UPVALUE", offset),
        i if i == OpCode::Return as u8 => simple_instruction("OP_RETURN", offset),
        i if i == OpCode::Class as u8 => constant_instruction("OP_CLASS", chunk, offset),
        i if i == OpCode::Inherit as u8 => simple_instruction("OP_INHERIT", offset),
        i if i == OpCode::Method as u8 => constant_instruction("OP_METHOD", chunk, offset),
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

unsafe fn invoke_instruction(name: &str, chunk: *mut Chunk, offset: i32) -> i32 {
    let constant = *(*chunk).code.offset(offset as isize + 1);
    let arg_count = *(*chunk).code.offset(offset as isize + 2);
    print!("{:-16} ({} args) {:4} '", name, constant);
    print_value(*(*chunk).constants.values.add(constant as usize));
    println!("'");
    offset + 3
}

unsafe fn byte_instruction(name: &str, chunk: *mut Chunk, offset: i32) -> i32 {
    let slot = *(*chunk).code.offset(offset as isize + 1);
    println!("{:-16} {:4}", name, slot);
    offset + 2
}

unsafe fn jump_instruction(name: &str, sign: i32, chunk: *mut Chunk, offset: i32) -> i32 {
    let jump = (*(*chunk).code.offset(offset as isize + 1) as u16) << 8
        | *(*chunk).code.offset(offset as isize + 2) as u16;
    println!(
        "{:-16} {:4} -> {}",
        name,
        offset,
        offset + 3 + sign * jump as i32
    );
    offset + 3
}
