use std::{slice, str};

use crate::scanner::{init_scanner, scan_token, TokenType};

pub unsafe fn compile(source: &str) {
    init_scanner(source);

    let mut line = -1i32;
    loop {
        let token = scan_token();
        if token.line != line {
            print!("{:4} ", token.line);
            line = token.line;
        } else {
            print!("   | ");
        }
        println!(
            "{:2} '{}'",
            token.ty as u8,
            str::from_utf8_unchecked(slice::from_raw_parts(token.start, token.length as usize))
        );

        if token.ty == TokenType::Eof {
            break;
        }
    }
}
