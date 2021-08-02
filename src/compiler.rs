use std::{mem, ptr, slice, str, u8};

use crate::{chunk::{add_constant, write_chunk, Chunk, OpCode}, debug::disassemble_chunk, object::{Obj, copy_string}, scanner::{init_scanner, scan_token, Token, TokenType}, value::{Value, number_val, obj_val}};

pub struct Parser {
    pub current: Token,
    pub previous: Token,
    pub had_error: bool,
    pub panic_mode: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

type ParseFn = unsafe fn();

#[derive(Clone, Copy)]
pub struct ParseRule {
    pub prefix: Option<ParseFn>,
    pub infix: Option<ParseFn>,
    pub precedence: Precedence,
}

#[allow(non_upper_case_globals)]
pub static mut parser: Parser = Parser {
    current: Token {
        ty: TokenType::Nil,
        line: 0,
        length: 0,
        start: ptr::null_mut(),
    },
    previous: Token {
        ty: TokenType::Nil,
        line: 0,
        length: 0,
        start: ptr::null_mut(),
    },
    had_error: false,
    panic_mode: false,
};
#[allow(non_upper_case_globals)]
pub static mut compiling_chunk: *mut Chunk = ptr::null_mut();

unsafe fn current_chunk() -> *mut Chunk {
    compiling_chunk
}

unsafe fn error_at(token: *mut Token, message: &str) {
    if parser.panic_mode {
        return;
    }
    parser.panic_mode = true;

    let token = &*token;
    eprint!("[line {}] Error", token.line);

    if token.ty == TokenType::Eof {
        eprint!(" at end");
    } else if token.ty == TokenType::Error {
    } else {
        eprint!(
            " at '{}'",
            str::from_utf8_unchecked(slice::from_raw_parts(token.start, token.length as usize))
        );
    }

    eprintln!(": {}", message);
    parser.had_error = true;
}

unsafe fn error(message: &str) {
    error_at(&mut parser.previous, message);
}

unsafe fn error_at_current(message: &str) {
    error_at(&mut parser.current, message);
}

pub unsafe fn compile(source: &str, chunk: &mut Chunk) -> bool {
    init_scanner(source);
    compiling_chunk = chunk;

    parser.had_error = false;
    parser.panic_mode = false;

    advance();
    expression();
    consume(TokenType::Eof, "Expect end of expression.");
    end_compiler();
    !parser.had_error
}

unsafe fn advance() {
    parser.previous = parser.current;

    loop {
        parser.current = scan_token();
        if parser.current.ty != TokenType::Error {
            break;
        }
        error_at_current(str::from_utf8_unchecked(slice::from_raw_parts(
            parser.current.start,
            parser.current.length as usize,
        )));
    }
}

unsafe fn consume(ty: TokenType, message: &str) {
    if parser.current.ty == ty {
        advance();
        return;
    }

    error_at_current(message);
}

unsafe fn emit_byte(byte: u8) {
    write_chunk(current_chunk(), byte, parser.previous.line);
}

unsafe fn emit_bytes(byte1: u8, byte2: u8) {
    emit_byte(byte1);
    emit_byte(byte2);
}

unsafe fn emit_return() {
    emit_byte(OpCode::Return as u8);
}

unsafe fn make_constant(value: Value) -> u8 {
    let constant = add_constant(current_chunk(), value);
    if constant > u8::MAX as i32 {
        error("Too many constants in one chunk.");
        return 0;
    }
    return constant as u8;
}

unsafe fn emit_constant(value: Value) {
    emit_bytes(OpCode::Constant as u8, make_constant(value));
}

unsafe fn end_compiler() {
    emit_return();
    #[cfg(feature = "debug_print_code")]
    {
        if !parser.had_error {
            disassemble_chunk(current_chunk(), "code");
        }
    }
}

unsafe fn binary() {
    let operator_type = parser.previous.ty;
    let rule = get_rule(operator_type);
    parse_precedence(mem::transmute((*rule).precedence as u8 + 1));

    match operator_type {
        TokenType::BangEqual => emit_bytes(OpCode::Equal as u8, OpCode::Not as u8),
        TokenType::EqualEqual => emit_byte(OpCode::Equal as u8),
        TokenType::Greater => emit_byte(OpCode::Greater as u8),
        TokenType::GreaterEqual => emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
        TokenType::Less => emit_byte(OpCode::Less as u8),
        TokenType::LessEqual => emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
        TokenType::Plus => emit_byte(OpCode::Add as u8),
        TokenType::Minus => emit_byte(OpCode::Subtract as u8),
        TokenType::Star => emit_byte(OpCode::Multiply as u8),
        TokenType::Slash => emit_byte(OpCode::Divide as u8),
        _ => return, // unreachable
    }
}

unsafe fn literal() {
    match parser.previous.ty {
        TokenType::False => emit_byte(OpCode::False as u8),
        TokenType::Nil => emit_byte(OpCode::Nil as u8),
        TokenType::True => emit_byte(OpCode::True as u8),
        _ => return, // unreachable
    }
}

unsafe fn grouping() {
    expression();
    consume(TokenType::RightParen, "Expect ')' after expression.");
}

unsafe fn number() {
    let value: f32 = str::from_utf8_unchecked(slice::from_raw_parts(
        parser.previous.start,
        parser.previous.length as usize,
    ))
    .parse()
    .unwrap();
    emit_constant(number_val(value as f64));
}

unsafe fn string() {
    emit_constant(obj_val(copy_string(parser.previous.start.add(1), parser.previous.length - 2) as *mut Obj));
}

unsafe fn unary() {
    let ty = parser.previous.ty;

    parse_precedence(Precedence::Unary);

    match ty {
        TokenType::Bang => emit_byte(OpCode::Not as u8),
        TokenType::Minus => emit_byte(OpCode::Negate as u8),
        _ => return, // unreachable
    }
}

#[allow(non_upper_case_globals)]
static rules: [ParseRule; 40] = unsafe {
    let mut data = [ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    }; 40];
    macro_rules! set_data {
        ($ty:ident, $pre:expr, $in:expr, $prec:ident) => {
            data[TokenType::$ty as u8 as usize] = ParseRule {
                prefix: $pre,
                infix: $in,
                precedence: Precedence::$prec,
            }
        };
    }
    set_data!(LeftParen, Some(grouping), None, None);
    set_data!(RightParen, None, None, None);
    set_data!(LeftBrace, None, None, None);
    set_data!(RightBrace, None, None, None);
    set_data!(Comma, None, None, None);
    set_data!(Dot, None, None, None);
    set_data!(Minus, Some(unary), Some(binary), Term);
    set_data!(Plus, None, Some(binary), Term);
    set_data!(Semicolon, None, None, None);
    set_data!(Slash, None, Some(binary), Factor);
    set_data!(Star, None, Some(binary), Factor);
    set_data!(Bang, Some(unary), None, None);
    set_data!(BangEqual, None, Some(binary), Equality);
    set_data!(Equal, None, None, None);
    set_data!(EqualEqual, None, Some(binary), Equality);
    set_data!(Greater, None, Some(binary), Comparison);
    set_data!(GreaterEqual, None, Some(binary), Comparison);
    set_data!(Less, None, Some(binary), Comparison);
    set_data!(LessEqual, None, Some(binary), Comparison);
    set_data!(Identifier, None, None, None);
    set_data!(String, Some(string), None, None);
    set_data!(Number, Some(number), None, None);
    set_data!(And, None, None, None);
    set_data!(Class, None, None, None);
    set_data!(Else, None, None, None);
    set_data!(False, Some(literal), None, None);
    set_data!(For, None, None, None);
    set_data!(Fun, None, None, None);
    set_data!(If, None, None, None);
    set_data!(Nil, Some(literal), None, None);
    set_data!(Or, None, None, None);
    set_data!(Print, None, None, None);
    set_data!(Return, None, None, None);
    set_data!(Super, None, None, None);
    set_data!(This, None, None, None);
    set_data!(True, Some(literal), None, None);
    set_data!(Var, None, None, None);
    set_data!(While, None, None, None);
    set_data!(Error, None, None, None);
    set_data!(Eof, None, None, None);
    mem::transmute(data)
};

unsafe fn parse_precedence(precedence: Precedence) {
    advance();
    let prefix_rule = (*get_rule(parser.previous.ty)).prefix;
    if prefix_rule == None {
        error("Expect expression.");
        return;
    }
    (prefix_rule.unwrap())();

    while precedence <= (*get_rule(parser.current.ty)).precedence {
        advance();
        let infix_rule = (*get_rule(parser.previous.ty)).infix;
        (infix_rule.unwrap())();
    }
}

unsafe fn get_rule(ty: TokenType) -> *const ParseRule {
    &rules[ty as u8 as usize]
}

unsafe fn expression() {
    parse_precedence(Precedence::Assignment);
}
