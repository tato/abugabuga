use std::{mem, ptr, slice, str, u8};

use crate::{
    chunk::{add_constant, write_chunk, Chunk, OpCode},
    object::{copy_string, Obj},
    scanner::{init_scanner, scan_token, Token, TokenType},
    value::{number_val, obj_val, Value},
    UINT8_COUNT,
};

#[cfg(feature = "debug_print_code")]
use crate::debug::disassemble_chunk;

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

type ParseFn = unsafe fn(bool);

#[derive(Clone, Copy)]
pub struct ParseRule {
    pub prefix: Option<ParseFn>,
    pub infix: Option<ParseFn>,
    pub precedence: Precedence,
}

#[derive(Clone, Copy, Debug)]
pub struct Local {
    pub name: Token,
    pub depth: i32,
}

#[derive(Clone, Copy, Debug)]
pub struct Compiler {
    pub locals: [Local; UINT8_COUNT],
    pub local_count: i32,
    pub scope_depth: i32,
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
pub static mut current: *mut Compiler = ptr::null_mut();
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
    let mut compiler: Compiler = mem::zeroed();
    init_compiler(&mut compiler);
    compiling_chunk = chunk;

    parser.had_error = false;
    parser.panic_mode = false;

    advance();

    while !mtch(TokenType::Eof) {
        declaration();
    }

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

unsafe fn check(ty: TokenType) -> bool {
    return parser.current.ty == ty;
}

unsafe fn mtch(ty: TokenType) -> bool {
    if !check(ty) {
        return false;
    }
    advance();
    true
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

unsafe fn init_compiler(compiler: *mut Compiler) {
    let compiler = &mut *compiler;
    compiler.local_count = 0;
    compiler.scope_depth = 0;
    current = compiler;
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

unsafe fn begin_scope() {
    (*current).scope_depth += 1;
}

unsafe fn end_scope() {
    (*current).scope_depth -= 1;

    while (*current).local_count > 0
        && (*current).locals[(*current).local_count as usize].depth > (*current).scope_depth
    {
        emit_byte(OpCode::Pop as u8);
        (*current).local_count;
    }
}

unsafe fn binary(_can_assign: bool) {
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

unsafe fn literal(_can_assign: bool) {
    match parser.previous.ty {
        TokenType::False => emit_byte(OpCode::False as u8),
        TokenType::Nil => emit_byte(OpCode::Nil as u8),
        TokenType::True => emit_byte(OpCode::True as u8),
        _ => return, // unreachable
    }
}

unsafe fn grouping(_can_assign: bool) {
    expression();
    consume(TokenType::RightParen, "Expect ')' after expression.");
}

unsafe fn number(_can_assign: bool) {
    let value: f32 = str::from_utf8_unchecked(slice::from_raw_parts(
        parser.previous.start,
        parser.previous.length as usize,
    ))
    .parse()
    .unwrap();
    emit_constant(number_val(value as f64));
}

unsafe fn string(_can_assign: bool) {
    emit_constant(obj_val(
        copy_string(parser.previous.start.add(1), parser.previous.length - 2) as *mut Obj,
    ));
}

unsafe fn named_variable(name: Token, can_assign: bool) {
    let (get_op, set_op);
    let mut arg = resolve_local(current, &name);
    if arg != -1 {
        get_op = OpCode::GetLocal;
        set_op = OpCode::SetLocal;
    } else {
        arg = identifier_constant(&name).into();
        get_op = OpCode::GetGlobal;
        set_op = OpCode::SetGlobal;
    }
    let arg = arg as u8;

    if can_assign && mtch(TokenType::Equal) {
        expression();
        emit_bytes(set_op as u8, arg);
    } else {
        emit_bytes(get_op as u8, arg);
    }
}

unsafe fn variable(can_assign: bool) {
    named_variable(parser.previous, can_assign);
}

unsafe fn unary(_can_assign: bool) {
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
    set_data!(Identifier, Some(variable), None, None);
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

    let can_assign = precedence <= Precedence::Assignment;
    (prefix_rule.unwrap())(can_assign);

    while precedence <= (*get_rule(parser.current.ty)).precedence {
        advance();
        let infix_rule = (*get_rule(parser.previous.ty)).infix;
        (infix_rule.unwrap())(can_assign);
    }

    if can_assign && mtch(TokenType::Equal) {
        error("Invalid assignment target.");
    }
}

unsafe fn identifier_constant(name: &Token) -> u8 {
    make_constant(obj_val(copy_string(name.start, name.length) as *mut Obj))
}

unsafe fn identifiers_equal(a: &Token, b: &Token) -> bool {
    if a.length != b.length {
        return false;
    }
    slice::from_raw_parts(a.start, a.length as usize)
        == slice::from_raw_parts(b.start, b.length as usize)
}

unsafe fn resolve_local(compiler: *mut Compiler, name: &Token) -> i32 {
    for i in (0..(*compiler).local_count).rev() {
        let local = &(*compiler).locals[i as usize];
        if identifiers_equal(name, &local.name) {
            if local.depth == -1 {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }
    return -1;
}

unsafe fn add_local(name: Token) {
    if (*current).local_count as usize == UINT8_COUNT {
        error("Too many local variables in scope.");
        return;
    }

    let local = &mut (*current).locals[(*current).local_count as usize];
    (*current).local_count += 1;
    local.name = name;
    local.depth = -1;
}

unsafe fn declare_variable() {
    if (*current).scope_depth == 0 {
        return;
    }

    let name = &parser.previous;
    for i in (0..(*current).local_count - 1).rev() {
        let local = &(*current).locals[i as usize];
        if local.depth != -1 && local.depth < (*current).scope_depth {
            break;
        }

        if identifiers_equal(name, &local.name) {
            error("Already a variable with this name in this scope.");
        }
    }
    add_local(*name);
}

unsafe fn parse_variable(error_message: &str) -> u8 {
    consume(TokenType::Identifier, error_message);

    declare_variable();
    if (*current).scope_depth > 0 {
        return 0;
    }

    identifier_constant(&parser.previous)
}

unsafe fn mark_initialized() {
    (*current).locals[(*current).local_count as usize - 1].depth = (*current).scope_depth;
}

unsafe fn define_variable(global: u8) {
    if (*current).scope_depth > 0 {
        mark_initialized();
        return; // runtime NOP, keep initializer temporary on top of the stack
    }

    emit_bytes(OpCode::DefineGlobal as u8, global);
}

unsafe fn get_rule(ty: TokenType) -> *const ParseRule {
    &rules[ty as u8 as usize]
}

unsafe fn expression() {
    parse_precedence(Precedence::Assignment);
}

unsafe fn block() {
    while !check(TokenType::RightBrace) && !check(TokenType::Eof) {
        declaration();
    }
    consume(TokenType::RightBrace, "Expect '}' after block.");
}

unsafe fn var_declaration() {
    let global = parse_variable("Expect variable name.");

    if mtch(TokenType::Equal) {
        expression();
    } else {
        emit_byte(OpCode::Nil as u8);
    }

    consume(
        TokenType::Semicolon,
        "Expect ';' after variable declaration.",
    );

    define_variable(global);
}

unsafe fn expression_statement() {
    expression();
    consume(TokenType::Semicolon, "Expect ';' after expression;");
    emit_byte(OpCode::Pop as u8);
}

unsafe fn print_statement() {
    expression();
    consume(TokenType::Semicolon, "Expect ';' after value.");
    emit_byte(OpCode::Print as u8);
}

unsafe fn synchronize() {
    parser.panic_mode = false;

    while parser.current.ty != TokenType::Eof {
        if parser.previous.ty == TokenType::Semicolon {
            return;
        }
        match parser.current.ty {
            TokenType::Class
            | TokenType::Fun
            | TokenType::Var
            | TokenType::For
            | TokenType::If
            | TokenType::While
            | TokenType::Print
            | TokenType::Return => return,
            _ => {}
        }

        advance();
    }
}

unsafe fn declaration() {
    if mtch(TokenType::Var) {
        var_declaration();
    } else {
        statement();
    }

    if parser.panic_mode {
        synchronize();
    }
}

unsafe fn statement() {
    if mtch(TokenType::Print) {
        print_statement();
    } else if mtch(TokenType::LeftBrace) {
        begin_scope();
        block();
        end_scope();
    } else {
        expression_statement();
    }
}
