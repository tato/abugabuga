use std::{mem, ptr, slice, str, u8};

use crate::{
    chunk::{add_constant, write_chunk, Chunk, OpCode},
    memory::mark_object,
    object::{copy_string, new_function, Obj, ObjFunction},
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
    _Primary,
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
    pub is_captured: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct Upvalue {
    pub index: u8,
    pub is_local: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FunctionType {
    Function,
    Script,
}

#[derive(Clone, Copy, Debug)]
pub struct Compiler {
    pub enclosing: *mut Compiler,

    pub function: *mut ObjFunction,
    pub ty: FunctionType,

    pub locals: [Local; UINT8_COUNT],
    pub local_count: i32,
    pub upvalues: [Upvalue; UINT8_COUNT],
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

unsafe fn current_chunk() -> *mut Chunk {
    &mut (*(*current).function).chunk
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

pub unsafe fn compile(source: &str) -> *mut ObjFunction {
    init_scanner(source);
    let mut compiler: Compiler = mem::zeroed();
    init_compiler(&mut compiler, FunctionType::Script);

    parser.had_error = false;
    parser.panic_mode = false;

    advance();

    while !mtch(TokenType::Eof) {
        declaration();
    }

    let function = end_compiler();
    if parser.had_error {
        ptr::null_mut()
    } else {
        function
    }
}

pub unsafe fn mark_compiler_roots() {
    let mut compiler = current;
    while compiler != ptr::null_mut() {
        mark_object((*compiler).function as *mut Obj);
        compiler = (*compiler).enclosing;
    }
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

unsafe fn emit_loop(loop_start: i32) {
    emit_byte(OpCode::Loop as u8);

    let offset = (*current_chunk()).count - loop_start + 2;
    if offset > u16::MAX as i32 {
        error("Loop body too large.");
    }

    emit_byte(((offset >> 8) & 0xff) as u8);
    emit_byte((offset & 0xff) as u8);
}

unsafe fn emit_jump(instruction: u8) -> i32 {
    emit_byte(instruction);
    emit_byte(0xff);
    emit_byte(0xff);
    (*current_chunk()).count - 2
}

unsafe fn emit_return() {
    emit_byte(OpCode::Nil as u8);
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

unsafe fn patch_jump(offset: i32) {
    let jump = (*current_chunk()).count - offset - 2;

    if jump > u16::MAX.into() {
        error("Too much code to jump over.");
    }

    *(*current_chunk()).code.offset(offset as isize) = ((jump >> 8) & 0xff) as u8;
    *(*current_chunk()).code.offset(offset as isize + 1) = (jump & 0xff) as u8;
}

unsafe fn init_compiler(compiler: *mut Compiler, ty: FunctionType) {
    let compiler = &mut *compiler;
    compiler.enclosing = current;
    compiler.function = ptr::null_mut();
    compiler.ty = ty;
    compiler.local_count = 0;
    compiler.scope_depth = 0;
    compiler.function = new_function();
    current = compiler;

    if ty != FunctionType::Script {
        (*(*current).function).name = copy_string(parser.previous.start, parser.previous.length);
    }

    let local = &mut (*current).locals[(*current).local_count as usize];
    (*current).local_count += 1;
    local.depth = 0;
    local.name.start = "".as_bytes().as_ptr();
    local.name.length = 0;
    local.is_captured = false;
}

unsafe fn end_compiler() -> *mut ObjFunction {
    emit_return();
    let function = (*current).function;

    #[cfg(feature = "debug_print_code")]
    {
        if !parser.had_error {
            let name = if (*function).name != ptr::null_mut() {
                let name = &*(*function).name;
                str::from_utf8_unchecked(slice::from_raw_parts(name.chars, name.length as usize))
            } else {
                "<script>"
            };
            disassemble_chunk(current_chunk(), name);
        }
    }

    current = (*current).enclosing;
    function
}

unsafe fn begin_scope() {
    (*current).scope_depth += 1;
}

unsafe fn end_scope() {
    (*current).scope_depth -= 1;

    while (*current).local_count > 0
        && (*current).locals[(*current).local_count as usize - 1].depth > (*current).scope_depth
    {
        if (*current).locals[(*current).local_count as usize - 1].is_captured {
            emit_byte(OpCode::CloseUpvalue as u8)
        } else {
            emit_byte(OpCode::Pop as u8);
        }
        (*current).local_count -= 1;
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

unsafe fn call(_can_assign: bool) {
    let arg_count = argument_list();
    emit_bytes(OpCode::Call as u8, arg_count);
}

unsafe fn dot(can_assign: bool) {
    consume(TokenType::Identifier, "Expect property name after '.'.");
    let name = identifier_constant(&parser.previous);

    if can_assign && mtch(TokenType::Equal) {
        expression();
        emit_bytes(OpCode::SetProperty as u8, name);
    } else {
        emit_bytes(OpCode::GetProperty as u8, name);
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

unsafe fn or(_can_assign: bool) {
    let else_jump = emit_jump(OpCode::JumpIfFalse as u8);
    let end_jump = emit_jump(OpCode::Jump as u8);

    patch_jump(else_jump);
    emit_byte(OpCode::Pop as u8);

    parse_precedence(Precedence::Or);
    patch_jump(end_jump);
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
    } else if {
        arg = resolve_upvalue(current, &name);
        arg
    } != -1
    {
        get_op = OpCode::GetUpvalue;
        set_op = OpCode::SetUpvalue;
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
    set_data!(LeftParen, Some(grouping), Some(call), Call);
    set_data!(RightParen, None, None, None);
    set_data!(LeftBrace, None, None, None);
    set_data!(RightBrace, None, None, None);
    set_data!(Comma, None, None, None);
    set_data!(Dot, None, Some(dot), Call);
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
    set_data!(And, None, Some(and), And);
    set_data!(Class, None, None, None);
    set_data!(Else, None, None, None);
    set_data!(False, Some(literal), None, None);
    set_data!(For, None, None, None);
    set_data!(Fun, None, None, None);
    set_data!(If, None, None, None);
    set_data!(Nil, Some(literal), None, None);
    set_data!(Or, None, Some(or), Or);
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

unsafe fn add_upvalue(compiler: *mut Compiler, index: u8, is_local: bool) -> i32 {
    let upvalue_count = (*(*compiler).function).upvalue_count;

    for i in 0..upvalue_count {
        let upvalue = &(*compiler).upvalues[i as usize];
        if upvalue.index == index && upvalue.is_local == is_local {
            return i;
        }
    }

    if upvalue_count as usize == UINT8_COUNT {
        error("Too many closure variables in function.");
        return 0;
    }

    (*compiler).upvalues[upvalue_count as usize].is_local = is_local;
    (*compiler).upvalues[upvalue_count as usize].index = index;
    let res = (*(*compiler).function).upvalue_count;
    (*(*compiler).function).upvalue_count += 1;
    res
}

unsafe fn resolve_upvalue(compiler: *mut Compiler, name: &Token) -> i32 {
    if (*compiler).enclosing == ptr::null_mut() {
        return -1;
    }
    let local = resolve_local((*compiler).enclosing, name);
    if local != -1 {
        (*(*compiler).enclosing).locals[local as usize].is_captured = true;
        return add_upvalue(compiler, local as u8, true);
    }

    let upvalue = resolve_upvalue((*compiler).enclosing, name);
    if upvalue != -1 {
        return add_upvalue(compiler, upvalue as u8, false);
    }

    -1
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
    local.is_captured = false;
}

unsafe fn declare_variable() {
    if (*current).scope_depth == 0 {
        return;
    }

    let name = &parser.previous;
    for i in (0..(*current).local_count).rev() {
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
    if (*current).scope_depth == 0 {
        return;
    }
    (*current).locals[(*current).local_count as usize - 1].depth = (*current).scope_depth;
}

unsafe fn define_variable(global: u8) {
    if (*current).scope_depth > 0 {
        mark_initialized();
        return; // runtime NOP, keep initializer temporary on top of the stack
    }

    emit_bytes(OpCode::DefineGlobal as u8, global);
}

unsafe fn argument_list() -> u8 {
    let mut arg_count = 0;
    if !check(TokenType::RightParen) {
        loop {
            expression();
            if arg_count == 255 {
                error("Can't have more than 255 arguments.");
            }
            arg_count += 1;
            if !mtch(TokenType::Comma) {
                break;
            }
        }
    }
    consume(TokenType::RightParen, "Expect ')' after arguments.");
    arg_count
}

unsafe fn and(_can_assign: bool) {
    let end_jump = emit_jump(OpCode::JumpIfFalse as u8);

    emit_byte(OpCode::Pop as u8);
    parse_precedence(Precedence::And);

    patch_jump(end_jump);
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

unsafe fn function(ty: FunctionType) {
    let mut compiler: Compiler = mem::zeroed();
    init_compiler(&mut compiler, ty);
    begin_scope();

    consume(TokenType::LeftParen, "Expect '(' after function name.");
    if !check(TokenType::RightParen) {
        loop {
            (*(*current).function).arity += 1;
            if (*(*current).function).arity > 255 {
                error_at_current("Can't have more than 255 parameters.");
            }
            let constant = parse_variable("Expect parameter name");
            define_variable(constant);

            if !mtch(TokenType::Comma) {
                break;
            }
        }
    }
    consume(TokenType::RightParen, "Expect ')' after parameters.");
    consume(TokenType::LeftBrace, "Expect '{' before function body.");
    block();

    let function = end_compiler();
    emit_bytes(
        OpCode::Closure as u8,
        make_constant(obj_val(function as *mut Obj)),
    );

    for i in 0..(*function).upvalue_count {
        emit_byte(if compiler.upvalues[i as usize].is_local {
            1
        } else {
            0
        });
        emit_byte(compiler.upvalues[i as usize].index);
    }
}

unsafe fn class_declaration() {
    consume(TokenType::Identifier, "Expect class name.");
    let name_constant = identifier_constant(&parser.previous);
    declare_variable();

    emit_bytes(OpCode::Class as u8, name_constant);
    define_variable(name_constant);

    consume(TokenType::LeftBrace, "Expect '{' before class body.");
    consume(TokenType::RightBrace, "Expect '}' after class body.");
}

unsafe fn fun_declaration() {
    let global = parse_variable("Expect function name.");
    mark_initialized();
    function(FunctionType::Function);
    define_variable(global);
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

unsafe fn for_statement() {
    begin_scope();

    consume(TokenType::LeftParen, "Expect '(' after 'for'.");
    if mtch(TokenType::Semicolon) {
    } else if mtch(TokenType::Var) {
        var_declaration();
    } else {
        expression_statement();
    }

    let mut loop_start = (*current_chunk()).count;
    let mut exit_jump = None;
    if !mtch(TokenType::Semicolon) {
        expression();
        consume(TokenType::Semicolon, "Expect ';' after loop condition.");

        exit_jump = Some(emit_jump(OpCode::JumpIfFalse as u8));
        emit_byte(OpCode::Pop as u8);
    }

    if !mtch(TokenType::RightParen) {
        let body_jump = emit_jump(OpCode::Jump as u8);
        let increment_start = (*current_chunk()).count;
        expression();
        emit_byte(OpCode::Pop as u8);
        consume(TokenType::RightParen, "Expect ')' after for clauses.");

        emit_loop(loop_start);
        loop_start = increment_start;
        patch_jump(body_jump);
    }

    statement();
    emit_loop(loop_start);

    if let Some(exit_jump) = exit_jump {
        patch_jump(exit_jump);
        emit_byte(OpCode::Pop as u8);
    }

    end_scope();
}

unsafe fn if_statement() {
    consume(TokenType::LeftParen, "Expect '(' after 'if'.");
    expression();
    consume(TokenType::RightParen, "Expect ')' after condition.");

    let then_jump = emit_jump(OpCode::JumpIfFalse as u8);

    emit_byte(OpCode::Pop as u8);
    statement();
    let else_jump = emit_jump(OpCode::Jump as u8);

    patch_jump(then_jump);

    emit_byte(OpCode::Pop as u8);
    if mtch(TokenType::Else) {
        statement()
    }
    patch_jump(else_jump);
}

unsafe fn print_statement() {
    expression();
    consume(TokenType::Semicolon, "Expect ';' after value.");
    emit_byte(OpCode::Print as u8);
}

unsafe fn return_statement() {
    if (*current).ty == FunctionType::Script {
        error("Can't return from top-level code.");
    }

    if mtch(TokenType::Semicolon) {
        emit_return();
    } else {
        expression();
        consume(TokenType::Semicolon, "Expect ';' after return value.");
        emit_byte(OpCode::Return as u8);
    }
}

unsafe fn while_statement() {
    let loop_start = (*current_chunk()).count;

    consume(TokenType::LeftParen, "Expect '(' after 'while'.");
    expression();
    consume(TokenType::RightParen, "Expect ')' after condition.");

    let exit_jump = emit_jump(OpCode::JumpIfFalse as u8);
    emit_byte(OpCode::Pop as u8);
    statement();
    emit_loop(loop_start);

    patch_jump(exit_jump);
    emit_byte(OpCode::Pop as u8);
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
    if mtch(TokenType::Class) {
        class_declaration();
    } else if mtch(TokenType::Fun) {
        fun_declaration();
    } else if mtch(TokenType::Var) {
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
    } else if mtch(TokenType::For) {
        for_statement();
    } else if mtch(TokenType::If) {
        if_statement();
    } else if mtch(TokenType::Return) {
        return_statement();
    } else if mtch(TokenType::While) {
        while_statement();
    } else if mtch(TokenType::LeftBrace) {
        begin_scope();
        block();
        end_scope();
    } else {
        expression_statement();
    }
}
