use std::{mem, ptr, str, u8};

use crate::{
    chunk::{add_constant, write_chunk, Chunk, OpCode},
    memory::{gc_track_parser, gc_untrack_parser, mark_object},
    object::{copy_string, new_function, Obj, ObjFunction},
    scanner::{Scanner, Token, TokenType},
    value::{number_val, obj_val, Value},
    UINT8_COUNT,
};

#[cfg(feature = "debug_print_code")]
use crate::debug::disassemble_chunk;

pub struct Parser<'source> {
    pub current: Token<'source>,
    pub previous: Token<'source>,
    pub had_error: bool,
    pub panic_mode: bool,

    pub current_compiler: *mut Compiler<'source>,
    pub current_class: *mut ClassCompiler,
    scanner: *mut Scanner<'source>,
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

type ParseFn = unsafe fn(&mut Parser, bool);

#[derive(Clone, Copy)]
pub struct ParseRule {
    pub prefix: Option<ParseFn>,
    pub infix: Option<ParseFn>,
    pub precedence: Precedence,
}

#[derive(Clone, Copy, Debug)]
pub struct Local<'source> {
    pub name: Token<'source>,
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
    Initializer,
    Method,
    Script,
}

#[derive(Clone, Copy, Debug)]
pub struct Compiler<'source> {
    pub enclosing: *mut Compiler<'source>,

    pub function: *mut ObjFunction,
    pub ty: FunctionType,

    pub locals: [Local<'source>; UINT8_COUNT],
    pub local_count: i32,
    pub upvalues: [Upvalue; UINT8_COUNT],
    pub scope_depth: i32,
}

pub struct ClassCompiler {
    pub enclosing: *mut ClassCompiler,
    pub has_superclass: bool,
}

impl<'source> Parser<'source> {
    unsafe fn current_chunk(&self) -> *mut Chunk {
        &mut (*(*self.current_compiler).function).chunk
    }

    unsafe fn error_at(&mut self, token: *mut Token, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;

        let token = &*token;
        eprint!("[line {}] Error", token.line);

        if token.ty == TokenType::Eof {
            eprint!(" at end");
        } else if token.ty == TokenType::Error {
        } else {
            eprint!(" at '{}'", token.lexeme);
        }

        eprintln!(": {}", message);
        self.had_error = true;
    }

    unsafe fn error(&mut self, message: &str) {
        let previous: *mut Token = &mut self.previous;
        self.error_at(previous, message);
    }

    unsafe fn error_at_current(&mut self, message: &str) {
        let current: *mut Token = &mut self.current;
        self.error_at(current, message);
    }

    unsafe fn advance(&mut self) {
        self.previous = self.current;

        loop {
            self.current = (*self.scanner).scan_token();
            if self.current.ty != TokenType::Error {
                break;
            }
            self.error_at_current(self.current.lexeme);
        }
    }

    unsafe fn consume(&mut self, ty: TokenType, message: &str) {
        if self.current.ty == ty {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    unsafe fn check(&self, ty: TokenType) -> bool {
        return self.current.ty == ty;
    }

    unsafe fn mtch(&mut self, ty: TokenType) -> bool {
        if !self.check(ty) {
            return false;
        }
        self.advance();
        true
    }

    unsafe fn emit_byte(&self, byte: u8) {
        write_chunk(self.current_chunk(), byte, self.previous.line);
    }

    unsafe fn emit_bytes(&self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    unsafe fn emit_loop(&mut self, loop_start: i32) {
        self.emit_byte(OpCode::Loop as u8);

        let offset = (*self.current_chunk()).count - loop_start + 2;
        if offset > u16::MAX as i32 {
            self.error("Loop body too large.");
        }

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    unsafe fn emit_jump(&self, instruction: u8) -> i32 {
        self.emit_byte(instruction);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        (*self.current_chunk()).count - 2
    }

    unsafe fn emit_return(&self) {
        if (*self.current_compiler).ty == FunctionType::Initializer {
            self.emit_bytes(OpCode::GetLocal as u8, 0);
        } else {
            self.emit_byte(OpCode::Nil as u8);
        }
        self.emit_byte(OpCode::Return as u8);
    }

    unsafe fn make_constant(&mut self, value: Value) -> u8 {
        let constant = add_constant(self.current_chunk(), value);
        if constant > u8::MAX as i32 {
            self.error("Too many constants in one chunk.");
            return 0;
        }
        return constant as u8;
    }

    unsafe fn emit_constant(&mut self, value: Value) {
        let shut_up_borrow_checker = self.make_constant(value);
        self.emit_bytes(OpCode::Constant as u8, shut_up_borrow_checker);
    }

    unsafe fn patch_jump(&mut self, offset: i32) {
        let jump = (*self.current_chunk()).count - offset - 2;

        if jump > u16::MAX.into() {
            self.error("Too much code to jump over.");
        }

        *(*self.current_chunk()).code.offset(offset as isize) = ((jump >> 8) & 0xff) as u8;
        *(*self.current_chunk()).code.offset(offset as isize + 1) = (jump & 0xff) as u8;
    }

    unsafe fn init_compiler(&mut self, compiler: *mut Compiler<'source>, ty: FunctionType) {
        let compiler = &mut *compiler;
        compiler.enclosing = self.current_compiler;
        compiler.function = ptr::null_mut();
        compiler.ty = ty;
        compiler.local_count = 0;
        compiler.scope_depth = 0;
        compiler.function = new_function();
        self.current_compiler = compiler;

        if ty != FunctionType::Script {
            (*(*self.current_compiler).function).name =
                copy_string(self.previous.lexeme.as_bytes());
        }

        let local =
            &mut (*self.current_compiler).locals[(*self.current_compiler).local_count as usize];
        (*self.current_compiler).local_count += 1;
        local.depth = 0;
        local.is_captured = false;
        if ty != FunctionType::Function {
            local.name.lexeme = "this";
        } else {
            local.name.lexeme = "";
        }
    }

    unsafe fn end_compiler(&mut self) -> *mut ObjFunction {
        self.emit_return();
        let function = (*self.current_compiler).function;

        #[cfg(feature = "debug_print_code")]
        {
            if !parser.had_error {
                let name = if (*function).name != ptr::null_mut() {
                    let name = &*(*function).name;
                    str::from_utf8_unchecked(slice::from_raw_parts(
                        name.chars,
                        name.length as usize,
                    ))
                } else {
                    "<script>"
                };
                disassemble_chunk(current_chunk(), name);
            }
        }

        self.current_compiler = (*self.current_compiler).enclosing;
        function
    }

    unsafe fn begin_scope(&mut self) {
        (*self.current_compiler).scope_depth += 1;
    }

    unsafe fn end_scope(&mut self) {
        (*self.current_compiler).scope_depth -= 1;

        while (*self.current_compiler).local_count > 0
            && (*self.current_compiler).locals[(*self.current_compiler).local_count as usize - 1]
                .depth
                > (*self.current_compiler).scope_depth
        {
            if (*self.current_compiler).locals[(*self.current_compiler).local_count as usize - 1]
                .is_captured
            {
                self.emit_byte(OpCode::CloseUpvalue as u8)
            } else {
                self.emit_byte(OpCode::Pop as u8);
            }
            (*self.current_compiler).local_count -= 1;
        }
    }

    pub unsafe fn mark_compiler_roots(&mut self) {
        let mut compiler = self.current_compiler;
        while compiler != ptr::null_mut() {
            mark_object((*compiler).function as *mut Obj);
            compiler = (*compiler).enclosing;
        }
    }

    unsafe fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = (*get_rule(self.previous.ty)).prefix;
        if prefix_rule.is_none() {
            self.error("Expect expression.");
            return;
        }

        let can_assign = precedence <= Precedence::Assignment;
        (prefix_rule.unwrap())(self, can_assign);

        while precedence <= (*get_rule(self.current.ty)).precedence {
            self.advance();
            let infix_rule = (*get_rule(self.previous.ty)).infix;
            (infix_rule.unwrap())(self, can_assign);
        }

        if can_assign && self.mtch(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }
    }

    unsafe fn identifier_constant(&mut self, name: &Token) -> u8 {
        self.make_constant(obj_val(copy_string(name.lexeme.as_bytes()) as *mut Obj))
    }

    unsafe fn resolve_local(&mut self, compiler: *mut Compiler, name: &Token) -> i32 {
        for i in (0..(*compiler).local_count).rev() {
            let local = &(*compiler).locals[i as usize];
            if identifiers_equal(name, &local.name) {
                if local.depth == -1 {
                    self.error("Can't read local variable in its own initializer.");
                }
                return i;
            }
        }
        return -1;
    }

    unsafe fn add_upvalue(&mut self, compiler: *mut Compiler, index: u8, is_local: bool) -> i32 {
        let upvalue_count = (*(*compiler).function).upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = &(*compiler).upvalues[i as usize];
            if upvalue.index == index && upvalue.is_local == is_local {
                return i;
            }
        }

        if upvalue_count as usize == UINT8_COUNT {
            self.error("Too many closure variables in scope.");
            return 0;
        }

        (*compiler).upvalues[upvalue_count as usize].is_local = is_local;
        (*compiler).upvalues[upvalue_count as usize].index = index;
        let res = (*(*compiler).function).upvalue_count;
        (*(*compiler).function).upvalue_count += 1;
        res
    }

    unsafe fn resolve_upvalue(&mut self, compiler: *mut Compiler, name: &Token) -> i32 {
        if (*compiler).enclosing == ptr::null_mut() {
            return -1;
        }
        let local = self.resolve_local((*compiler).enclosing, name);
        if local != -1 {
            (*(*compiler).enclosing).locals[local as usize].is_captured = true;
            return self.add_upvalue(compiler, local as u8, true);
        }

        let upvalue = self.resolve_upvalue((*compiler).enclosing, name);
        if upvalue != -1 {
            return self.add_upvalue(compiler, upvalue as u8, false);
        }

        -1
    }

    unsafe fn add_local(&mut self, name: Token<'source>) {
        if (*self.current_compiler).local_count as usize == UINT8_COUNT {
            self.error("Too many local variables in scope.");
            return;
        }

        let local =
            &mut (*self.current_compiler).locals[(*self.current_compiler).local_count as usize];
        (*self.current_compiler).local_count += 1;
        local.name = name;
        local.depth = -1;
        local.is_captured = false;
    }

    unsafe fn declare_variable(&mut self) {
        if (*self.current_compiler).scope_depth == 0 {
            return;
        }

        let name = self.previous;
        for i in (0..(*self.current_compiler).local_count).rev() {
            let local = &(*self.current_compiler).locals[i as usize];
            if local.depth != -1 && local.depth < (*self.current_compiler).scope_depth {
                break;
            }

            if identifiers_equal(&name, &local.name) {
                self.error("Already a variable with this name in this scope.");
            }
        }
        self.add_local(name);
    }

    unsafe fn parse_variable(&mut self, error_message: &str) -> u8 {
        self.consume(TokenType::Identifier, error_message);

        self.declare_variable();
        if (*self.current_compiler).scope_depth > 0 {
            return 0;
        }

        let previous: Token = self.previous;
        self.identifier_constant(&previous)
    }

    unsafe fn mark_initialized(&mut self) {
        if (*self.current_compiler).scope_depth == 0 {
            return;
        }
        (*self.current_compiler).locals[(*self.current_compiler).local_count as usize - 1].depth =
            (*self.current_compiler).scope_depth;
    }

    unsafe fn define_variable(&mut self, global: u8) {
        if (*self.current_compiler).scope_depth > 0 {
            self.mark_initialized();
            return; // runtime NOP, keep initializer temporary on top of the stack
        }

        self.emit_bytes(OpCode::DefineGlobal as u8, global);
    }

    unsafe fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error("Can't have more than 255 arguments.");
                }
                arg_count += 1;
                if !self.mtch(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    unsafe fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    unsafe fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    unsafe fn function(&mut self, ty: FunctionType) {
        let mut compiler: Compiler = mem::zeroed();
        self.init_compiler(&mut compiler, ty);
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::RightParen) {
            loop {
                (*(*self.current_compiler).function).arity += 1;
                if (*(*self.current_compiler).function).arity > 255 {
                    self.error_at_current("Can't have more than 255 parameters.");
                }
                let constant = self.parse_variable("Expect parameter name");
                self.define_variable(constant);

                if !self.mtch(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();

        let function = self.end_compiler();
        let shut_up_borrow_checker = self.make_constant(obj_val(function as *mut Obj));
        self.emit_bytes(OpCode::Closure as u8, shut_up_borrow_checker);

        for i in 0..(*function).upvalue_count {
            self.emit_byte(if compiler.upvalues[i as usize].is_local {
                1
            } else {
                0
            });
            self.emit_byte(compiler.upvalues[i as usize].index);
        }
    }

    unsafe fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expect method name.");
        let shut_up_borrow_checker: Token = self.previous;
        let constant = self.identifier_constant(&shut_up_borrow_checker);

        let mut ty = FunctionType::Method;
        if "init" == self.previous.lexeme {
            ty = FunctionType::Initializer;
        }
        self.function(ty);
        self.emit_bytes(OpCode::Method as u8, constant);
    }

    unsafe fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expect class name.");
        let class_name = self.previous;
        let shut_up_borrow_checker: Token = self.previous;
        let name_constant = self.identifier_constant(&shut_up_borrow_checker);
        self.declare_variable();

        self.emit_bytes(OpCode::Class as u8, name_constant);
        self.define_variable(name_constant);

        let mut class_compiler = ClassCompiler {
            enclosing: self.current_class,
            has_superclass: false,
        };
        self.current_class = &mut class_compiler;

        if self.mtch(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expect superclass name.");
            variable(self, false);

            if identifiers_equal(&class_name, &self.previous) {
                self.error("A class can't inherit from itself.");
            }

            self.begin_scope();
            let shut_up_borrow_checker = synthetic_token(self, "super");
            self.add_local(shut_up_borrow_checker);
            self.define_variable(0);

            named_variable(self, class_name, false);
            self.emit_byte(OpCode::Inherit as u8);
            class_compiler.has_superclass = true;
        }

        named_variable(self, class_name, false);
        self.consume(TokenType::LeftBrace, "Expect '{' before class body.");
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.method();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after class body.");
        self.emit_byte(OpCode::Pop as u8);

        if class_compiler.has_superclass {
            self.end_scope();
        }

        self.current_class = (*self.current_class).enclosing;
    }

    unsafe fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    unsafe fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.mtch(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Nil as u8);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    unsafe fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop as u8);
    }

    unsafe fn for_statement(&mut self) {
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.mtch(TokenType::Semicolon) {
        } else if self.mtch(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = (*self.current_chunk()).count;
        let mut exit_jump = None;
        if !self.mtch(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse as u8));
            self.emit_byte(OpCode::Pop as u8);
        }

        if !self.mtch(TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump as u8);
            let increment_start = (*self.current_chunk()).count;
            self.expression();
            self.emit_byte(OpCode::Pop as u8);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(OpCode::Pop as u8);
        }

        self.end_scope();
    }

    unsafe fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse as u8);

        self.emit_byte(OpCode::Pop as u8);
        self.statement();
        let else_jump = self.emit_jump(OpCode::Jump as u8);

        self.patch_jump(then_jump);

        self.emit_byte(OpCode::Pop as u8);
        if self.mtch(TokenType::Else) {
            self.statement()
        }
        self.patch_jump(else_jump);
    }

    unsafe fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print as u8);
    }

    unsafe fn return_statement(&mut self) {
        if (*self.current_compiler).ty == FunctionType::Script {
            self.error("Can't return from top-level code.");
        }

        if self.mtch(TokenType::Semicolon) {
            self.emit_return();
        } else {
            if (*self.current_compiler).ty == FunctionType::Initializer {
                self.error("Can't return a value from an initializer.");
            }

            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.emit_byte(OpCode::Return as u8);
        }
    }

    unsafe fn while_statement(&mut self) {
        let loop_start = (*self.current_chunk()).count;

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        self.emit_byte(OpCode::Pop as u8);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::Pop as u8);
    }

    unsafe fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.ty != TokenType::Eof {
            if self.previous.ty == TokenType::Semicolon {
                return;
            }
            match self.current.ty {
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

            self.advance();
        }
    }

    unsafe fn declaration(&mut self) {
        if self.mtch(TokenType::Class) {
            self.class_declaration();
        } else if self.mtch(TokenType::Fun) {
            self.fun_declaration();
        } else if self.mtch(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    unsafe fn statement(&mut self) {
        if self.mtch(TokenType::Print) {
            self.print_statement();
        } else if self.mtch(TokenType::For) {
            self.for_statement();
        } else if self.mtch(TokenType::If) {
            self.if_statement();
        } else if self.mtch(TokenType::Return) {
            self.return_statement();
        } else if self.mtch(TokenType::While) {
            self.while_statement();
        } else if self.mtch(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }
}

pub unsafe fn compile(source: &str) -> *mut ObjFunction {
    let mut scanner = Scanner::new(source);

    let uninit_error_msg = "INTERNAL COMPILER ERROR: Uninitialized token.";
    let mut parser = Parser {
        current: scanner.error_token(uninit_error_msg),
        previous: scanner.error_token(uninit_error_msg),
        had_error: false,
        panic_mode: false,
        current_compiler: ptr::null_mut(),
        current_class: ptr::null_mut(),
        scanner: &mut scanner,
    };

    gc_track_parser(mem::transmute(&mut parser));

    let mut compiler: Compiler = mem::zeroed();
    parser.init_compiler(&mut compiler, FunctionType::Script);

    parser.advance();

    while !parser.mtch(TokenType::Eof) {
        parser.declaration();
    }

    let function = parser.end_compiler();
    let result = if parser.had_error {
        ptr::null_mut()
    } else {
        function
    };

    gc_untrack_parser(mem::transmute(&mut parser));

    result
}

unsafe fn binary(parser: &mut Parser, _can_assign: bool) {
    let operator_type = parser.previous.ty;
    let rule = get_rule(operator_type);
    parser.parse_precedence(mem::transmute((*rule).precedence as u8 + 1));

    match operator_type {
        TokenType::BangEqual => parser.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8),
        TokenType::EqualEqual => parser.emit_byte(OpCode::Equal as u8),
        TokenType::Greater => parser.emit_byte(OpCode::Greater as u8),
        TokenType::GreaterEqual => parser.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
        TokenType::Less => parser.emit_byte(OpCode::Less as u8),
        TokenType::LessEqual => parser.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
        TokenType::Plus => parser.emit_byte(OpCode::Add as u8),
        TokenType::Minus => parser.emit_byte(OpCode::Subtract as u8),
        TokenType::Star => parser.emit_byte(OpCode::Multiply as u8),
        TokenType::Slash => parser.emit_byte(OpCode::Divide as u8),
        _ => return, // unreachable
    }
}

unsafe fn call(parser: &mut Parser, _can_assign: bool) {
    let arg_count = parser.argument_list();
    parser.emit_bytes(OpCode::Call as u8, arg_count);
}

unsafe fn dot(parser: &mut Parser, can_assign: bool) {
    parser.consume(TokenType::Identifier, "Expect property name after '.'.");
    let shut_up_borrow_checker = parser.previous;
    let name = parser.identifier_constant(&shut_up_borrow_checker);

    if can_assign && parser.mtch(TokenType::Equal) {
        parser.expression();
        parser.emit_bytes(OpCode::SetProperty as u8, name);
    } else if parser.mtch(TokenType::LeftParen) {
        let arg_count = parser.argument_list();
        parser.emit_bytes(OpCode::Invoke as u8, name);
        parser.emit_byte(arg_count);
    } else {
        parser.emit_bytes(OpCode::GetProperty as u8, name);
    }
}

unsafe fn literal(parser: &mut Parser, _can_assign: bool) {
    match parser.previous.ty {
        TokenType::False => parser.emit_byte(OpCode::False as u8),
        TokenType::Nil => parser.emit_byte(OpCode::Nil as u8),
        TokenType::True => parser.emit_byte(OpCode::True as u8),
        _ => return, // unreachable
    }
}

unsafe fn grouping(parser: &mut Parser, _can_assign: bool) {
    parser.expression();
    parser.consume(TokenType::RightParen, "Expect ')' after expression.");
}

unsafe fn number(parser: &mut Parser, _can_assign: bool) {
    let value: f32 = parser
        .previous
        .lexeme
        .parse()
        .expect("Token should be parseable as 32-bit floating point number.");
    parser.emit_constant(number_val(value as f64));
}

unsafe fn or(parser: &mut Parser, _can_assign: bool) {
    let else_jump = parser.emit_jump(OpCode::JumpIfFalse as u8);
    let end_jump = parser.emit_jump(OpCode::Jump as u8);

    parser.patch_jump(else_jump);
    parser.emit_byte(OpCode::Pop as u8);

    parser.parse_precedence(Precedence::Or);
    parser.patch_jump(end_jump);
}

unsafe fn string(parser: &mut Parser, _can_assign: bool) {
    let lexeme_len = parser.previous.lexeme.len();
    let contents_slice = &parser.previous.lexeme.as_bytes()[1..lexeme_len - 1];
    parser.emit_constant(obj_val(copy_string(contents_slice) as *mut Obj));
}

unsafe fn list(parser: &mut Parser, _can_assign: bool) {
    let mut count = 0;
    if !parser.check(TokenType::RightBracket) {
        loop {
            parser.expression();
            if count == 255 {
                parser.error("Can't have more than 255 elements in list initializer.");
            }
            count += 1;
            if !parser.mtch(TokenType::Comma) {
                break;
            }
        }
    }
    parser.consume(
        TokenType::RightBracket,
        "Expect ']' after list initializer.",
    );
    parser.emit_bytes(OpCode::List as u8, count as u8);
}

unsafe fn index(parser: &mut Parser, can_assign: bool) {
    parser.expression();
    parser.consume(TokenType::RightBracket, "Expect ']' after index operator.");

    if can_assign && parser.mtch(TokenType::Equal) {
        parser.expression();
        unimplemented!("assign to []");
    } else {
        parser.emit_byte(OpCode::Index as u8);
    }
}

unsafe fn named_variable(parser: &mut Parser, name: Token, can_assign: bool) {
    let (get_op, set_op);
    let mut arg = parser.resolve_local(parser.current_compiler, &name);
    if arg != -1 {
        get_op = OpCode::GetLocal;
        set_op = OpCode::SetLocal;
    } else if {
        arg = parser.resolve_upvalue(parser.current_compiler, &name);
        arg
    } != -1
    {
        get_op = OpCode::GetUpvalue;
        set_op = OpCode::SetUpvalue;
    } else {
        arg = parser.identifier_constant(&name).into();
        get_op = OpCode::GetGlobal;
        set_op = OpCode::SetGlobal;
    }
    let arg = arg as u8;

    if can_assign && parser.mtch(TokenType::Equal) {
        parser.expression();
        parser.emit_bytes(set_op as u8, arg);
    } else {
        parser.emit_bytes(get_op as u8, arg);
    }
}

unsafe fn variable(parser: &mut Parser, can_assign: bool) {
    named_variable(parser, parser.previous, can_assign);
}

unsafe fn synthetic_token(parser: &mut Parser, text: &'static str) -> Token<'static> {
    Token {
        ty: TokenType::Nil,
        lexeme: text,
        line: parser.previous.line,
    }
}

unsafe fn super_(parser: &mut Parser, _can_assign: bool) {
    if parser.current_class == ptr::null_mut() {
        parser.error("Can't use 'super' outside of a class.");
    } else if !(*parser.current_class).has_superclass {
        parser.error("Can't use 'super' in a class with no superclass.");
    }

    parser.consume(TokenType::Dot, "Expect '.' after 'super'.");
    parser.consume(TokenType::Identifier, "Expect superclass method name.");
    let shut_up_borrow_checker = parser.previous;
    let name = parser.identifier_constant(&shut_up_borrow_checker);

    let shut_up_borrow_checker = synthetic_token(parser, "this");
    named_variable(parser, shut_up_borrow_checker, false);
    if parser.mtch(TokenType::LeftParen) {
        let arg_count = parser.argument_list();
        let shut_up_borrow_checker = synthetic_token(parser, "super");
        named_variable(parser, shut_up_borrow_checker, false);
        parser.emit_bytes(OpCode::SuperInvoke as u8, name);
        parser.emit_byte(arg_count);
    } else {
        let shut_up_borrow_checker = synthetic_token(parser, "super");
        named_variable(parser, shut_up_borrow_checker, false);
        parser.emit_bytes(OpCode::GetSuper as u8, name);
    }
}

unsafe fn this(parser: &mut Parser, _can_assign: bool) {
    if parser.current_class == ptr::null_mut() {
        parser.error("Can't use 'this' outside of a class.");
        return;
    }

    variable(parser, false);
}

unsafe fn unary(parser: &mut Parser, _can_assign: bool) {
    let ty = parser.previous.ty;

    parser.parse_precedence(Precedence::Unary);

    match ty {
        TokenType::Bang => parser.emit_byte(OpCode::Not as u8),
        TokenType::Minus => parser.emit_byte(OpCode::Negate as u8),
        _ => return, // unreachable
    }
}

unsafe fn and(parser: &mut Parser, _can_assign: bool) {
    let end_jump = parser.emit_jump(OpCode::JumpIfFalse as u8);

    parser.emit_byte(OpCode::Pop as u8);
    parser.parse_precedence(Precedence::And);

    parser.patch_jump(end_jump);
}

const TOKEN_COUNT: usize = 42;
#[allow(non_upper_case_globals)]
static rules: [ParseRule; TOKEN_COUNT] = unsafe {
    let mut data = [ParseRule {
        prefix: None,
        infix: None,
        precedence: Precedence::None,
    }; TOKEN_COUNT];
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
    set_data!(LeftBracket, Some(list), Some(index), Call);
    set_data!(RightBracket, None, None, None);
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
    set_data!(Super, Some(super_), None, None);
    set_data!(This, Some(this), None, None);
    set_data!(True, Some(literal), None, None);
    set_data!(Var, None, None, None);
    set_data!(While, None, None, None);
    set_data!(Error, None, None, None);
    set_data!(Eof, None, None, None);
    mem::transmute(data)
};

unsafe fn identifiers_equal(a: &Token, b: &Token) -> bool {
    a.lexeme == b.lexeme
}

unsafe fn get_rule(ty: TokenType) -> *const ParseRule {
    &rules[ty as u8 as usize]
}
