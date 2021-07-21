use core::slice;
use std::{
    mem,
    ops::{Add, Sub},
    ptr,
};

pub struct Scanner {
    pub start: *const u8,
    pub current: *const u8,
    pub line: i32,
}

pub struct Token {
    pub ty: TokenType,
    pub start: *const u8,
    pub length: i32,
    pub line: i32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

#[allow(non_upper_case_globals)]
pub static mut scanner: Scanner = Scanner {
    start: ptr::null_mut(),
    current: ptr::null_mut(),
    line: 0,
};

pub unsafe fn init_scanner(source: &str) {
    scanner.start = source.as_ptr();
    scanner.current = source.as_ptr();
    scanner.line = 1;
}

unsafe fn is_alpha(c: u8) -> bool {
    let c = c as char;
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

unsafe fn is_digit(c: u8) -> bool {
    c as char >= '0' && c as char <= '9'
}

pub unsafe fn scan_token() -> Token {
    skip_whitespace();
    scanner.start = scanner.current;

    if is_at_end() {
        return make_token(TokenType::Eof);
    }

    let c = advance();

    if is_alpha(c) {
        return identifier();
    }

    if is_digit(c) {
        return number();
    }

    match c as char {
        '(' => return make_token(TokenType::LeftParen),
        ')' => return make_token(TokenType::RightParen),
        '{' => return make_token(TokenType::LeftBrace),
        '}' => return make_token(TokenType::RightBrace),
        ';' => return make_token(TokenType::Semicolon),
        ',' => return make_token(TokenType::Comma),
        '.' => return make_token(TokenType::Dot),
        '-' => return make_token(TokenType::Minus),
        '+' => return make_token(TokenType::Plus),
        '/' => return make_token(TokenType::Slash),
        '*' => return make_token(TokenType::Star),
        '!' => {
            return make_token(if mtch('=') {
                TokenType::BangEqual
            } else {
                TokenType::Bang
            })
        }
        '=' => {
            return make_token(if mtch('=') {
                TokenType::EqualEqual
            } else {
                TokenType::Equal
            })
        }
        '<' => {
            return make_token(if mtch('=') {
                TokenType::LessEqual
            } else {
                TokenType::Less
            })
        }
        '>' => {
            return make_token(if mtch('=') {
                TokenType::GreaterEqual
            } else {
                TokenType::Greater
            })
        }
        '"' => return string(),
        _ => {}
    }

    return error_token("Unexpected character.");
}

unsafe fn is_at_end() -> bool {
    return *scanner.current == 0;
}

unsafe fn advance() -> u8 {
    scanner.current = scanner.current.add(1);
    *scanner.current.offset(-1)
}

unsafe fn peek() -> u8 {
    *scanner.current
}

unsafe fn peek_next() -> u8 {
    if is_at_end() {
        return 0;
    }
    return *scanner.current.add(1);
}

unsafe fn mtch(expected: char) -> bool {
    if is_at_end() {
        return false;
    }
    if *scanner.current as char != expected {
        return false;
    }
    scanner.current = scanner.current.add(1);
    true
}

unsafe fn make_token(ty: TokenType) -> Token {
    Token {
        ty,
        start: scanner.start,
        length: scanner.current.sub(scanner.start as usize) as i32,
        line: scanner.line,
    }
}

unsafe fn error_token(message: &'static str) -> Token {
    Token {
        ty: TokenType::Error,
        start: message.as_ptr(),
        length: message.as_bytes().len() as i32,
        line: scanner.line,
    }
}

unsafe fn skip_whitespace() {
    loop {
        let c = peek();
        match c as char {
            ' ' | '\r' | '\t' => {
                advance();
            }
            '\n' => {
                scanner.line += 1;
                advance();
            }
            '/' => {
                if peek_next() as char == '/' {
                    while peek() as char != '\n' && !is_at_end() {
                        advance();
                    }
                } else {
                    return;
                }
            }
            _ => return,
        };
    }
}

unsafe fn check_keyword(start: i32, length: i32, rest: &'static str, ty: TokenType) -> TokenType {
    if scanner.current.sub(scanner.start as usize) == scanner.start.add(length as usize)
        && rest.as_bytes()
            == slice::from_raw_parts(scanner.start.add(start as usize), length as usize)
    {
        return ty;
    }
    return TokenType::Identifier;
}

unsafe fn identifier_type() -> TokenType {
    match *scanner.start as char {
        'a' => check_keyword(1, 2, "nd", TokenType::And),
        'c' => check_keyword(1, 4, "lass", TokenType::Class),
        'e' => check_keyword(1, 3, "lse", TokenType::Else),
        'f' if scanner.current.sub(scanner.start as usize) as usize > 1 => {
            match *scanner.start.add(1) as char {
                'a' => check_keyword(2, 3, "lse", TokenType::False),
                'o' => check_keyword(2, 1, "r", TokenType::For),
                'u' => check_keyword(2, 1, "n", TokenType::Fun),
                _ => TokenType::Identifier,
            }
        }
        'i' => check_keyword(1, 1, "f", TokenType::If),
        'n' => check_keyword(1, 2, "il", TokenType::Nil),
        'o' => check_keyword(1, 1, "r", TokenType::Or),
        'p' => check_keyword(1, 4, "rint", TokenType::Print),
        'r' => check_keyword(1, 5, "eturn", TokenType::Return),
        's' => check_keyword(1, 4, "uper", TokenType::Super),
        't' if scanner.current.sub(scanner.start as usize) as usize > 1 => {
            match *scanner.start.add(1) as char {
                'h' => check_keyword(2, 2, "is", TokenType::This),
                'r' => check_keyword(2, 2, "ue", TokenType::True),
                _ => TokenType::Identifier,
            }
        }
        'v' => check_keyword(1, 2, "ar", TokenType::Var),
        'w' => check_keyword(1, 4, "hile", TokenType::While),
        _ => TokenType::Identifier,
    }
}

unsafe fn identifier() -> Token {
    while is_alpha(peek()) || is_digit(peek()) {
        advance();
    }
    make_token(identifier_type())
}

unsafe fn number() -> Token {
    while is_digit(peek()) {
        advance();
    }

    if peek() as char == '.' && is_digit(peek_next()) {
        advance();

        while is_digit(peek()) {
            advance();
        }
    }

    make_token(TokenType::Number)
}

unsafe fn string() -> Token {
    while peek() as char != '"' && !is_at_end() {
        if peek() as char == '\n' {
            scanner.line += 1;
        }
        advance();
    }
    if is_at_end() {
        return error_token("Unterminated string.");
    }
    advance();
    make_token(TokenType::String)
}
