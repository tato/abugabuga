use std::str;
// TODO: tests\fixtures\functions5.lox

#[derive(Debug, Clone, Copy)]
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
    LeftBracket,
    RightBracket,
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

pub struct Scanner {
    source: Vec<u8>, // TODO: not owned
    start: usize,
    current: usize,
    line: i32,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        let mut source = source.as_bytes().to_owned();
        source.push(0); // TODO: do i want sentinel?
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start += self.current;
        self.current = 0;

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        if is_alpha(c) {
            return self.identifier();
        }

        if is_digit(c) {
            return self.number();
        }

        match c as char {
            '(' => return self.make_token(TokenType::LeftParen),
            ')' => return self.make_token(TokenType::RightParen),
            '{' => return self.make_token(TokenType::LeftBrace),
            '}' => return self.make_token(TokenType::RightBrace),
            '[' => return self.make_token(TokenType::LeftBracket),
            ']' => return self.make_token(TokenType::RightBracket),
            ';' => return self.make_token(TokenType::Semicolon),
            ',' => return self.make_token(TokenType::Comma),
            '.' => return self.make_token(TokenType::Dot),
            '-' => return self.make_token(TokenType::Minus),
            '+' => return self.make_token(TokenType::Plus),
            '/' => return self.make_token(TokenType::Slash),
            '*' => return self.make_token(TokenType::Star),
            '!' => {
                let shut_up_borrow_checker = self.mtch('=');
                return self.make_token(if shut_up_borrow_checker {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                })
            }
            '=' => {
                let shut_up_borrow_checker = self.mtch('=');
                return self.make_token(if shut_up_borrow_checker {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                })
            }
            '<' => {
                let shut_up_borrow_checker = self.mtch('=');
                return self.make_token(if shut_up_borrow_checker {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                })
            }
            '>' => {
                let shut_up_borrow_checker = self.mtch('=');
                return self.make_token(if shut_up_borrow_checker {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                })
            }
            '"' => return self.string(),
            _ => {}
        }

        return self.error_token("Unexpected character.");
    }

    fn is_at_end(&self) -> bool {
        return self.source.len() - self.start == self.current;
    }

    fn advance(&mut self) -> u8 {
        self.current += 1;
        self.source[self.start + self.current - 1]
    }

    fn peek(&self) -> u8 {
        self.source[self.start + self.current]
    }

    fn peek_next(&self) -> u8 {
        if self.is_at_end() {
            return 0;
        }
        // TODO: almost certain this is off by one. when "at end", start[current_length] is outside the slice, so that + 1...
        // LATER: actually probably "fixed" by the sentinel i added. is that what i want?
        self.source[self.start + self.current + 1]
    }

    fn mtch(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() as char != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn make_token(&self, ty: TokenType) -> Token {
        Token {
            ty,
            // TODO: slice instead of start/length
            start: self.source[self.start..].as_ptr(),
            length: self.current as i32,
            line: self.line,
        }
    }

    fn error_token(&self, message: &'static str) -> Token {
        Token {
            ty: TokenType::Error,
            start: message.as_ptr(),
            length: message.as_bytes().len() as i32,
            line: self.line,
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let c = self.peek() as char;
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() as char == '/' {
                        while self.peek() as char != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            };
        }
    }

    fn check_keyword(
        &self,
        start: i32,
        length: i32,
        rest: &'static str,
        ty: TokenType,
    ) -> TokenType {
        if self.current == (start + length) as usize
            && rest.as_bytes() == &self.source[self.start + start as usize..(start + length) as usize]
        {
            return ty;
        }
        return TokenType::Identifier;
    }

    fn identifier_type(&self) -> TokenType {
        match self.source[self.start] as char {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'f' if self.current > 1 => match self.source[self.start + 1] as char {
                'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                'o' => self.check_keyword(2, 1, "r", TokenType::For),
                'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                _ => TokenType::Identifier,
            },
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            't' if self.current > 1 => match self.source[self.start + 1] as char {
                'h' => self.check_keyword(2, 2, "is", TokenType::This),
                'r' => self.check_keyword(2, 2, "ue", TokenType::True),
                _ => TokenType::Identifier,
            },
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn identifier(&mut self) -> Token {
        while is_alpha(self.peek()) || is_digit(self.peek()) {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    fn number(&mut self) -> Token {
        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() as char == '.' && is_digit(self.peek_next()) {
            self.advance();

            while is_digit(self.peek()) {
                self.advance();
            }

            if self.mtch('e') {
                if !self.mtch('+') && !self.mtch('-') {
                    return self.error_token("Expect sign after 'e'.");
                }
                while is_digit(self.peek()) {
                    self.advance();
                }
            }
        }

        self.make_token(TokenType::Number)
    }

    fn string(&mut self) -> Token {
        while self.peek() as char != '"' && !self.is_at_end() {
            if self.peek() as char == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }
        self.advance();
        self.make_token(TokenType::String)
    }
}

fn is_alpha(c: u8) -> bool {
    let c = c as char;
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_digit(c: u8) -> bool {
    c as char >= '0' && c as char <= '9'
}
