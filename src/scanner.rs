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
    source: String,
    start: usize,
    current: usize,
    line: i32,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        let source = source.to_string();
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        // println!("++++ scan_token: {:?}", unsafe { str::from_utf8_unchecked(&self.source[self.start..]) });
        self.skip_whitespace();
        self.start = self.current;
        // self.current = 0;

        if self.is_at_end() {
            // println!("---- scan_token: Eof");
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        let result = match c {
            _ if is_alpha(c) => self.identifier(),
            _ if is_digit(c) => self.number(),
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            '[' => self.make_token(TokenType::LeftBracket),
            ']' => self.make_token(TokenType::RightBracket),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                let shut_up_borrow_checker = self.mtch('=');
                self.make_token(if shut_up_borrow_checker {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                })
            }
            '=' => {
                let shut_up_borrow_checker = self.mtch('=');
                self.make_token(if shut_up_borrow_checker {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                })
            }
            '<' => {
                let shut_up_borrow_checker = self.mtch('=');
                self.make_token(if shut_up_borrow_checker {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                })
            }
            '>' => {
                let shut_up_borrow_checker = self.mtch('=');
                self.make_token(if shut_up_borrow_checker {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                })
            }
            '"' => self.string(),
            _ => self.error_token("Unexpected character."),
        };

        // println!("---- scan_token: {:?}", result);
        result
    }

    fn is_at_end(&self) -> bool {
        return self.source.len() == self.current;
    }

    fn advance(&mut self) -> char {
        let result = self.source[self.current..].chars().next().unwrap();
        self.current += result.len_utf8();
        result
    }

    fn peek(&self) -> Option<char> {
        self.source[self.current..].chars().next()
    }

    fn peek_next(&self) -> Option<char> {
        self.source[self.current..].chars().nth(1)
    }

    fn mtch(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != Some(expected) {
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
            length: (self.current - self.start) as i32,
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
            let c = self.peek().unwrap_or('A');
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == Some('/') {
                        while self.peek() != Some('\n') && !self.is_at_end() {
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
        if (self.current - self.start) == (start + length) as usize
            && rest == &self.source[self.start + start as usize..(self.start as i32 + start + length) as usize]
        {
            return ty;
        }
        return TokenType::Identifier;
    }

    fn identifier_type(&self) -> TokenType {
        let current_1st_char = self.source[self.start..].chars().next().unwrap();
        match current_1st_char {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'f' if (self.current - self.start) > current_1st_char.len_utf8() => match self.source[self.start..].chars().nth(1).unwrap() {
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
            't' if (self.current - self.start) > current_1st_char.len_utf8() => match self.source[self.start..].chars().nth(1).unwrap() as char {
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
        while is_alpha(self.peek().unwrap_or('@')) || is_digit(self.peek().unwrap_or('@')) {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    fn number(&mut self) -> Token {
        while is_digit(self.peek().unwrap_or('@')) {
            self.advance();
        }

        if self.peek() == Some('.') && is_digit(self.peek_next().unwrap_or('@')) {
            self.advance();

            while is_digit(self.peek().unwrap_or('@')) {
                self.advance();
            }

            if self.mtch('e') {
                if !self.mtch('+') && !self.mtch('-') {
                    return self.error_token("Expect sign after 'e'.");
                }
                while is_digit(self.peek().unwrap_or('@')) {
                    self.advance();
                }
            }
        }

        self.make_token(TokenType::Number)
    }

    fn string(&mut self) -> Token {
        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') {
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

fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}
