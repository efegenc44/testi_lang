use crate::{
    error::error::{ Res, simple_error },
    span::{ Spanned, Span } 
};

use super::token::Token::{ self, * };

pub struct Lexer {
    source_name: String,
    chars: Vec<char>,
    /// Absolute position 
    cursor: usize,  
    /// Line relative position
    row: usize, 
    col: usize,
}

impl Iterator for Lexer {
    type Item = Res<Spanned<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl Lexer {
    pub fn from_file(file_name: &str) -> std::io::Result<Self> {
        let file = std::fs::read_to_string(file_name)?;
        let mut chars: Vec<_> = file.chars().collect();
        chars.push('\0');
        Ok(Self { source_name: file_name.to_string(), chars, cursor: 0, col: 1, row: 1 })
    }

    fn peek(&self) -> char {
        self.chars[self.cursor]
    }

    fn advance(&mut self) {
        self.col    += 1;
        self.cursor += 1;
    }

    fn peek_is(&mut self, ch: char) -> bool {
        if self.peek() == ch {
            self.advance();
            true
        } else {
            false
        }
    }

    fn digit_sequence(&mut self) -> String {
        let mut text = String::new();
        while self.peek().is_ascii_digit() {
            text.push(self.peek());
            self.advance();
        }
        text
    }

    fn string(&mut self) -> Res<Token> {
        let start = self.col;
        let first_line = self.row;
        // Consume '"' and advance.
        self.advance();
        let string_start = self.cursor;
        while self.peek() != '"' && self.peek() != '\0' {
            if self.peek() == '\n' {
                self.row += 1;
                self.col  = 0; // self.advance() already increments col
            }
            self.advance();
        }

        if self.peek() == '\0' {
            return simple_error(
                "Unterminated string literal.", 
                Span::new(self.source_name.clone(), first_line, self.row, start, self.col)
            )
        }
        let range = string_start..self.cursor;
        self.advance(); // Consume closing '"' and advance.
        let string = self.chars[range].iter().collect();
        Ok(STRING(string))
    }

    fn number(&mut self) -> Token {
        let whole_part = self.digit_sequence();
        if self.peek() == '.' && 
           self.chars[self.cursor+1].is_ascii_digit() {
            self.advance();
            let fractional_part = self.digit_sequence();
            return FLOAT((whole_part + "." + &fractional_part).parse().unwrap())
        }
        return NATURAL(whole_part.parse().unwrap())
    }

    fn symbol(&mut self) -> Token {
        let start = self.cursor;
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let sym: String = self.chars[start..self.cursor].iter().collect();
        
        match sym.as_str() {
            "mod"      => KMOD,
            "and"      => KAND,
            "or"       => KOR,
            "is"       => KIS,
            "let"      => KLET,
            "import"   => KIMPORT,
            "fun"      => KFUN,
            "def"      => KDEF,
            "impl"     => KIMPL,
            "for"      => KFOR,
            "in"       => KIN,
            "while"    => KWHILE,
            "if"       => KIF,
            "ef"       => KEF,
            "then"     => KTHEN,
            "else"     => KELSE,
            "return"   => KRETURN,
            "continue" => KCONTINUE,
            "break"    => KBREAK,
            "end"      => KEND,
            "true"     => KTRUE,
            "false"    => KFALSE,
            "nothing"  => KNOTHING,
            _          => SYMBOL(sym)
        }
    }

    fn spanned(&self, token: Token, start: usize) -> Option<Res<Spanned<Token>>> {
        Some(Ok(Spanned::new(token, Span::new(self.source_name.clone(), self.row, self.row, start, self.col))))
    }
    
    fn next_token(&mut self) -> Option<Res<Spanned<Token>>> {
        let start = self.col;
        
        if self.cursor == self.chars.len() {
            return None
        }

        let ch = self.peek();

        if ch == '"' {
            let token = match self.string() {
                Ok(token) => token,
                Err(err)  => return Some(Err(err)),
            };
           return self.spanned(token, start);
        }

        if ch.is_ascii_digit() {
            let token = self.number();
            return self.spanned(token, start);
        }

        if ch.is_alphabetic() {
            let token = self.symbol();
            return self.spanned(token, start);
        }

        // In the next step, it is guaranteed that we consume the char
        // so we can basically advance here to return immediately
        self.advance();
        
        let token = match ch {
            '('  => LPAREN,
            ')'  => RPAREN,
            '['  => LSQUARE,
            ']'  => RSQUARE,
            ';'  => SEMICOLON,
            ':'  => COLON,
            '#'  => HASH,
            ','  => COMMA,
            '\\' => BACKSLASH,
            '.'  => if self.peek_is('.') {
                if self.peek_is('=') {
                    TWODOTEQUAL
                } else {
                    TWODOT
                }
            } else {
                DOT
            },
            '+'  => if self.peek_is('=') {
                PLUSEQUAL
            } else {
                PLUS
            },
            '*'  => if self.peek_is('=') {
                STAREQUAL
            } else {
                STAR
            },
            '-'  => if self.peek_is('=') {
                MINUSEQUAL
            } else {
                MINUS
            },
            '/'  => if self.peek_is('/') {
                while self.peek() != '\n' && self.peek() != '\0' {
                    self.advance()
                }
                return self.next_token()
            } else if self.peek_is('=') {
                SLASHEQUAL
            } else {
                SLASH
            },
            '='  => if self.peek_is('=') {
                DEQUAL
            } else {
                EQUAL
            },
            '!'  => if self.peek_is('=') {
                BANGEQUAL
            } else {
                BANG
            },
            '<' => if self.peek_is('=') {
                LESSEQUAL
            } else {
                LESS
            },
            '>' => if self.peek_is('=') {
                GREATEREQUAL
            } else {
                GREATER
            },
            '\0' => END,
            '\n' => {
                self.row += 1;
                self.col  = 1;
                return self.next_token()
            },
            '\r' |
            '\t' |
            ' ' => return self.next_token(),
            _   => return Some(simple_error(
                format!("Unknown start of token `{ch}`"),
                Span::new(self.source_name.clone(), self.row, self.row, start, self.col)
            ))
        };
        self.spanned(token, start)
    }
}