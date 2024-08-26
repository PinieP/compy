use std::str::Chars;

#[derive(PartialEq, Clone, Debug)]
enum TokenKind {
    LineComment,
    BlockComment,
    VerticalWhitespace,
    NewLine,
    Identifier,
    Literal,

    Semicolon,
    Comma,
    Dot,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,

    Equal,
    LessThan,
    GreaterThan,

    Plus,
    Minus,
    And,
    Or,
    Star,
    Slash,
    Arrow,
    Eof,

    Unknown,
}

#[derive(Clone)]
struct Token<'a> {
    text: &'a str,
    kind: TokenKind,
}
impl Token<'_> {
    pub fn new(text: &str, kind: TokenKind) -> Token {
        Token { text, kind }
    }
}

fn is_vertical_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\t')
}

struct Cursor<'a> {
    chars: Chars<'a>,
}

const EOF_CHAR: char = '\0';

use TokenKind::*;
impl<'a> Cursor<'a> {
    fn new(text: &'a str) -> Self {
        Cursor {
            chars: text.chars(),
        }
    }
    fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }
    fn second(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }
    fn third(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }
    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn eat_while(&mut self, mut pred: impl FnMut(char) -> bool) {
        while pred(self.first()) && !self.is_eof() {
            self.bump();
        }
    }

    fn line_comment(&mut self) -> TokenKind {
        self.bump();
        self.eat_while(|c| c != '\n');
        LineComment
    }

    fn block_comment(&mut self) -> TokenKind {
        self.bump();
        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            match c {
                '/' if self.first() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    self.bump();
                    if depth == 1 {
                        return BlockComment;
                    }
                    depth -= 1;
                }
                _ => (),
            }
        }
        Eof
    }

    fn numeric_literal(&mut self) -> bool {
        while let Some(c) = self.bump() {
            match c {
                '0'..='9' | '_' => {
                    self.bump();
                }
                _ => (),
            }
        }
        false
    }
    fn string_literal(&mut self) -> bool {
        while let Some(c) = self.bump() {
            match c {
                '"' => {
                    return true;
                }
                '\\' if self.first() == '\\' || self.first() == '"' => {
                    self.bump();
                }
                _ => (),
            }
        }
        false
    }
    fn advance_token(&mut self) -> Token<'a> {
        let remaining_str = self.chars.as_str();
        let first_char = match self.bump() {
            Some(c) => c,
            None => EOF_CHAR,
        };
        let token_kind = match first_char {
            '/' => match self.first() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => Slash,
            },
            c if is_vertical_whitespace(c) => {
                self.eat_while(is_vertical_whitespace);
                VerticalWhitespace
            }
            c @ '0'..='9' => todo!("{c}"),

            '\n' => NewLine,
            ';' => Semicolon,
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '=' => Equal,
            '<' => LessThan,
            '>' => GreaterThan,
            '+' => Plus,
            '&' => And,
            '|' => Or,
            '*' => Star,
            '-' => match self.first() {
                '>' => Arrow,
                _ => Minus,
            },

            '"' => todo!(),

            EOF_CHAR => Eof,

            _ => Unknown,
        };
        let size = remaining_str.len() - self.chars.as_str().len();
        Token::new(&remaining_str[..size], token_kind)
    }
}

fn tokenize(text: &str) -> impl Iterator<Item = Token> {
    let mut cursor = Cursor::new(text);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}
