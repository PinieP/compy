use crate::compile_target::{FileId, FilePos, SourceFile, Span};
use std::str::Chars;

use DelimiterKind::*;
use KeywordKind::*;
use OperatorKind::*;
use TokenKind::*;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenKind {
    NewLine,
    Identifier,
    Keyword(KeywordKind),
    Literal(LiteralKind),
    Semicolon,
    Colon,
    Comma,
    OpenDelimiter(DelimiterKind),
    CloseDelimiter(DelimiterKind),
    Operator(OperatorKind),
    Eof,
}
impl TokenKind {
    fn identifier_to_keyword(self, text: &str) -> TokenKind {
        match self {
            Identifier => Keyword(match dbg!(text) {
                "fnc" => Fnc,
                "let" => Let,
                "var" => Var,
                _ => return self,
            }),
            _ => self,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum OperatorKind {
    Assign,

    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,

    LogicalAnd,
    BitwiseAnd,
    InplaceBitwiseAnd,

    LogicalOr,
    BitwiseOr,
    InplaceBitwiseOr,

    BitwiseXor,
    InplaceBitwiseXor,

    Dot,
    Plus,
    InplacePlus,
    Minus,
    InplaceMinus,
    Multiply,
    InplaceMultiply,
    Divide,
    InplaceDivide,
    Modulus,
    InplaceModulus,

    Not,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum LiteralKind {
    Integer,
    String,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum DelimiterKind {
    Paren,
    Brace,
    Bracket,
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum KeywordKind {
    Fnc,
    Let,
    Var,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

fn is_valid_identifier_start(c: char) -> bool {
    matches!( c, 'a'..='z' | 'A'..='Z' | '_' )
}

#[derive(Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    file_id: FileId,
    start_len: u64,
}

const EOF_CHAR: char = '\0';

#[derive(Clone, Debug)]
pub struct LexErr {
    pub span: Span,
    pub kind: LexErrKind,
}
#[derive(Clone, Debug)]
pub enum LexErrKind {
    UnknownToken,
    OpenQuotes,
    OpenBlockComment,
}

impl<'a> Cursor<'a> {
    pub fn from_file(file: &'a SourceFile, file_id: FileId) -> Self {
        Cursor {
            chars: file.text.chars(),
            file_id: file_id,
            start_len: file.text.len() as u64,
        }
    }
    pub fn from_str(text: &'a str) -> Self {
        Cursor {
            chars: text.chars(),
            file_id: FileId::default(),
            start_len: text.len() as u64,
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

    fn eat_line_comment(&mut self) {
        self.bump();
        self.eat_while(|c| c != '\n');
    }

    fn eat_block_comment(&mut self) -> bool {
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
                        return true;
                    }
                    depth -= 1;
                }
                _ => (),
            }
        }
        false
    }
    fn eat_whitespace(&mut self) {
        self.eat_while(char::is_whitespace)
    }
    fn identifier_or_keyword(&mut self) -> TokenKind {
        self.eat_while(|c| matches!(c, 'a'..='z'|'A'..='Z'|'_'));
        Identifier
    }

    fn numeric_literal(&mut self) -> TokenKind {
        self.eat_while(|c| matches!(c, '0'..='9' | '_'));
        Literal(LiteralKind::Integer)
    }

    fn file_id(&self) -> FileId {
        self.file_id
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

    fn advance_token(&mut self) -> Result<Token, LexErr> {
        let mut remaining_str;

        let token_kind: Result<TokenKind, LexErrKind> = loop {
            remaining_str = self.chars.as_str();
            let first_char = self.bump().unwrap_or(EOF_CHAR);
            break Ok(match first_char {
                '/' => match self.first() {
                    '/' => {
                        self.eat_line_comment();
                        continue;
                    }
                    '*' => {
                        if !self.eat_block_comment() {
                            break Err(LexErrKind::OpenBlockComment);
                        }
                        continue;
                    }
                    '=' => {
                        self.bump();
                        Operator(InplaceDivide)
                    }
                    _ => Operator(Divide),
                },
                c if c.is_whitespace() => {
                    self.eat_whitespace();
                    continue;
                }

                c if is_valid_identifier_start(c) => self.identifier_or_keyword(),

                '0'..='9' => self.numeric_literal(),

                EOF_CHAR => Eof,
                '\n' => NewLine,
                ';' => Semicolon,
                ':' => Colon,
                ',' => Comma,
                '(' => OpenDelimiter(Paren),
                ')' => CloseDelimiter(Paren),
                '{' => OpenDelimiter(Brace),
                '}' => CloseDelimiter(Brace),
                '[' => OpenDelimiter(Bracket),
                ']' => CloseDelimiter(Bracket),

                '.' => Operator(Dot),
                '!' => match self.first() {
                    '=' => {
                        self.bump();
                        Operator(NotEqual)
                    }
                    _ => Operator(Not),
                },
                '=' => match self.first() {
                    '=' => {
                        self.bump();
                        Operator(Equal)
                    }
                    _ => Operator(Assign),
                },
                '<' => match self.first() {
                    '=' => {
                        self.bump();
                        Operator(LessEqual)
                    }
                    _ => Operator(LessThan),
                },
                '>' => match self.first() {
                    '=' => {
                        self.bump();
                        Operator(GreaterEqual)
                    }
                    _ => Operator(GreaterThan),
                },
                '+' => match self.first() {
                    '=' => {
                        self.bump();
                        Operator(InplacePlus)
                    }
                    _ => Operator(Plus),
                },
                '-' => match self.first() {
                    '=' => {
                        self.bump();
                        Operator(InplaceMinus)
                    }
                    _ => Operator(Minus),
                },
                '*' => match self.first() {
                    '=' => {
                        self.bump();
                        Operator(InplaceMultiply)
                    }
                    _ => Operator(Multiply),
                },
                '%' => match self.first() {
                    '=' => {
                        self.bump();
                        Operator(InplaceModulus)
                    }
                    _ => Operator(Modulus),
                },
                '&' => match self.first() {
                    '&' => {
                        self.bump();
                        Operator(LogicalAnd)
                    }
                    '=' => {
                        self.bump();
                        Operator(InplaceBitwiseAnd)
                    }
                    _ => Operator(BitwiseAnd),
                },
                '|' => match self.first() {
                    '|' => {
                        self.bump();
                        Operator(LogicalOr)
                    }
                    '=' => {
                        self.bump();
                        Operator(InplaceBitwiseOr)
                    }
                    _ => Operator(BitwiseOr),
                },
                '^' => match self.first() {
                    '=' => {
                        self.bump();
                        Operator(InplaceBitwiseXor)
                    }
                    _ => Operator(BitwiseXor),
                },

                '"' => {
                    if self.string_literal() {
                        Literal(LiteralKind::String)
                    } else {
                        break Err(LexErrKind::OpenQuotes);
                    }
                }

                _ => break Err(LexErrKind::UnknownToken),
            });
        };
        let begin = FilePos {
            index: self.start_len - remaining_str.len() as u64,
        };
        let end = FilePos {
            index: self.start_len - self.chars.as_str().len() as u64,
        };
        let text = &remaining_str[..(end.index - begin.index) as usize];

        let token_kind = token_kind.map(|kind| kind.identifier_to_keyword(text));

        let span = Span {
            file_id: self.file_id,
            begin,
            end,
        };
        token_kind
            .map_err(|kind| LexErr {
                span: span.clone(),
                kind,
            })
            .map(|kind| Token { kind, span })
    }

    pub fn into_vec(mut self) -> (Vec<Token>, Vec<LexErr>) {
        let mut oks = Vec::new();
        let mut errs = Vec::new();
        loop {
            match self.advance_token() {
                Ok(token) => {
                    if token.kind == Eof {
                        oks.push(token);
                        break;
                    }
                    oks.push(token);
                }
                Err(err) => {
                    errs.push(err);
                }
            }
        }
        (oks, errs)
    }
}

impl Iterator for Cursor<'_> {
    type Item = Result<Token, LexErr>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.advance_token() {
            Ok(token) if token.kind == Eof => None,
            r => Some(r),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn lex() {
        let text = "identifier,/*Ignore me*/ 432984\"string\"";
        let mut cursor = Cursor::from_str(text);
        println!("{:?}", cursor.clone().into_vec());
        assert_eq!(cursor.next().unwrap().unwrap().kind, Identifier);
        assert_eq!(cursor.next().unwrap().unwrap().kind, Comma);
        assert_eq!(
            cursor.next().unwrap().unwrap().kind,
            Literal(LiteralKind::Integer)
        );
        assert_eq!(
            cursor.next().unwrap().unwrap().kind,
            Literal(LiteralKind::String)
        );
    }
}
