use crate::compile_target::{FileId, SourceFile, Span};
use std::str::Chars;

#[cfg(test)]
mod tests;

use DelimiterKind::*;
use KeywordKind::*;
use LiteralKind::*;
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
    Dot,
    OpenDelimiter(DelimiterKind),
    CloseDelimiter(DelimiterKind),
    Operator(OperatorKind),
    Eof,
}
impl TokenKind {
    fn identifier_to_keyword(self, text: &str) -> TokenKind {
        match self {
            Identifier => Keyword(match text {
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
    Equal,
    LessThan,
    GreaterThan,
    Plus,
    Minus,
    And,
    Or,
    Star,
    Slash,
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
    pub preceeded_by_whitespace: bool,
    pub span: Span,
}

fn is_valid_identifier_start(c: char) -> bool {
    matches!( c, 'a'..='z' | 'A'..='Z' | '_' )
}

#[derive(Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    file_id: Option<FileId>,
    start_len: u64,
}

const EOF_CHAR: char = '\0';

pub struct LexErr {
    pub span: Span,
    kind: LexErrKind,
}
pub enum LexErrKind {
    UnknownToken,
    OpenQuotes,
    OpenBlockComment,
}

impl<'a> Cursor<'a> {
    fn from_file(file: &'a SourceFile, file_id: FileId) -> Self {
        Cursor {
            chars: file.text.chars(),
            file_id: Some(file_id),
            start_len: file.text.len() as u64,
        }
    }
    fn from_str(text: &'a str) -> Self {
        Cursor {
            chars: text.chars(),
            file_id: None,
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
        self.file_id.unwrap_or(FileId::from_index(0))
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
        let remaining_str = self.chars.as_str();
        let first_char = self.bump().unwrap_or(EOF_CHAR);

        let mut preceeded_by_whitespace = false;
        let token_kind: Result<TokenKind, LexErrKind> = loop {
            break Ok(match first_char {
                '/' => match self.first() {
                    '/' => {
                        self.eat_line_comment();
                        preceeded_by_whitespace = true;
                        continue;
                    }
                    '*' => {
                        if !self.eat_block_comment() {
                            break Err(LexErrKind::OpenBlockComment);
                        }
                        preceeded_by_whitespace = true;
                        continue;
                    }
                    _ => Operator(Slash),
                },
                c if c.is_whitespace() => {
                    self.eat_whitespace();
                    preceeded_by_whitespace = true;
                    continue;
                }

                c if is_valid_identifier_start(c) => self.identifier_or_keyword(),

                '0'..='9' => self.numeric_literal(),

                EOF_CHAR => Eof,
                '\n' => NewLine,
                ';' => Semicolon,
                ':' => Colon,
                ',' => Comma,
                '.' => Dot,
                '(' => OpenDelimiter(Paren),
                ')' => CloseDelimiter(Paren),
                '{' => OpenDelimiter(Brace),
                '}' => CloseDelimiter(Brace),
                '[' => OpenDelimiter(Bracket),
                ']' => CloseDelimiter(Bracket),
                '=' => Operator(Equal),
                '<' => Operator(LessThan),
                '>' => Operator(GreaterThan),
                '+' => Operator(Plus),
                '&' => Operator(And),
                '|' => Operator(Or),
                '*' => Operator(Star),
                '-' => Operator(Minus),

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
        let begin = self.start_len - remaining_str.len() as u64;
        let end = self.start_len - self.chars.as_str().len() as u64;
        let text = &remaining_str[..(end - begin) as usize];

        let token_kind = token_kind.map(|kind| kind.identifier_to_keyword(text));

        let span = Span {
            file_id: self.file_id(),
            begin,
            end,
        };
        token_kind
            .map_err(|kind| LexErr {
                span: span.clone(),
                kind,
            })
            .map(|kind| Token {
                kind,
                preceeded_by_whitespace,
                span,
            })
    }

    pub fn into_vec(mut self) -> (Vec<Token>, Vec<LexErr>) {
        let mut oks = Vec::new();
        let mut errs = Vec::new();
        loop {
            match self.advance_token() {
                Ok(token) => {
                    if token.kind == Eof {
                        break;
                    }
                    oks.push(token)
                }
                Err(err) => errs.push(err),
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
