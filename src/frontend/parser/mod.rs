mod tokens;
use crate::frontend::lexer::{KeywordKind, Token, TokenKind};

mod ast;
use ast::*;

pub struct Parser {
    tokens: Vec<Token>,
    head: usize,
}

impl Parser {
    fn bump(&mut self) -> &Token {
        &self.tokens[self.head]
    }
    fn first(&mut self) -> &Token {
        &self.tokens[self.head + 1]
    }
}

impl Parser {
    // parse
    fn parse_module(&mut self) -> Module {
        let mut decls = Vec::new();
        use KeywordKind::*;
        match self.bump().kind {
            TokenKind::Keyword(k) => match k {
                Function => ,
                Let => {}
                Var => {}
            },
            _ => todo!(),
        }
        todo!()
    }
    fn parse_declaration(&mut self) {}
    fn parse(&mut self) {
        let module = Module {
            declarations: Vec::new(),
            span: None,
        };
    }
    fn parse_list(&mut self, start : TokenKind, end : TokenKind) {}
}

macro_rules! parse_list {
    (parser: &mut Parser, start: TokenKin, end: TokenKind, ) => {
        
    };
}
