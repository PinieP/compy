mod tokens;
use std::intrinsics::unreachable;

use crate::compile_target::Span;
use crate::frontend::lexer;
use lexer::{DelimiterKind, KeywordKind, OperatorKind, Token, TokenKind};

mod ast;
use ast::*;
use DelimiterKind::*;
use KeywordKind::*;
use OperatorKind::*;
use ParseErrKind::*;
use TokenKind::*;

#[derive(Debug, Clone)]
pub struct ParseErr {
    pub kind: ParseErrKind,
    pub span: Span,
    pub producer: Option<Box<ParseErr>>,
}
impl ParseErr {
    fn unexpected_token(token: &Token) -> Self {
        ParseErr {
            kind: UnexpectedToken(token.kind),
            span: token.span.clone(),
            producer: None,
        }
    }
    fn expected_token(kind: TokenKind, token: &Token) -> Self {
        ParseErr {
            kind: ExpectedToken {
                got: token.kind,
                expected: kind,
            },
            span: token.span.clone(),
            producer: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseErrKind {
    ExpectedToken { got: TokenKind, expected: TokenKind },
    UnexpectedToken(TokenKind),
    ExpectedKeyword,
}

type ParseResult<T> = Result<T, ParseErr>;

impl ast::Literal {
    fn from_lex_literal(lex_lit: lexer::LiteralKind) -> Self {
        Self {
            kind: match lex_lit {
                lexer::LiteralKind::Integer => ast::LiteralKind::Integer,
                lexer::LiteralKind::String => ast::LiteralKind::String,
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ParsePositon {
    pos: usize,
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    head: ParsePositon,
}

impl<'a> Parser<'a> {
    fn bump(&mut self) -> &Token {
        &self.tokens[self.head.pos]
    }
    fn first(&self) -> &Token {
        &self.tokens[self.head.pos + 1]
    }
    fn nth(&self, n: usize) -> &Token {
        &self.tokens[self.head.pos + n]
    }
}

enum TrailingMode {
    Required,
    Allowed,
    Forbidden,
}
impl DelimiterKind {
    fn others(&self) -> (DelimiterKind, DelimiterKind) {
        match self {
            Paren => (Brace, Bracket),
            Brace => (Paren, Bracket),
            Bracket => (Paren, Brace),
        }
    }
}

impl<'a> Parser<'a> {
    // parse
    fn parse_module(&mut self) -> ParseResult<Module> {
        let mut decls = Vec::new();
        let span_begin = self.first().span.begin;
        loop {
            let token = self.first();
            match token.kind {
                Eof => {
                    break Ok(Module {
                        span: Span {
                            file_id: token.span.file_id,
                            begin: span_begin,
                            end: token.span.end,
                        },
                        declarations: decls,
                    })
                }
                _ => decls.push(self.parse_declaration()?),
            }
        }
    }

    fn parse_declaration(&mut self) -> ParseResult<Declaration> {
        let token = self.first();
        match token.kind {
            Keyword(kind) => Ok(match kind {
                Fnc => Declaration::Function(Box::new(self.parse_function()?)),
                Let => Declaration::Variable(Box::new(self.parse_let()?)),
                Var => Declaration::Variable(Box::new(self.parse_var()?)),
            }),
            _ => Err(ParseErr {
                kind: ExpectedKeyword,
                span: token.span.clone(),
                producer: None,
            }),
        }
    }

    fn parse_variable_declaration(
        &mut self,
        keyword_kind: KeywordKind,
        var_decl_kind: VariableDeclarationKind,
    ) -> ParseResult<VariableDeclaration> {
        let begin_span = self.first().span.clone();
        self.expect_token(Keyword(keyword_kind))?;
        let identifier = self.parse_identifier()?;
        self.expect_token(Operator(Equal))?;
        let expression = self.parse_expression()?;
        Ok(VariableDeclaration {
            identifier,
            expression: Box::new(expression),
            kind: var_decl_kind,
            span: Span {
                begin: begin_span.begin,
                end: self.first().span.begin,
                file_id: begin_span.file_id,
            },
        })
    }
    fn parse_let(&mut self) -> ParseResult<VariableDeclaration> {
        self.parse_variable_declaration(Let, VariableDeclarationKind::Let)
    }
    fn parse_var(&mut self) -> ParseResult<VariableDeclaration> {
        self.parse_variable_declaration(Var, VariableDeclarationKind::Var)
    }

    fn parse_function(&mut self) -> ParseResult<Function> {
        let token = self.bump();
        debug_assert_eq!(token.kind, Keyword(Fnc));
        Ok(Function {
            identifier: self.parse_identifier()?,
            args: self.parse_sequence(Paren, Some(Comma), |this| this.parse_identifier())?,
            block: Box::new(self.parse_block()?),
        })
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        let begin_span = self.first().span.clone();
        Ok(Block {
            statements: self.parse_sequence(Brace, None, |this| this.parse_statement())?,
            span: Span {
                begin: begin_span.begin,
                end: self.first().span.begin,
                file_id: begin_span.file_id,
            },
        })
    }
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        let token = self.first();
        Ok(match token.kind {
            Keyword(_) => Statement::Declaration(Box::new(self.parse_declaration()?)),
            OpenDelimiter(Brace) => Statement::Block(Box::new(self.parse_block()?)),
            _ => Statement::Expression(Box::new(self.parse_expression()?)),
        })
    }

    fn parse_expression(&mut self) -> ParseResult<Expression> {
        let token = self.first();
        let kind = match token.kind {
            OpenDelimiter(kind) => match kind {
                Paren => {
                    self.bump();
                    let expr = self.parse_expression()?;
                    self.expect_token(OpenDelimiter(Paren))?;
                    return Ok(expr);
                }
                Bracket => todo!(),
                _ => return Err(ParseErr::unexpected_token(token)),
            },
            Literal(k) => ExpressionKind::Literal(Box::new(ast::Literal::from_lex_literal(k))),
            Operator(op_kind) if op_kind.is_valid_unary() => {
                let unop_kind = match op_kind {
                    Plus => UnopKind::Plus,
                    Minus => UnopKind::Minus,
                    _ => panic!(),
                };
                let expr = self.parse_expression()?;
                ExpressionKind::Unop(Box::new(Unop {
                    kind: unop_kind,
                    expr,
                }))
            }
            _ => return Err(ParseErr::unexpected_token(token)),
        };
        todo!()
    }

    fn parse_unop(&mut self) -> ParseResult<Unop> {
        let token = self.bump();
        let kind = match token.kind {
            Operator(kind) => match kind {
                Plus => UnopKind::Plus,
                Minus => UnopKind::Minus,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };
        let expr = self.parse_expression()?;
        Ok(Unop {
            kind,
            expr: Box::new(expr),
        })
    }

    fn try_parse<T>(
        &mut self,
        mut parse_fn: impl FnMut(&mut Parser) -> ParseResult<T>,
    ) -> ParseResult<T> {
        let head = self.head;
        let res = parse_fn(self);
        if let Err(_) = res {
            self.head = head;
        }
        res
    }
    fn expect_token(&mut self, kind: TokenKind) -> ParseResult<&Token> {
        let token = self.bump();
        match token.kind {
            got if got == kind => Ok(token),
            got => Err(ParseErr {
                kind: ParseErrKind::ExpectedToken {
                    got,
                    expected: kind,
                },
                span: token.span.clone(),
                producer: None,
            }),
        }
    }

    fn parse_identifier(&mut self) -> ParseResult<ast::Identifier> {
        let token = self.expect_token(Identifier)?;
        Ok(ast::Identifier {
            span: token.span.clone(),
        })
    }

    fn parse_sequence<ElemT>(
        &mut self,
        kind: DelimiterKind,
        sep: Option<TokenKind>,
        mut inner_parse: impl FnMut(&mut Parser) -> ParseResult<ElemT>,
    ) -> ParseResult<Vec<ElemT>> {
        self.expect_token(OpenDelimiter(kind))?;
        let mut seq = Vec::new();
        let mut previous_was_sep = true;
        loop {
            let token = self.first();
            match token.kind {
                CloseDelimiter(delim_k) if delim_k == kind => break Ok(seq),
                k if sep == Some(k) => {
                    if previous_was_sep {
                        return Err(ParseErr::unexpected_token(token));
                    }
                    self.bump();
                    previous_was_sep = true;
                }
                _ => {
                    if !previous_was_sep {
                        return Err(ParseErr::expected_token(sep.unwrap(), token));
                    }
                    seq.push(inner_parse(self)?);
                    if sep.is_some() {
                        previous_was_sep = false;
                    }
                }
            };
        }
    }
}

impl OperatorKind {
    fn is_valid_unary(&self) -> bool {
        matches!(self, Plus | Minus)
    }
}
