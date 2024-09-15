use super::lexer::{self, Cursor, DelimiterKind, KeywordKind, OperatorKind, Token, TokenKind};
use crate::session::Session;
use crate::source::{BytePos, FileId, Span};
use crate::util::IntegerWrapper;

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
    tail_pos: BytePos,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            head: ParsePositon { pos: 0 },
            tail_pos: BytePos::from_index(0),
        }
    }
    fn bump(&mut self) -> &Token {
        self.tail_pos = self.tokens[self.head.pos].span.end;
        let old_head = self.head.pos;
        self.head.pos += 1;
        &self.tokens[old_head]
    }
    fn peek(&self) -> &Token {
        &self.tokens[dbg!(self.head.pos.min(self.tokens.len() - 1))]
    }

    fn span_from(&self, span: &Span) -> Span {
        Span {
            begin: span.begin,
            end: self.tail_pos,
        }
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

impl Parser<'_> {
    // parse
    fn parse_module(&mut self) -> ParseResult<Module> {
        let mut decls = Vec::new();
        let begin_span = self.peek().span.clone();
        loop {
            let token = self.peek();
            match dbg!(token.kind) {
                Eof => {
                    break Ok(Module {
                        span: self.span_from(&begin_span),
                        declarations: decls,
                    })
                }
                _ => decls.push(self.parse_declaration()?),
            }
        }
    }

    fn parse_declaration(&mut self) -> ParseResult<Declaration> {
        let token = self.peek();
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
        let begin_span = self.peek().span.clone();
        self.expect_token(Keyword(keyword_kind))?;
        let identifier = self.parse_identifier()?;
        self.expect_token(Operator(Assign))?;
        let expression = self.parse_expression()?;
        self.expect_token(Semicolon)?;
        Ok(VariableDeclaration {
            identifier,
            expression: Box::new(expression),
            kind: var_decl_kind,
            span: self.span_from(&begin_span),
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
            body: match self.peek().kind {
                OpenDelimiter(Brace) => FunctionBody::Block(Box::new(self.parse_block()?)),
                _ => FunctionBody::Expression(Box::new(self.parse_expression()?)),
            },
        })
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        let begin_span = self.peek().span.clone();
        Ok(Block {
            statements: self.parse_sequence(Brace, None, |this| this.parse_statement())?,
            span: self.span_from(&begin_span),
        })
    }
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        let token = self.peek();
        Ok(match token.kind {
            Keyword(_) => Statement::Declaration(Box::new(self.parse_declaration()?)),
            OpenDelimiter(Brace) => Statement::Block(Box::new(self.parse_block()?)),
            _ => Statement::Expression(Box::new(self.parse_expression()?)),
        })
    }

    fn parse_prefix_unop(&mut self, kind: OperatorKind) -> ParseResult<ExpressionKind> {
        debug_assert!(matches!(self.peek().kind, Operator(op_kind) if op_kind.is_valid_unary()));
        self.bump();
        let unop_kind = match kind {
            Plus => UnopKind::Plus,
            Minus => UnopKind::Minus,
            _ => panic!(),
        };
        let expr = self.parse_expression()?;
        Ok(ExpressionKind::Unop(Box::new(Unop {
            kind: unop_kind,
            expr,
        })))
    }

    fn parse_atom(&mut self) -> ParseResult<Expression> {
        let token = self.peek().clone();
        let kind = match token.kind {
            OpenDelimiter(kind) => match kind {
                Paren => {
                    self.bump();
                    let expr = self.parse_expression()?;
                    if self.peek().kind == OpenDelimiter(Paren) {
                        expr.kind
                    } else {
                        return Err(ParseErr::expected_token(CloseDelimiter(Paren), &token));
                    }
                }
                Bracket => todo!(),
                _ => return Err(ParseErr::unexpected_token(&token)),
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
            _ => return Err(ParseErr::unexpected_token(&token)),
        };
        self.bump();

        Ok(Expression {
            kind,
            span: self.span_from(&token.span),
        })
    }

    fn parse_expression(&mut self) -> ParseResult<Expression> {
        let atom = self.parse_atom()?;
        match self.peek().kind {
            Operator(op_kind) => self.parse_binop(atom, op_kind),
            _ => Ok(atom),
        }
    }

    fn parse_binop(&mut self, lhs: Expression, op_kind: OperatorKind) -> ParseResult<Expression> {
        debug_assert!(matches!(self.peek().kind, Operator(_)));
        self.bump();
        let immediate_rhs = self.parse_atom()?;
        let rhs = match self.peek().kind {
            Operator(next_op_kind)
                if next_op_kind.binop_precedence() < op_kind.binop_precedence() =>
            {
                self.parse_binop(immediate_rhs, next_op_kind)?
            }
            _ => immediate_rhs,
        };

        Ok(Expression {
            span: self.span_from(&lhs.span),
            kind: ExpressionKind::Binop(Box::new(Binop {
                lhs,
                rhs,
                kind: BinopKind::from_operator_kind(op_kind),
            })),
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
            let token = self.peek();
            match token.kind {
                CloseDelimiter(delim_k) if delim_k == kind => {
                    self.bump();
                    break Ok(seq);
                }
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
    fn binop_precedence(&self) -> u32 {
        match self {
            Multiply | Divide | Modulus => 1,
            Plus | Minus => 2,
            GreaterThan | GreaterEqual | LessThan | LessEqual => 3,
            Equal | NotEqual => 4,
            BitwiseAnd => 5,
            BitwiseXor => 6,
            BitwiseOr => 7,
            LogicalAnd => 8,
            LogicalOr => 9,
            _ => panic!(),
        }
    }
}

impl BinopKind {
    pub fn from_operator_kind(kind: OperatorKind) -> Self {
        match kind {
            Assign => BinopKind::Assign,
            Equal => BinopKind::Equal,
            NotEqual => BinopKind::NotEqual,
            LessThan => BinopKind::LessThan,
            LessEqual => BinopKind::LessEqual,
            GreaterThan => BinopKind::GreaterThan,
            GreaterEqual => BinopKind::GreaterEqual,
            LogicalAnd => BinopKind::LogicalAnd,
            BitwiseAnd => BinopKind::BitwiseAnd,
            CompoundAssignBitwiseAnd => BinopKind::CompoundAssignBitwiseAnd,
            LogicalOr => BinopKind::LogicalOr,
            BitwiseOr => BinopKind::BitwiseOr,
            CompoundAssignBitwiseOr => BinopKind::CompoundAssignBitwiseOr,
            BitwiseXor => BinopKind::BitwiseXor,
            CompoundAssignBitwiseXor => BinopKind::CompoundAssignBitwiseXor,
            Plus => BinopKind::Plus,
            CompoundAssignPlus => BinopKind::CompoundAssignPlus,
            Minus => BinopKind::Minus,
            CompoundAssignMinus => BinopKind::CompoundAssignMinus,
            Multiply => BinopKind::Multiply,
            CompoundAssignMultiply => BinopKind::CompoundAssignMultiply,
            Divide => BinopKind::Divide,
            CompoundAssignDivide => BinopKind::CompoundAssignDivide,
            Modulus => BinopKind::Modulus,
            CompoundAssignModulus => BinopKind::CompoundAssignModulus,
            _ => panic!(),
        }
    }
}

impl Session {
    pub fn parse(&self) -> Vec<Module> {
        let mut modules = Vec::new();
        let mut lex_errs = Vec::new();
        let mut parse_errs = Vec::new();
        for (index, file) in self.source_map.files.iter().enumerate() {
            let (tokens, mut file_lex_errs) =
                Cursor::from_file(file, FileId::from_usize(index)).into_vec();
            lex_errs.append(&mut file_lex_errs);
            match Parser::new(&tokens.as_slice()).parse_module() {
                Ok(module) => modules.push(module),
                Err(err) => parse_errs.push(err),
            }
        }
        modules
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn parse() {
        let text = "fnc hi(param) { let x = 2; }";
        let (tokens, _errs) = Cursor::from_str(text).into_vec();
        println!("tokens: {:?}", tokens);
        let mut parser = Parser::new(&tokens);
        let module = parser.parse_module().unwrap();
        print!("mod: {:?}", module);
    }
}
