use crate::compile_target::Span;

#[derive(Debug)]
pub struct Module {
    pub declarations: Vec<Declaration>,
    pub span: Span,
}
#[derive(Debug)]
pub enum Declaration {
    Function(Box<Function>),
    Variable(Box<VariableDeclaration>),
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub identifier: Identifier,
    pub kind: VariableDeclarationKind,
    pub expression: Box<Expression>,
    pub span: Span,
}
#[derive(Debug)]
pub enum VariableDeclarationKind {
    Let,
    Var,
}
#[derive(Debug)]
pub struct Function {
    pub identifier: Identifier,
    pub args: Vec<Identifier>,
    pub block: Box<Block>,
}
#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}
#[derive(Debug)]
pub enum Statement {
    Expression(Box<Expression>),
    Declaration(Box<Declaration>),
    Block(Box<Block>),
}

#[derive(Debug)]
pub struct Expression {
    span: Span,
    kind: ExpressionKind,
}
#[derive(Debug)]
pub enum ExpressionKind {
    Binop(Box<Binop>),
    Unop(Box<Unop>),
    Literal(Box<Literal>),
}

#[derive(Debug)]
pub struct Binop {
    pub kind: BinopKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}
#[derive(Debug)]
pub enum BinopKind {
    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Debug)]
pub struct Unop {
    pub kind: UnopKind,
    pub expr: Expression,
}
#[derive(Debug)]
pub enum UnopKind {
    Plus,
    Minus,
}
#[derive(Debug)]
pub struct Literal {
    pub kind: LiteralKind,
}

#[derive(Debug)]
pub enum LiteralKind {
    Integer,
    String,
}

#[derive(Debug)]
pub struct Identifier {
    pub span: Span,
}
