use crate::compile_target::Span;

#[derive(Debug)]
pub struct Module {
    pub declarations: Vec<Declaration>,
    pub span: Option<Span>,
}
#[derive(Debug)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub identifier: Identifier,
    pub span: Span,
}
#[derive(Debug)]
pub enum DeclarationKind {
    Function(Box<Function>),
    Variable(Box<VariableDeclaration>),
}

#[derive(Debug)]
pub struct VariableDeclaration {
    kind: VariableDeclarationKind,
    expression: Box<Expression>,
}
#[derive(Debug)]
pub enum VariableDeclarationKind {
    Let,
    Var,
}
#[derive(Debug)]
pub struct Function {
    pub args: Vec<Identifier>,
    pub block: Box<Block>,
}
#[derive(Debug)]
pub struct Block {
    statements: Vec<Statement>,
    span: Span,
}
#[derive(Debug)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}
#[derive(Debug)]
pub enum StatementKind {
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
    pub expr: Box<Expression>,
}
#[derive(Debug)]
pub enum UnopKind {
    Plus,
    Minus,
}
#[derive(Debug)]
pub struct Literal {
    kind: LiteralKind,
}
#[derive(Debug)]
pub enum LiteralKind {
    Integer { value: u128 },
    String { value: String },
}

#[derive(Debug)]
pub struct Identifier {
    pub span: Span,
}
