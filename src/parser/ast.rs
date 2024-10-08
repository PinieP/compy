use crate::source::Span;

#[derive(Debug)]
pub struct Module {
    pub declarations: Vec<Declaration>,
    pub span: Span,
}
#[derive(Debug)]
pub enum Declaration {
    Function(Box<Function>),
    Type,
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
    pub body: FunctionBody,
}

#[derive(Debug)]
pub enum FunctionBody {
    Block(Box<Block>),
    Expression(Box<Expression>),
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
    pub span: Span,
    pub kind: ExpressionKind,
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
    pub lhs: Expression,
    pub rhs: Expression,
}
#[derive(Debug)]
pub enum BinopKind {
    Assign,

    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,

    LogicalAnd,
    BitwiseAnd,
    CompoundAssignBitwiseAnd,

    LogicalOr,
    BitwiseOr,
    CompoundAssignBitwiseOr,

    BitwiseXor,
    CompoundAssignBitwiseXor,

    Plus,
    CompoundAssignPlus,
    Minus,
    CompoundAssignMinus,
    Multiply,
    CompoundAssignMultiply,
    Divide,
    CompoundAssignDivide,
    Modulus,
    CompoundAssignModulus,
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
