enum BinopKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}
enum UnopKind {
    Neg,
    Plus
}
enum Expr {
    Paren(Box<Expr>),
    Binop {
        kind: BinopKind,
        operands: (BinopKind, BinopKind),
    },
    Unop {
        kind: UnopKind,
        operand: Box<Expr>,
    },
}

enum Program {
    Decl(),
    Expr(),
}
