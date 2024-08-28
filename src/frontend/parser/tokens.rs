use crate::frontend::lexer::{Token, TokenKind};

use TokenKind::*;
impl TokenKind {
    pub fn is_binop(&self) -> bool {
        matches!(self, Plus | Minus | Star | Slash)
    }
    pub fn is_unop(&self) -> bool {
        matches!(self, Plus | Minus)
    }
}
