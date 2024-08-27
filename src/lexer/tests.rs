use super::tokenize;

use super::TokenKind::*;
#[test]
fn test() {
    let text = "identifier, 432984\"string\"";
    let mut token_iter = tokenize(text);
    assert_eq!(token_iter.next().unwrap().kind, Identifier);
    assert_eq!(token_iter.next().unwrap().kind, Comma);
    assert_eq!(token_iter.next().unwrap().kind, VerticalWhitespace);
    assert_eq!(token_iter.next().unwrap().kind, IntegerLiteral);
    assert_eq!(
        token_iter.next().unwrap().kind,
        StringLiteral { is_closed: true }
    );
}
