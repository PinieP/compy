use super::{Cursor, LiteralKind, TokenKind::*};

#[test]
fn test() {
    let text = "identifier,/*Ignore me*/ 432984\"string\"";
    let mut cursor = Cursor::from_str(text);
    println!("{:?}", cursor.clone().collect::<Vec<_>>());
    assert_eq!(cursor.next().unwrap().kind, Identifier);
    assert_eq!(cursor.next().unwrap().kind, Comma);
    assert_eq!(cursor.next().unwrap().kind, Literal(LiteralKind::Integer));
    assert_eq!(
        cursor.next().unwrap().kind,
        Literal(LiteralKind::String { is_closed: true })
    );
}
