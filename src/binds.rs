use crate::asth::Op;

pub fn prefix_binding_power(op: Op) -> ((), u8) { 
    match op {
        Op::BANG | Op::MINUS => ((), 11),
        Op::RETURN | Op::PRINT => ((), 1),
        _ => panic!("bad op: {:?}", op),
    }
}

pub fn infix_binding_power(op: Op) -> Option<(u8, u8)> {
    let res = match op {
        // '=' => (2, 1),
        // '?' => (4, 3),
        Op::AND | Op::OR => (3, 4),
        Op::BANG_EQUAL
        | Op::EQUAL_EQUAL
        | Op::LESS
        | Op::LESS_EQUAL
        | Op::GREATER
        | Op::GREATER_EQUAL => (5, 6),
        Op::PLUS | Op::MINUS => (7, 8),
        Op::STAR | Op::SLASH => (9, 10),
        Op::FIELD => (16, 15),
        _ => return None,
    };
    Some(res)
}

pub fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    let res = match op {
        Op::CALL => (13, ()),
        _ => return None,
    };
    Some(res)
}
