use super::parser;
use super::values::{p_u32, p_vec};
use super::Decoder;
use super::Encoder;
use crate::structure::types::{
    ElemType, FuncType, GlobalType, Limits, MemType, Mut, ResultType, TableType, ValType,
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{flat_map, map, map_res},
    multi::many_m_n,
    sequence::tuple,
    IResult,
};

impl Encoder for ValType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(match self {
            ValType::I32 => 0x7f,
            ValType::I64 => 0x7e,
            ValType::F32 => 0x7d,
            ValType::F64 => 0x7c,
        });
    }
}

impl Decoder for ValType {
    fn decode(input: &[u8]) -> IResult<&[u8], ValType> {
        alt((
            map(parser::token(0x7f), |_| ValType::I32),
            map(parser::token(0x7e), |_| ValType::I64),
            map(parser::token(0x7d), |_| ValType::F32),
            map(parser::token(0x7c), |_| ValType::F64),
        ))(input)
    }
}

impl Encoder for ResultType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self.0 {
            Some(x) => {
                x.encode(bytes);
            }
            None => {
                bytes.push(0x40);
            }
        }
    }
}

impl Decoder for ResultType {
    fn decode(input: &[u8]) -> IResult<&[u8], ResultType> {
        map(
            alt((
                map(ValType::decode, |vt| Some(vt)),
                map(parser::token(0x40), |_| (None)),
            )),
            ResultType,
        )(input)
    }
}

impl Encoder for FuncType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(0x60);
        self.0.encode(bytes);
        self.1.encode(bytes);
    }
}

impl Decoder for FuncType {
    fn decode(input: &[u8]) -> IResult<&[u8], FuncType> {
        map(
            tuple((
                parser::token(0x60),
                p_vec(ValType::decode),
                p_vec(ValType::decode),
            )),
            |(_, a, b)| FuncType(a, b),
        )(input)
    }
}

impl Encoder for Limits {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self.max {
            None => {
                bytes.push(0x00);
                self.min.encode(bytes);
            }
            Some(max) => {
                bytes.push(0x01);
                self.min.encode(bytes);
                max.encode(bytes);
            }
        }
    }
}

impl Decoder for Limits {
    fn decode(input: &[u8]) -> IResult<&[u8], Limits> {
        alt((
            map(tuple((parser::token(0x00), p_u32)), |(_, min)| Limits {
                min,
                max: None,
            }),
            map(
                tuple((parser::token(0x01), p_u32, p_u32)),
                |(_, min, max)| Limits {
                    min,
                    max: Some(max),
                },
            ),
        ))(input)
    }
}

impl Encoder for MemType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Decoder for MemType {
    fn decode(input: &[u8]) -> IResult<&[u8], MemType> {
        map(Limits::decode, MemType)(input)
    }
}

impl Encoder for TableType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.1.encode(bytes);
        self.0.encode(bytes);
    }
}

impl Decoder for TableType {
    fn decode(input: &[u8]) -> IResult<&[u8], TableType> {
        map(tuple((ElemType::decode, Limits::decode)), |(et, lt)| {
            TableType(lt, et)
        })(input)
    }
}

impl Encoder for ElemType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            ElemType::FuncRef => {
                bytes.push(0x70);
            }
        }
    }
}

impl Decoder for ElemType {
    fn decode(input: &[u8]) -> IResult<&[u8], ElemType> {
        map(parser::token(0x70), |_| ElemType::FuncRef)(input)
    }
}

impl Encoder for GlobalType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.1.encode(bytes);
        self.0.encode(bytes);
    }
}

impl Decoder for GlobalType {
    fn decode(input: &[u8]) -> IResult<&[u8], GlobalType> {
        map(tuple((ValType::decode, Mut::decode)), |(gt, m)| {
            GlobalType(m, gt)
        })(input)
    }
}

impl Encoder for Mut {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(match self {
            Mut::Const => 0x00,
            Mut::Var => 0x01,
        });
    }
}

impl Decoder for Mut {
    fn decode(input: &[u8]) -> IResult<&[u8], Mut> {
        alt((
            map(parser::token(0x00), |_| Mut::Const),
            map(parser::token(0x01), |_| Mut::Var),
        ))(input)
    }
}

#[test]
fn tests() {
    use super::test_helper;

    test_helper::identity_encode_decode::<ValType>();
    test_helper::identity_encode_decode::<ResultType>();
    test_helper::identity_encode_decode::<FuncType>();
    test_helper::identity_encode_decode::<Limits>();
    test_helper::identity_encode_decode::<MemType>();
    test_helper::identity_encode_decode::<TableType>();
    test_helper::identity_encode_decode::<ElemType>();
    test_helper::identity_encode_decode::<GlobalType>();
    test_helper::identity_encode_decode::<Mut>();
}
