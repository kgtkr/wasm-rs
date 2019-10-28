use super::parser;
use super::values::{p_u32, p_vec};
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

pub fn p_valtype(input: &[u8]) -> IResult<&[u8], ValType> {
    alt((
        map(parser::token(0x7f), |_| ValType::I32),
        map(parser::token(0x7e), |_| ValType::I64),
        map(parser::token(0x7d), |_| ValType::F32),
        map(parser::token(0x7c), |_| ValType::F64),
    ))(input)
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

pub fn p_resulttype(input: &[u8]) -> IResult<&[u8], ResultType> {
    map(
        alt((
            map(p_valtype, |vt| Some(vt)),
            map(parser::token(0x40), |_| (None)),
        )),
        ResultType,
    )(input)
}

impl Encoder for FuncType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(0x60);
        self.0.encode(bytes);
        self.1.encode(bytes);
    }
}

pub fn p_functype(input: &[u8]) -> IResult<&[u8], FuncType> {
    map(
        tuple((parser::token(0x60), p_vec(p_valtype), p_vec(p_valtype))),
        |(_, a, b)| FuncType(a, b),
    )(input)
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

pub fn p_limits(input: &[u8]) -> IResult<&[u8], Limits> {
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

impl Encoder for MemType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

pub fn p_memtype(input: &[u8]) -> IResult<&[u8], MemType> {
    map(p_limits, MemType)(input)
}

impl Encoder for TableType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.1.encode(bytes);
        self.0.encode(bytes);
    }
}

pub fn p_tabletype(input: &[u8]) -> IResult<&[u8], TableType> {
    map(tuple((p_elemtype, p_limits)), |(et, lt)| TableType(lt, et))(input)
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

pub fn p_elemtype(input: &[u8]) -> IResult<&[u8], ElemType> {
    map(parser::token(0x70), |_| ElemType::FuncRef)(input)
}

impl Encoder for GlobalType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.1.encode(bytes);
        self.0.encode(bytes);
    }
}

pub fn p_globaltype(input: &[u8]) -> IResult<&[u8], GlobalType> {
    map(tuple((p_valtype, p_mut)), |(gt, m)| GlobalType(m, gt))(input)
}

impl Encoder for Mut {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(match self {
            Mut::Const => 0x00,
            Mut::Var => 0x01,
        });
    }
}

pub fn p_mut(input: &[u8]) -> IResult<&[u8], Mut> {
    alt((
        map(parser::token(0x00), |_| Mut::Const),
        map(parser::token(0x01), |_| Mut::Var),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn test_valtype() {
        proptest!(|(x: ValType)| {
            assert_eq!(p_valtype(&x.encode_to_vec()), Ok((vec![].as_ref(),x)));
        });
    }

    #[test]
    fn test_resulttype() {
        proptest!(|(x: ResultType)| {
            assert_eq!(p_resulttype(&x.encode_to_vec()), Ok((vec![].as_ref(),x)));
        });
    }

    #[test]
    fn test_functype() {
        proptest!(|(x: FuncType)| {
            assert_eq!(p_functype(&x.encode_to_vec()), Ok((vec![].as_ref(),x)));
        });
    }

    #[test]
    fn test_limits() {
        proptest!(|(x: Limits)| {
            assert_eq!(p_limits(&x.encode_to_vec()), Ok((vec![].as_ref(),x)));
        });
    }

    #[test]
    fn test_memtype() {
        proptest!(|(x: MemType)| {
            assert_eq!(p_memtype(&x.encode_to_vec()), Ok((vec![].as_ref(),x)));
        });
    }

    #[test]
    fn test_tabletype() {
        proptest!(|(x: TableType)| {
            assert_eq!(p_tabletype(&x.encode_to_vec()), Ok((vec![].as_ref(),x)));
        });
    }

    #[test]
    fn test_elemtype() {
        proptest!(|(x: ElemType)| {
            assert_eq!(p_elemtype(&x.encode_to_vec()), Ok((vec![].as_ref(),x)));
        });
    }

    #[test]
    fn test_globaltype() {
        proptest!(|(x: GlobalType)| {
            assert_eq!(p_globaltype(&x.encode_to_vec()), Ok((vec![].as_ref(),x)));
        });
    }

    #[test]
    fn test_mut() {
        proptest!(|(x: Mut)| {
            assert_eq!(p_mut(&x.encode_to_vec()), Ok((vec![].as_ref(),x)));
        });
    }
}
