use super::parser;
use super::util::loop_encode;
use super::Encoder;
use crate::structure::values::{Byte, Name};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use leb128;
use nom::{
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{map, map_res},
    multi::many_m_n,
    sequence::tuple,
    IResult,
};
use std::convert::TryFrom;
use std::io::Read;

impl Encoder for Byte {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(self.0);
    }
}

pub fn p_byte(input: &[u8]) -> IResult<&[u8], Byte> {
    map(parser::any(), Byte)(input)
}

impl Encoder for i32 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        leb128::write::signed(bytes, *self as i64).unwrap();
    }
}

pub fn p_i32(input: &[u8]) -> IResult<&[u8], i32> {
    parser::io_read(|rb| {
        leb128::read::signed(rb)
            .ok()
            .and_then(|x| i32::try_from(x).ok())
    })(input)
}

impl Encoder for i64 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        leb128::write::signed(bytes, *self).unwrap();
    }
}

pub fn p_i64(input: &[u8]) -> IResult<&[u8], i64> {
    parser::io_read(|rb| leb128::read::signed(rb).ok())(input)
}

impl Encoder for u32 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        leb128::write::unsigned(bytes, *self as u64).unwrap();
    }
}

pub fn p_u32(input: &[u8]) -> IResult<&[u8], u32> {
    parser::io_read(|rb| {
        leb128::read::unsigned(rb)
            .ok()
            .and_then(|x| u32::try_from(x).ok())
    })(input)
}

impl Encoder for u64 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        leb128::write::unsigned(bytes, *self).unwrap();
    }
}

pub fn p_u64(input: &[u8]) -> IResult<&[u8], u64> {
    parser::io_read(|rb| leb128::read::unsigned(rb).ok())(input)
}

impl Encoder for f32 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.write_f32::<LittleEndian>(*self).unwrap();
    }
}

pub fn p_f32(input: &[u8]) -> IResult<&[u8], f32> {
    parser::io_read(|rb| rb.read_f32::<LittleEndian>().ok())(input)
}

impl Encoder for f64 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.write_f64::<LittleEndian>(*self).unwrap();
    }
}

pub fn p_f64(input: &[u8]) -> IResult<&[u8], f64> {
    parser::io_read(|rb| rb.read_f64::<LittleEndian>().ok())(input)
}

impl Encoder for Name {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.bytes().map(Byte).collect::<Vec<_>>().encode(bytes);
    }
}

pub fn p_name(input: &[u8]) -> IResult<&[u8], Name> {
    let (input, bytes) = p_vec(p_byte)(input)?;
    let bytes = bytes.into_iter().map(|x| x.0).collect::<Vec<_>>();
    let s = String::from_utf8(bytes).map(Name);
    match s {
        Ok(x) => Ok((input, x)),
        Err(_) => Err(nom::Err::Error((input, nom::error::ErrorKind::Char))),
    }
}

impl<T> Encoder for Vec<T>
where
    T: Encoder,
{
    fn encode(&self, bytes: &mut Vec<u8>) {
        (self.len() as u32).encode(bytes);
        loop_encode(self, bytes);
    }
}

pub fn p_vec<T, P: Fn(&[u8]) -> IResult<&[u8], T>>(
    p: P,
) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<T>> {
    move |input| {
        let (input, size) = p_u32(input)?;
        let size = size as usize;
        many_m_n(size, size, &p)(input)
    }
}