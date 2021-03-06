use super::parser::eof;
use nom::{sequence::tuple, IResult};

pub trait Decoder
where
    Self: std::marker::Sized,
{
    fn decode(input: &[u8]) -> IResult<&[u8], Self>;

    fn decode_to_end(input: &[u8]) -> Result<Self, nom::Err<(&[u8], nom::error::ErrorKind)>> {
        let (_, (x, _)) = tuple((Self::decode, eof()))(input)?;
        Ok(x)
    }
}

impl<A, B> Decoder for (A, B)
where
    A: Decoder,
    B: Decoder,
{
    fn decode(input: &[u8]) -> IResult<&[u8], Self> {
        tuple((A::decode, B::decode))(input)
    }
}
