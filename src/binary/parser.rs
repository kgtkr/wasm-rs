use nom::{
    bytes::complete::{take},
    combinator::map,
    error::ParseError,
    IResult, InputIter, InputTake,
};

pub fn any<Input, Error: ParseError<Input>>() -> impl Fn(Input) -> IResult<Input, Input::Item, Error>
where
    Input: InputIter + InputTake + Clone,
{
    map(take(1usize), |x: Input| x.iter_indices().next().unwrap().1)
}

pub fn token<'a, Error: ParseError<&'a [u8]>>(
    x: u8,
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], u8, Error> {
    move |input| {
        let (input, token) = any()(input)?;
        if token == x {
            Ok((input, token))
        } else {
            Err(nom::Err::Error(Error::from_char(input, '?')))
        }
    }
}

pub fn eof<'a, Error: ParseError<&'a [u8]>>() -> impl Fn(&'a [u8]) -> IResult<&'a [u8], (), Error> {
    |input| {
        if input.len() == 0 {
            Ok((input, ()))
        } else {
            Err(nom::Err::Error(Error::from_char(input, '?')))
        }
    }
}

pub struct ReadBytes<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> std::io::Read for ReadBytes<'a> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let old_pos = self.pos;
        for buf_x in buf {
            if let Some(x) = self.bytes.get(self.pos) {
                (*buf_x) = *x;
                self.pos += 1;
            } else {
                break;
            }
        }
        Ok(self.pos - old_pos)
    }
}

pub fn io_read<'a, O, Error: ParseError<&'a [u8]>>(
    f: impl Fn(&mut ReadBytes) -> Option<O>,
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], O, Error> {
    move |input| {
        let mut rb = ReadBytes {
            bytes: input,
            pos: 0,
        };
        match f(&mut rb) {
            Some(x) => Ok((&input[rb.pos..], x)),
            None => Err(nom::Err::Error(Error::from_char(input, '?'))),
        }
    }
}
