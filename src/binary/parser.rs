use nom::{
    bytes::complete::take, combinator::map, error::ParseError, IResult, InputIter, InputTake,
};

pub fn any<Input, Error: ParseError<Input>>() -> impl Fn(Input) -> IResult<Input, Input::Item, Error>
where
    Input: InputIter + InputTake + Clone,
{
    map(take(1usize), |x: Input| x.iter_indices().next().unwrap().1)
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
    /*function::parser(move |input: &mut Input| {
        let position = input.position();
        let mut rs = ReadStream {
            stream: input,
            consumed_len: 0,
        };
        match f(&mut rs) {
            Some(x) => Ok((
                x,
                if rs.consumed_len != 0 {
                    Consumed::Consumed(())
                } else {
                    Consumed::Empty(())
                },
            )),
            None => {
                let err = Input::Error::empty(position);
                Err(if rs.consumed_len != 0 {
                    Consumed::Consumed(err.into())
                } else {
                    Consumed::Empty(err.into())
                })
            }
        }
    })*/
}
