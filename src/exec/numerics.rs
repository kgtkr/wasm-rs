use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use generic_array::{ArrayLength, GenericArray};
use std::io::Cursor;
use typenum::{consts, Unsigned};

pub trait Byteable: Sized {
    type N: ArrayLength<u8>;

    fn to_bytes(self) -> GenericArray<u8, Self::N>;
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self;
}

impl Byteable for i8 {
    type N = consts::U1;

    fn to_bytes(self) -> GenericArray<u8, Self::N> {
        let mut buf = Vec::with_capacity(Self::N::to_usize());
        buf.write_i8(self).unwrap();
        GenericArray::clone_from_slice(&buf[..])
    }
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self {
        let mut rdr = Cursor::new(bytes.as_slice());
        rdr.read_i8().unwrap()
    }
}

impl Byteable for u8 {
    type N = consts::U1;

    fn to_bytes(self) -> GenericArray<u8, Self::N> {
        let mut buf = Vec::with_capacity(Self::N::to_usize());
        buf.write_u8(self).unwrap();
        GenericArray::clone_from_slice(&buf[..])
    }
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self {
        let mut rdr = Cursor::new(bytes.as_slice());
        rdr.read_u8().unwrap()
    }
}

impl Byteable for i16 {
    type N = consts::U2;

    fn to_bytes(self) -> GenericArray<u8, Self::N> {
        let mut buf = Vec::with_capacity(Self::N::to_usize());
        buf.write_i16::<LittleEndian>(self).unwrap();
        GenericArray::clone_from_slice(&buf[..])
    }
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self {
        let mut rdr = Cursor::new(bytes.as_slice());
        rdr.read_i16::<LittleEndian>().unwrap()
    }
}

impl Byteable for u16 {
    type N = consts::U2;

    fn to_bytes(self) -> GenericArray<u8, Self::N> {
        let mut buf = Vec::with_capacity(Self::N::to_usize());
        buf.write_u16::<LittleEndian>(self).unwrap();
        GenericArray::clone_from_slice(&buf[..])
    }
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self {
        let mut rdr = Cursor::new(bytes.as_slice());
        rdr.read_u16::<LittleEndian>().unwrap()
    }
}

impl Byteable for i32 {
    type N = consts::U4;

    fn to_bytes(self) -> GenericArray<u8, Self::N> {
        let mut buf = Vec::with_capacity(Self::N::to_usize());
        buf.write_i32::<LittleEndian>(self).unwrap();
        GenericArray::clone_from_slice(&buf[..])
    }
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self {
        let mut rdr = Cursor::new(bytes.as_slice());
        rdr.read_i32::<LittleEndian>().unwrap()
    }
}

impl Byteable for u32 {
    type N = consts::U4;

    fn to_bytes(self) -> GenericArray<u8, Self::N> {
        let mut buf = Vec::with_capacity(Self::N::to_usize());
        buf.write_u32::<LittleEndian>(self).unwrap();
        GenericArray::clone_from_slice(&buf[..])
    }
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self {
        let mut rdr = Cursor::new(bytes.as_slice());
        rdr.read_u32::<LittleEndian>().unwrap()
    }
}

impl Byteable for i64 {
    type N = consts::U8;

    fn to_bytes(self) -> GenericArray<u8, Self::N> {
        let mut buf = Vec::with_capacity(Self::N::to_usize());
        buf.write_i64::<LittleEndian>(self).unwrap();
        GenericArray::clone_from_slice(&buf[..])
    }
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self {
        let mut rdr = Cursor::new(bytes.as_slice());
        rdr.read_i64::<LittleEndian>().unwrap()
    }
}

impl Byteable for u64 {
    type N = consts::U8;

    fn to_bytes(self) -> GenericArray<u8, Self::N> {
        let mut buf = Vec::with_capacity(Self::N::to_usize());
        buf.write_u64::<LittleEndian>(self).unwrap();
        GenericArray::clone_from_slice(&buf[..])
    }
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self {
        let mut rdr = Cursor::new(bytes.as_slice());
        rdr.read_u64::<LittleEndian>().unwrap()
    }
}

impl Byteable for f32 {
    type N = consts::U4;

    fn to_bytes(self) -> GenericArray<u8, Self::N> {
        let mut buf = Vec::with_capacity(Self::N::to_usize());
        buf.write_f32::<LittleEndian>(self).unwrap();
        GenericArray::clone_from_slice(&buf[..])
    }
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self {
        let mut rdr = Cursor::new(bytes.as_slice());
        rdr.read_f32::<LittleEndian>().unwrap()
    }
}

impl Byteable for f64 {
    type N = consts::U8;

    fn to_bytes(self) -> GenericArray<u8, Self::N> {
        let mut buf = Vec::with_capacity(Self::N::to_usize());
        buf.write_f64::<LittleEndian>(self).unwrap();
        GenericArray::clone_from_slice(&buf[..])
    }
    fn from_bytes(bytes: &GenericArray<u8, Self::N>) -> Self {
        let mut rdr = Cursor::new(bytes.as_slice());
        rdr.read_f64::<LittleEndian>().unwrap()
    }
}
