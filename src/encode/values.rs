use super::util::loop_encode;
use super::Encoder;
use crate::structure::values::{Byte, Name};
use byteorder::{LittleEndian, WriteBytesExt};
use leb128;

impl Encoder for Byte {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(self.0);
    }
}

impl Encoder for i32 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        leb128::write::signed(bytes, *self as i64).unwrap();
    }
}

impl Encoder for i64 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        leb128::write::signed(bytes, *self).unwrap();
    }
}

impl Encoder for u32 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        leb128::write::unsigned(bytes, *self as u64).unwrap();
    }
}

impl Encoder for u64 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        leb128::write::unsigned(bytes, *self).unwrap();
    }
}

impl Encoder for f32 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.write_f32::<LittleEndian>(*self).unwrap();
    }
}

impl Encoder for f64 {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.write_f64::<LittleEndian>(*self).unwrap();
    }
}

impl Encoder for Name {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.bytes().map(Byte).collect::<Vec<_>>().encode(bytes);
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
