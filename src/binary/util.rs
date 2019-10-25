use super::Encoder;

pub fn loop_encode<T: Encoder>(xs: &Vec<T>, bytes: &mut Vec<u8>) {
    for x in xs {
        x.encode(bytes);
    }
}
