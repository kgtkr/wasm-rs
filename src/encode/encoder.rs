pub trait Encoder {
    fn encode(&self, bytes: &mut Vec<u8>);
}
