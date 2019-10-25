pub trait Encoder {
    fn encode(&self, bytes: &mut Vec<u8>);

    fn encode_to_vec(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        self.encode(&mut bytes);
        bytes
    }
}

impl<T> Encoder for &T
where
    T: Encoder,
{
    fn encode(&self, bytes: &mut Vec<u8>) {
        (*self).encode(bytes);
    }
}

impl<A, B> Encoder for (A, B)
where
    A: Encoder,
    B: Encoder,
{
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
        self.1.encode(bytes);
    }
}
