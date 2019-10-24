use super::util::loop_encode;
use super::Encoder;
use crate::structure::modules::{
    FuncIdx, GlobalIdx, LabelIdx, LocalIdx, MemIdx, Module, TableIdx, TypeIdx,
};

impl Encoder for TypeIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Encoder for FuncIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Encoder for GlobalIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Encoder for LocalIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Encoder for LabelIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Encoder for MemIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Encoder for TableIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}
