use super::Encoder;
use crate::structure::types::{
    ElemType, FuncType, GlobalType, Limits, MemType, Mut, ResultType, TableType, ValType,
};

impl Encoder for ValType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(match self {
            ValType::I32 => 0x7f,
            ValType::I64 => 0x7e,
            ValType::F32 => 0x7d,
            ValType::F64 => 0x7c,
        });
    }
}

impl Encoder for ResultType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self.0 {
            Some(x) => {
                x.encode(bytes);
            }
            None => {
                bytes.push(0x40);
            }
        }
    }
}

impl Encoder for FuncType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(0x60);
        self.0.encode(bytes);
        self.1.encode(bytes);
    }
}

impl Encoder for Limits {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self.max {
            None => {
                bytes.push(0x00);
                self.min.encode(bytes);
            }
            Some(max) => {
                bytes.push(0x01);
                self.min.encode(bytes);
                max.encode(bytes);
            }
        }
    }
}

impl Encoder for MemType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Encoder for TableType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.1.encode(bytes);
        self.0.encode(bytes);
    }
}

impl Encoder for ElemType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            ElemType::FuncRef => {
                bytes.push(0x70);
            }
        }
    }
}

impl Encoder for GlobalType {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.1.encode(bytes);
        self.0.encode(bytes);
    }
}

impl Encoder for Mut {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(match self {
            Mut::Const => 0x00,
            Mut::Var => 0x01,
        });
    }
}
