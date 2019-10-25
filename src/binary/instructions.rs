use super::util::loop_encode;
use super::Encoder;
use crate::structure::instructions::{Expr, Instr, Memarg};

impl Encoder for Expr {
    fn encode(&self, bytes: &mut Vec<u8>) {
        loop_encode(&self.0, bytes);
        bytes.push(0x0b);
    }
}

impl Encoder for Instr {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self {
            Instr::Unreachable => bytes.push(0x00),
            Instr::Nop => bytes.push(0x01),
            Instr::Block(rt, is) => {
                bytes.push(0x02);
                rt.encode(bytes);
                loop_encode(is, bytes);
                bytes.push(0x0b);
            }
            Instr::Loop(rt, is) => {
                bytes.push(0x03);
                rt.encode(bytes);
                loop_encode(is, bytes);
                bytes.push(0x0b);
            }
            Instr::If(rt, is1, is2) => {
                bytes.push(0x04);
                rt.encode(bytes);
                loop_encode(is1, bytes);
                if !is2.is_empty() {
                    bytes.push(0x05);
                    loop_encode(is2, bytes);
                }
                bytes.push(0x0b);
            }
            Instr::Br(l) => {
                bytes.push(0x0c);
                l.encode(bytes);
            }
            Instr::BrIf(l) => {
                bytes.push(0x0d);
                l.encode(bytes);
            }
            Instr::BrTable(ls, l) => {
                bytes.push(0x0e);
                ls.encode(bytes);
                l.encode(bytes);
            }
            Instr::Return => bytes.push(0x0f),
            Instr::Call(x) => {
                bytes.push(0x10);
                x.encode(bytes);
            }
            Instr::CallIndirect(x) => {
                bytes.push(0x11);
                x.encode(bytes);
                bytes.push(0x00);
            }
            Instr::Drop => bytes.push(0x1a),
            Instr::Select => bytes.push(0x1b),
            Instr::LocalGet(x) => {
                bytes.push(0x20);
                x.encode(bytes);
            }
            Instr::LocalSet(x) => {
                bytes.push(0x21);
                x.encode(bytes);
            }
            Instr::LocalTee(x) => {
                bytes.push(0x22);
                x.encode(bytes);
            }
            Instr::GlobalGet(x) => {
                bytes.push(0x23);
                x.encode(bytes);
            }
            Instr::GlobalSet(x) => {
                bytes.push(0x24);
                x.encode(bytes);
            }
            Instr::I32Load(m) => {
                bytes.push(0x28);
                m.encode(bytes);
            }
            Instr::I64Load(m) => {
                bytes.push(0x29);
                m.encode(bytes);
            }
            Instr::F32Load(m) => {
                bytes.push(0x2a);
                m.encode(bytes);
            }
            Instr::F64Load(m) => {
                bytes.push(0x2b);
                m.encode(bytes);
            }
            Instr::I32Load8S(m) => {
                bytes.push(0x2c);
                m.encode(bytes);
            }
            Instr::I32Load8U(m) => {
                bytes.push(0x2d);
                m.encode(bytes);
            }
            Instr::I32Load16S(m) => {
                bytes.push(0x2e);
                m.encode(bytes);
            }
            Instr::I32Load16U(m) => {
                bytes.push(0x2f);
                m.encode(bytes);
            }
            Instr::I64Load8S(m) => {
                bytes.push(0x30);
                m.encode(bytes);
            }
            Instr::I64Load8U(m) => {
                bytes.push(0x31);
                m.encode(bytes);
            }
            Instr::I64Load16S(m) => {
                bytes.push(0x32);
                m.encode(bytes);
            }
            Instr::I64Load16U(m) => {
                bytes.push(0x33);
                m.encode(bytes);
            }
            Instr::I64Load32S(m) => {
                bytes.push(0x34);
                m.encode(bytes);
            }
            Instr::I64Load32U(m) => {
                bytes.push(0x35);
                m.encode(bytes);
            }
            Instr::I32Store(m) => {
                bytes.push(0x36);
                m.encode(bytes);
            }
            Instr::I64Store(m) => {
                bytes.push(0x37);
                m.encode(bytes);
            }
            Instr::F32Store(m) => {
                bytes.push(0x38);
                m.encode(bytes);
            }
            Instr::F64Store(m) => {
                bytes.push(0x39);
                m.encode(bytes);
            }
            Instr::I32Store8(m) => {
                bytes.push(0x3a);
                m.encode(bytes);
            }
            Instr::I32Store16(m) => {
                bytes.push(0x3b);
                m.encode(bytes);
            }
            Instr::I64Store8(m) => {
                bytes.push(0x3c);
                m.encode(bytes);
            }
            Instr::I64Store16(m) => {
                bytes.push(0x3d);
                m.encode(bytes);
            }
            Instr::I64Store32(m) => {
                bytes.push(0x3e);
                m.encode(bytes);
            }
            Instr::MemorySize => {
                bytes.push(0x3f);
                bytes.push(0x00);
            }
            Instr::MemoryGrow => {
                bytes.push(0x40);
                bytes.push(0x00);
            }
            Instr::I32Const(n) => {
                bytes.push(0x41);
                n.encode(bytes);
            }
            Instr::I64Const(n) => {
                bytes.push(0x42);
                n.encode(bytes);
            }
            Instr::F32Const(z) => {
                bytes.push(0x43);
                z.encode(bytes);
            }
            Instr::F64Const(z) => {
                bytes.push(0x44);
                z.encode(bytes);
            }
            Instr::I32Eqz => bytes.push(0x45),
            Instr::I32Eq => bytes.push(0x46),
            Instr::I32Ne => bytes.push(0x47),
            Instr::I32LtS => bytes.push(0x48),
            Instr::I32LtU => bytes.push(0x49),
            Instr::I32GtS => bytes.push(0x4a),
            Instr::I32GtU => bytes.push(0x4b),
            Instr::I32LeS => bytes.push(0x4c),
            Instr::I32LeU => bytes.push(0x4d),
            Instr::I32GeS => bytes.push(0x4e),
            Instr::I32GeU => bytes.push(0x4f),
            Instr::I64Eqz => bytes.push(0x50),
            Instr::I64Eq => bytes.push(0x51),
            Instr::I64Ne => bytes.push(0x52),
            Instr::I64LtS => bytes.push(0x53),
            Instr::I64LtU => bytes.push(0x54),
            Instr::I64GtS => bytes.push(0x55),
            Instr::I64GtU => bytes.push(0x56),
            Instr::I64LeS => bytes.push(0x57),
            Instr::I64LeU => bytes.push(0x58),
            Instr::I64GeS => bytes.push(0x59),
            Instr::I64GeU => bytes.push(0x5a),
            Instr::F32Eq => bytes.push(0x5b),
            Instr::F32Ne => bytes.push(0x5c),
            Instr::F32Lt => bytes.push(0x5d),
            Instr::F32Gt => bytes.push(0x5e),
            Instr::F32Le => bytes.push(0x5f),
            Instr::F32Ge => bytes.push(0x60),
            Instr::F64Eq => bytes.push(0x61),
            Instr::F64Ne => bytes.push(0x62),
            Instr::F64Lt => bytes.push(0x63),
            Instr::F64Gt => bytes.push(0x64),
            Instr::F64Le => bytes.push(0x65),
            Instr::F64Ge => bytes.push(0x66),
            Instr::I32Clz => bytes.push(0x67),
            Instr::I32Ctz => bytes.push(0x68),
            Instr::I32Popcnt => bytes.push(0x69),
            Instr::I32Add => bytes.push(0x6a),
            Instr::I32Sub => bytes.push(0x6b),
            Instr::I32Mul => bytes.push(0x6c),
            Instr::I32DivS => bytes.push(0x6d),
            Instr::I32DivU => bytes.push(0x6e),
            Instr::I32RemS => bytes.push(0x6f),
            Instr::I32RemU => bytes.push(0x70),
            Instr::I32And => bytes.push(0x71),
            Instr::I32Or => bytes.push(0x72),
            Instr::I32Xor => bytes.push(0x73),
            Instr::I32ShL => bytes.push(0x74),
            Instr::I32ShrS => bytes.push(0x75),
            Instr::I32ShrU => bytes.push(0x76),
            Instr::I32Rotl => bytes.push(0x77),
            Instr::I32Rotr => bytes.push(0x78),
            Instr::I64Clz => bytes.push(0x79),
            Instr::I64Ctz => bytes.push(0x7a),
            Instr::I64Popcnt => bytes.push(0x7b),
            Instr::I64Add => bytes.push(0x7c),
            Instr::I64Sub => bytes.push(0x7d),
            Instr::I64Mul => bytes.push(0x7e),
            Instr::I64DivS => bytes.push(0x7f),
            Instr::I64DivU => bytes.push(0x80),
            Instr::I64RemS => bytes.push(0x81),
            Instr::I64RemU => bytes.push(0x82),
            Instr::I64And => bytes.push(0x83),
            Instr::I64Or => bytes.push(0x84),
            Instr::I64Xor => bytes.push(0x85),
            Instr::I64ShL => bytes.push(0x86),
            Instr::I64ShrS => bytes.push(0x87),
            Instr::I64ShrU => bytes.push(0x88),
            Instr::I64Rotl => bytes.push(0x89),
            Instr::I64Rotr => bytes.push(0x8a),
            Instr::F32Abs => bytes.push(0x8b),
            Instr::F32Neg => bytes.push(0x8c),
            Instr::F32Ceil => bytes.push(0x8d),
            Instr::F32Floor => bytes.push(0x8e),
            Instr::F32Trunc => bytes.push(0x8f),
            Instr::F32Nearest => bytes.push(0x90),
            Instr::F32Sqrt => bytes.push(0x91),
            Instr::F32Add => bytes.push(0x92),
            Instr::F32Sub => bytes.push(0x93),
            Instr::F32Mul => bytes.push(0x94),
            Instr::F32Div => bytes.push(0x95),
            Instr::F32Min => bytes.push(0x96),
            Instr::F32Max => bytes.push(0x97),
            Instr::F32CopySign => bytes.push(0x98),
            Instr::F64Abs => bytes.push(0x99),
            Instr::F64Neg => bytes.push(0x9a),
            Instr::F64Ceil => bytes.push(0x9b),
            Instr::F64Floor => bytes.push(0x9c),
            Instr::F64Trunc => bytes.push(0x9d),
            Instr::F64Nearest => bytes.push(0x9e),
            Instr::F64Sqrt => bytes.push(0x9f),
            Instr::F64Add => bytes.push(0xa0),
            Instr::F64Sub => bytes.push(0xa1),
            Instr::F64Mul => bytes.push(0xa2),
            Instr::F64Div => bytes.push(0xa3),
            Instr::F64Min => bytes.push(0xa4),
            Instr::F64Max => bytes.push(0xa5),
            Instr::F64CopySign => bytes.push(0xa6),
            Instr::I32WrapI64 => bytes.push(0xa7),
            Instr::I32TruncF32S => bytes.push(0xa8),
            Instr::I32TruncF32U => bytes.push(0xa9),
            Instr::I32TruncF64S => bytes.push(0xaa),
            Instr::I32TruncF64U => bytes.push(0xab),
            Instr::I64ExtendI32S => bytes.push(0xac),
            Instr::I64ExtendI32U => bytes.push(0xad),
            Instr::I64TruncF32S => bytes.push(0xae),
            Instr::I64TruncF32U => bytes.push(0xaf),
            Instr::I64TruncF64S => bytes.push(0xb0),
            Instr::I64TruncF64U => bytes.push(0xb1),
            Instr::F32ConvertI32S => bytes.push(0xb2),
            Instr::F32ConvertI32U => bytes.push(0xb3),
            Instr::F32ConvertI64S => bytes.push(0xb4),
            Instr::F32ConvertI64U => bytes.push(0xb5),
            Instr::F32DemoteF64 => bytes.push(0xb6),
            Instr::F64ConvertI32S => bytes.push(0xb7),
            Instr::F64ConvertI32U => bytes.push(0xb8),
            Instr::F64ConvertI64S => bytes.push(0xb9),
            Instr::F64ConvertI64U => bytes.push(0xba),
            Instr::F64PromoteF32 => bytes.push(0xbb),
            Instr::I32ReinteretF32 => bytes.push(0xbc),
            Instr::I64ReinteretF64 => bytes.push(0xbd),
            Instr::F32ReinteretI32 => bytes.push(0xbe),
            Instr::F64ReinteretI64 => bytes.push(0xbf),
        }
    }
}

impl Encoder for Memarg {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.align.encode(bytes);
        self.offset.encode(bytes);
    }
}
