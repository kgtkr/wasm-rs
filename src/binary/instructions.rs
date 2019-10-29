use super::parser;
use super::util::loop_encode;
use super::Encoder;
use crate::structure::instructions::{Expr, Instr, Memarg};
use crate::structure::modules::{
    FuncIdx, GlobalIdx, LabelIdx, LocalIdx, MemIdx, TableIdx, TypeIdx,
};
use crate::structure::types::{
    ElemType, FuncType, GlobalType, Limits, MemType, Mut, ResultType, TableType, ValType,
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{map, map_res, opt},
    multi::{many0, many_m_n},
    sequence::tuple,
    IResult,
};

use super::Decoder;

macro_rules! alt_m {
  ($x:expr) => {
    $x
  };
  ($x:expr,) => {
    $x
  };
  (
    $a: expr,
    $b: expr,
    $c: expr,
    $d: expr,
    $e: expr,
    $f: expr,
    $g: expr,
    $h: expr,
    $i: expr,
    $j: expr,
    $k: expr,
    $l: expr,
    $m: expr,
    $n: expr,
    $o: expr,
    $p: expr,
    $q: expr,
    $r: expr,
    $s: expr,
    $t: expr,
    $u: expr,
    $v: expr,
    $w: expr,
    $x: expr,
    $y: expr,
    $($xs:tt)+) => {
    alt((
        alt(($a,
        $b,
        $c,
        $d,
        $e)),
        alt(($f,
        $g,
        $h,
        $i,
        $j)),
        alt(($k,
        $l,
        $m,
        $n,
        $o)),
        alt(($p,
        $q,
        $r,
        $s,
        $t)),
        alt(($u,
        $v,
        $w,
        $x,
        $y)),
        alt_m!($($xs)+)))
  };
  ($x1:expr, $x2:expr, $x3:expr, $x4:expr, $x5:expr, $x6:expr, $x7:expr, $x8:expr, $($xs:tt)+) => {
    alt(($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, alt_m!($($xs)+)))
  };
  ($x1:expr, $x2:expr, $x3:expr, $x4:expr, $x5:expr, $x6:expr, $x7:expr, $($xs:tt)+) => {
    alt(($x1, $x2, $x3, $x4, $x5, $x6, $x7, alt_m!($($xs)+)))
  };
  ($x1:expr, $x2:expr, $x3:expr, $x4:expr, $x5:expr, $x6:expr, $($xs:tt)+) => {
    alt(($x1, $x2, $x3, $x4, $x5, $x6, alt_m!($($xs)+)))
  };
  ($x1:expr, $x2:expr, $x3:expr, $x4:expr, $x5:expr, $($xs:tt)+) => {
    alt(($x1, $x2, $x3, $x4, $x5, alt_m!($($xs)+)))
  };
  ($x1:expr, $x2:expr, $x3:expr, $x4:expr, $($xs:tt)+) => {
    alt(($x1, $x2, $x3, $x4, alt_m!($($xs)+)))
  };
  ($x1:expr, $x2:expr, $x3:expr, $($xs:tt)+) => {
    alt(($x1, $x2, $x3 ,alt_m!($($xs)+)))
  };
  ($x1:expr, $x2:expr, $($xs:tt)+) => {
    alt(($x1, $x2 ,alt_m!($($xs)+)))
  };
  ($x:expr, $($xs:tt)+) => {
    alt(($x,alt_m!($($xs)+)))
  };
}

impl Encoder for Expr {
    fn encode(&self, bytes: &mut Vec<u8>) {
        loop_encode(&self.0, bytes);
    }
}

impl Decoder for Expr {
    fn decode(input: &[u8]) -> IResult<&[u8], Expr> {
        let mut deep = 0;
        let mut input = input;
        let mut instrs = Vec::new();
        while deep >= 0 {
            let (input2, instr) = Instr::decode(input)?;
            input = input2;
            deep += instr.nest_value();
            instrs.push(instr);
        }
        Ok((input, Expr(instrs)))
    }
}

impl Encoder for Instr {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self {
            Instr::Unreachable => bytes.push(0x00),
            Instr::Nop => bytes.push(0x01),
            Instr::Block(rt) => {
                bytes.push(0x02);
                rt.encode(bytes);
            }
            Instr::Loop(rt) => {
                bytes.push(0x03);
                rt.encode(bytes);
            }
            Instr::If(rt) => {
                bytes.push(0x04);
                rt.encode(bytes);
            }
            Instr::Else => {
                bytes.push(0x05);
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
            Instr::End => bytes.push(0x0b),
        }
    }
}

impl Decoder for Instr {
    fn decode(input: &[u8]) -> IResult<&[u8], Instr> {
        alt_m!(
            map(parser::token(0x00), |_| Instr::Unreachable),
            map(parser::token(0x01), |_| Instr::Nop),
            map(
                tuple((parser::token(0x02), ResultType::decode,)),
                |(_, rt)| Instr::Block(rt),
            ),
            map(
                tuple((parser::token(0x03), ResultType::decode,)),
                |(_, rt)| Instr::Loop(rt),
            ),
            map(
                tuple((parser::token(0x04), ResultType::decode,)),
                |(_, rt)| Instr::If(rt),
            ),
            map(parser::token(0x05), |_| Instr::Else,),
            map(tuple((parser::token(0x0c), LabelIdx::decode)), |(_, l)| {
                Instr::Br(l)
            }),
            map(tuple((parser::token(0x0d), LabelIdx::decode)), |(_, l)| {
                Instr::BrIf(l)
            }),
            map(
                tuple((
                    parser::token(0x0e),
                    super::values::p_vec(LabelIdx::decode),
                    LabelIdx::decode
                )),
                |(_, ls, l)| Instr::BrTable(ls, l),
            ),
            map(parser::token(0x0f), |_| Instr::Return),
            map(tuple((parser::token(0x10), FuncIdx::decode)), |(_, f)| {
                Instr::Call(f)
            }),
            map(
                tuple((parser::token(0x11), TypeIdx::decode, parser::token(0x00))),
                |(_, t, _)| Instr::CallIndirect(t),
            ),
            map(parser::token(0x1a), |_| Instr::Drop),
            map(parser::token(0x1b), |_| Instr::Select),
            map(tuple((parser::token(0x20), LocalIdx::decode)), |(_, x)| {
                Instr::LocalGet(x)
            }),
            map(tuple((parser::token(0x21), LocalIdx::decode)), |(_, x)| {
                Instr::LocalSet(x)
            }),
            map(tuple((parser::token(0x22), LocalIdx::decode)), |(_, x)| {
                Instr::LocalTee(x)
            }),
            map(tuple((parser::token(0x23), GlobalIdx::decode)), |(_, x)| {
                Instr::GlobalGet(x)
            }),
            map(tuple((parser::token(0x24), GlobalIdx::decode)), |(_, x)| {
                Instr::GlobalSet(x)
            }),
            map(tuple((parser::token(0x28), Memarg::decode)), |(_, m)| {
                Instr::I32Load(m)
            }),
            map(tuple((parser::token(0x29), Memarg::decode)), |(_, m)| {
                Instr::I64Load(m)
            }),
            map(tuple((parser::token(0x2a), Memarg::decode)), |(_, m)| {
                Instr::F32Load(m)
            }),
            map(tuple((parser::token(0x2a), Memarg::decode)), |(_, m)| {
                Instr::F32Load(m)
            }),
            map(tuple((parser::token(0x2b), Memarg::decode)), |(_, m)| {
                Instr::F64Load(m)
            }),
            map(tuple((parser::token(0x2c), Memarg::decode)), |(_, m)| {
                Instr::I32Load8S(m)
            }),
            map(tuple((parser::token(0x2d), Memarg::decode)), |(_, m)| {
                Instr::I32Load8U(m)
            }),
            map(tuple((parser::token(0x2e), Memarg::decode)), |(_, m)| {
                Instr::I32Load16S(m)
            }),
            map(tuple((parser::token(0x2f), Memarg::decode)), |(_, m)| {
                Instr::I32Load16U(m)
            }),
            map(tuple((parser::token(0x30), Memarg::decode)), |(_, m)| {
                Instr::I64Load8S(m)
            }),
            map(tuple((parser::token(0x31), Memarg::decode)), |(_, m)| {
                Instr::I64Load8U(m)
            }),
            map(tuple((parser::token(0x32), Memarg::decode)), |(_, m)| {
                Instr::I64Load16S(m)
            }),
            map(tuple((parser::token(0x33), Memarg::decode)), |(_, m)| {
                Instr::I64Load16U(m)
            }),
            map(tuple((parser::token(0x34), Memarg::decode)), |(_, m)| {
                Instr::I64Load32S(m)
            }),
            map(tuple((parser::token(0x35), Memarg::decode)), |(_, m)| {
                Instr::I64Load32U(m)
            }),
            map(tuple((parser::token(0x36), Memarg::decode)), |(_, m)| {
                Instr::I32Store(m)
            }),
            map(tuple((parser::token(0x37), Memarg::decode)), |(_, m)| {
                Instr::I64Store(m)
            }),
            map(tuple((parser::token(0x38), Memarg::decode)), |(_, m)| {
                Instr::F32Store(m)
            }),
            map(tuple((parser::token(0x39), Memarg::decode)), |(_, m)| {
                Instr::F64Store(m)
            }),
            map(tuple((parser::token(0x3a), Memarg::decode)), |(_, m)| {
                Instr::I32Store8(m)
            }),
            map(tuple((parser::token(0x3b), Memarg::decode)), |(_, m)| {
                Instr::I32Store16(m)
            }),
            map(tuple((parser::token(0x3c), Memarg::decode)), |(_, m)| {
                Instr::I64Store8(m)
            }),
            map(tuple((parser::token(0x3d), Memarg::decode)), |(_, m)| {
                Instr::I64Store16(m)
            }),
            map(tuple((parser::token(0x3e), Memarg::decode)), |(_, m)| {
                Instr::I64Store32(m)
            }),
            map(tuple((parser::token(0x3f), parser::token(0x00))), |_| {
                Instr::MemorySize
            }),
            map(tuple((parser::token(0x40), parser::token(0x00))), |_| {
                Instr::MemoryGrow
            }),
            map(tuple((parser::token(0x41), i32::decode)), |(_, n)| {
                Instr::I32Const(n)
            }),
            map(tuple((parser::token(0x42), i64::decode)), |(_, n)| {
                Instr::I64Const(n)
            }),
            map(tuple((parser::token(0x43), f32::decode)), |(_, z)| {
                Instr::F32Const(z)
            }),
            map(tuple((parser::token(0x44), f64::decode)), |(_, z)| {
                Instr::F64Const(z)
            }),
            map(parser::token(0x45), |_| Instr::I32Eqz),
            map(parser::token(0x46), |_| Instr::I32Eq),
            map(parser::token(0x47), |_| Instr::I32Ne),
            map(parser::token(0x48), |_| Instr::I32LtS),
            map(parser::token(0x49), |_| Instr::I32LtU),
            map(parser::token(0x4a), |_| Instr::I32GtS),
            map(parser::token(0x4b), |_| Instr::I32GtU),
            map(parser::token(0x4c), |_| Instr::I32LeS),
            map(parser::token(0x4d), |_| Instr::I32LeU),
            map(parser::token(0x4e), |_| Instr::I32GeS),
            map(parser::token(0x4f), |_| Instr::I32GeU),
            map(parser::token(0x50), |_| Instr::I64Eqz),
            map(parser::token(0x51), |_| Instr::I64Eq),
            map(parser::token(0x52), |_| Instr::I64Ne),
            map(parser::token(0x53), |_| Instr::I64LtS),
            map(parser::token(0x54), |_| Instr::I64LtU),
            map(parser::token(0x55), |_| Instr::I64GtS),
            map(parser::token(0x56), |_| Instr::I64GtU),
            map(parser::token(0x57), |_| Instr::I64LeS),
            map(parser::token(0x58), |_| Instr::I64LeU),
            map(parser::token(0x59), |_| Instr::I64GeS),
            map(parser::token(0x5a), |_| Instr::I64GeU),
            map(parser::token(0x5b), |_| Instr::F32Eq),
            map(parser::token(0x5c), |_| Instr::F32Ne),
            map(parser::token(0x5d), |_| Instr::F32Lt),
            map(parser::token(0x5e), |_| Instr::F32Gt),
            map(parser::token(0x5f), |_| Instr::F32Le),
            map(parser::token(0x60), |_| Instr::F32Ge),
            map(parser::token(0x61), |_| Instr::F64Eq),
            map(parser::token(0x62), |_| Instr::F64Ne),
            map(parser::token(0x63), |_| Instr::F64Lt),
            map(parser::token(0x64), |_| Instr::F64Gt),
            map(parser::token(0x65), |_| Instr::F64Le),
            map(parser::token(0x66), |_| Instr::F64Ge),
            map(parser::token(0x67), |_| Instr::I32Clz),
            map(parser::token(0x68), |_| Instr::I32Ctz),
            map(parser::token(0x69), |_| Instr::I32Popcnt),
            map(parser::token(0x6a), |_| Instr::I32Add),
            map(parser::token(0x6b), |_| Instr::I32Sub),
            map(parser::token(0x6c), |_| Instr::I32Mul),
            map(parser::token(0x6d), |_| Instr::I32DivS),
            map(parser::token(0x6e), |_| Instr::I32DivU),
            map(parser::token(0x6f), |_| Instr::I32RemS),
            map(parser::token(0x70), |_| Instr::I32RemU),
            map(parser::token(0x71), |_| Instr::I32And),
            map(parser::token(0x72), |_| Instr::I32Or),
            map(parser::token(0x73), |_| Instr::I32Xor),
            map(parser::token(0x74), |_| Instr::I32ShL),
            map(parser::token(0x75), |_| Instr::I32ShrS),
            map(parser::token(0x76), |_| Instr::I32ShrU),
            map(parser::token(0x77), |_| Instr::I32Rotl),
            map(parser::token(0x78), |_| Instr::I32Rotr),
            map(parser::token(0x79), |_| Instr::I64Clz),
            map(parser::token(0x7a), |_| Instr::I64Ctz),
            map(parser::token(0x7b), |_| Instr::I64Popcnt),
            map(parser::token(0x7c), |_| Instr::I64Add),
            map(parser::token(0x7d), |_| Instr::I64Sub),
            map(parser::token(0x7e), |_| Instr::I64Mul),
            map(parser::token(0x7f), |_| Instr::I64DivS),
            map(parser::token(0x80), |_| Instr::I64DivU),
            map(parser::token(0x81), |_| Instr::I64RemS),
            map(parser::token(0x82), |_| Instr::I64RemU),
            map(parser::token(0x83), |_| Instr::I64And),
            map(parser::token(0x84), |_| Instr::I64Or),
            map(parser::token(0x85), |_| Instr::I64Xor),
            map(parser::token(0x86), |_| Instr::I64ShL),
            map(parser::token(0x87), |_| Instr::I64ShrS),
            map(parser::token(0x88), |_| Instr::I64ShrU),
            map(parser::token(0x89), |_| Instr::I64Rotl),
            map(parser::token(0x8a), |_| Instr::I64Rotr),
            map(parser::token(0x8b), |_| Instr::F32Abs),
            map(parser::token(0x8c), |_| Instr::F32Neg),
            map(parser::token(0x8d), |_| Instr::F32Ceil),
            map(parser::token(0x8e), |_| Instr::F32Floor),
            map(parser::token(0x8f), |_| Instr::F32Trunc),
            map(parser::token(0x90), |_| Instr::F32Nearest),
            map(parser::token(0x91), |_| Instr::F32Sqrt),
            map(parser::token(0x92), |_| Instr::F32Add),
            map(parser::token(0x93), |_| Instr::F32Sub),
            map(parser::token(0x94), |_| Instr::F32Mul),
            map(parser::token(0x95), |_| Instr::F32Div),
            map(parser::token(0x96), |_| Instr::F32Min),
            map(parser::token(0x97), |_| Instr::F32Max),
            map(parser::token(0x98), |_| Instr::F32CopySign),
            map(parser::token(0x99), |_| Instr::F64Abs),
            map(parser::token(0x9a), |_| Instr::F64Neg),
            map(parser::token(0x9b), |_| Instr::F64Ceil),
            map(parser::token(0x9c), |_| Instr::F64Floor),
            map(parser::token(0x9d), |_| Instr::F64Trunc),
            map(parser::token(0x9e), |_| Instr::F64Nearest),
            map(parser::token(0x9f), |_| Instr::F64Sqrt),
            map(parser::token(0xa0), |_| Instr::F64Add),
            map(parser::token(0xa1), |_| Instr::F64Sub),
            map(parser::token(0xa2), |_| Instr::F64Mul),
            map(parser::token(0xa3), |_| Instr::F64Div),
            map(parser::token(0xa4), |_| Instr::F64Min),
            map(parser::token(0xa5), |_| Instr::F64Max),
            map(parser::token(0xa6), |_| Instr::F64CopySign),
            map(parser::token(0xa7), |_| Instr::I32WrapI64),
            map(parser::token(0xa8), |_| Instr::I32TruncF32S),
            map(parser::token(0xa9), |_| Instr::I32TruncF32U),
            map(parser::token(0xaa), |_| Instr::I32TruncF64S),
            map(parser::token(0xab), |_| Instr::I32TruncF64U),
            map(parser::token(0xac), |_| Instr::I64ExtendI32S),
            map(parser::token(0xad), |_| Instr::I64ExtendI32U),
            map(parser::token(0xae), |_| Instr::I64TruncF32S),
            map(parser::token(0xaf), |_| Instr::I64TruncF32U),
            map(parser::token(0xb0), |_| Instr::I64TruncF64S),
            map(parser::token(0xb1), |_| Instr::I64TruncF64U),
            map(parser::token(0xb2), |_| Instr::F32ConvertI32S),
            map(parser::token(0xb3), |_| Instr::F32ConvertI32U),
            map(parser::token(0xb4), |_| Instr::F32ConvertI64S),
            map(parser::token(0xb5), |_| Instr::F32ConvertI64U),
            map(parser::token(0xb6), |_| Instr::F32DemoteF64),
            map(parser::token(0xb7), |_| Instr::F64ConvertI32S),
            map(parser::token(0xb8), |_| Instr::F64ConvertI32U),
            map(parser::token(0xb9), |_| Instr::F64ConvertI64S),
            map(parser::token(0xba), |_| Instr::F64ConvertI64U),
            map(parser::token(0xbb), |_| Instr::F64PromoteF32),
            map(parser::token(0xbc), |_| Instr::I32ReinteretF32),
            map(parser::token(0xbd), |_| Instr::I64ReinteretF64),
            map(parser::token(0xbe), |_| Instr::F32ReinteretI32),
            map(parser::token(0xbf), |_| Instr::F64ReinteretI64),
            map(parser::token(0x0b), |_| Instr::End),
        )(input)
    }
}

impl Encoder for Memarg {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.align.encode(bytes);
        self.offset.encode(bytes);
    }
}

impl Decoder for Memarg {
    fn decode(input: &[u8]) -> IResult<&[u8], Memarg> {
        map(tuple((u32::decode, u32::decode)), |(align, offset)| {
            Memarg { align, offset }
        })(input)
    }
}

#[test]
fn tests() {
    use super::test_helper;

    // test_helper::identity_encode_decode::<Expr>();
    // test_helper::identity_encode_decode::<Instr>();
    test_helper::identity_encode_decode::<Memarg>();
}
