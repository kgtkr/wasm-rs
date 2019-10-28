use super::parser;
use super::util::loop_encode;
use super::Encoder;
use crate::structure::instructions::{
    ControlInstr, Expr, Instr, Memarg, MemoryInstr, NumericInstr, ParametricInstr, VariableInstr,
};
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
        bytes.push(0x0b);
    }
}

impl Decoder for Expr {
    fn decode(input: &[u8]) -> IResult<&[u8], Expr> {
        map(
            tuple((many0(Instr::decode), parser::token(0x0b))),
            |(is, _)| Expr(is),
        )(input)
    }
}

impl Encoder for NumericInstr {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self {
            NumericInstr::I32Const(n) => {
                bytes.push(0x41);
                n.encode(bytes);
            }
            NumericInstr::I64Const(n) => {
                bytes.push(0x42);
                n.encode(bytes);
            }
            NumericInstr::F32Const(z) => {
                bytes.push(0x43);
                z.encode(bytes);
            }
            NumericInstr::F64Const(z) => {
                bytes.push(0x44);
                z.encode(bytes);
            }
            NumericInstr::I32Eqz => bytes.push(0x45),
            NumericInstr::I32Eq => bytes.push(0x46),
            NumericInstr::I32Ne => bytes.push(0x47),
            NumericInstr::I32LtS => bytes.push(0x48),
            NumericInstr::I32LtU => bytes.push(0x49),
            NumericInstr::I32GtS => bytes.push(0x4a),
            NumericInstr::I32GtU => bytes.push(0x4b),
            NumericInstr::I32LeS => bytes.push(0x4c),
            NumericInstr::I32LeU => bytes.push(0x4d),
            NumericInstr::I32GeS => bytes.push(0x4e),
            NumericInstr::I32GeU => bytes.push(0x4f),
            NumericInstr::I64Eqz => bytes.push(0x50),
            NumericInstr::I64Eq => bytes.push(0x51),
            NumericInstr::I64Ne => bytes.push(0x52),
            NumericInstr::I64LtS => bytes.push(0x53),
            NumericInstr::I64LtU => bytes.push(0x54),
            NumericInstr::I64GtS => bytes.push(0x55),
            NumericInstr::I64GtU => bytes.push(0x56),
            NumericInstr::I64LeS => bytes.push(0x57),
            NumericInstr::I64LeU => bytes.push(0x58),
            NumericInstr::I64GeS => bytes.push(0x59),
            NumericInstr::I64GeU => bytes.push(0x5a),
            NumericInstr::F32Eq => bytes.push(0x5b),
            NumericInstr::F32Ne => bytes.push(0x5c),
            NumericInstr::F32Lt => bytes.push(0x5d),
            NumericInstr::F32Gt => bytes.push(0x5e),
            NumericInstr::F32Le => bytes.push(0x5f),
            NumericInstr::F32Ge => bytes.push(0x60),
            NumericInstr::F64Eq => bytes.push(0x61),
            NumericInstr::F64Ne => bytes.push(0x62),
            NumericInstr::F64Lt => bytes.push(0x63),
            NumericInstr::F64Gt => bytes.push(0x64),
            NumericInstr::F64Le => bytes.push(0x65),
            NumericInstr::F64Ge => bytes.push(0x66),
            NumericInstr::I32Clz => bytes.push(0x67),
            NumericInstr::I32Ctz => bytes.push(0x68),
            NumericInstr::I32Popcnt => bytes.push(0x69),
            NumericInstr::I32Add => bytes.push(0x6a),
            NumericInstr::I32Sub => bytes.push(0x6b),
            NumericInstr::I32Mul => bytes.push(0x6c),
            NumericInstr::I32DivS => bytes.push(0x6d),
            NumericInstr::I32DivU => bytes.push(0x6e),
            NumericInstr::I32RemS => bytes.push(0x6f),
            NumericInstr::I32RemU => bytes.push(0x70),
            NumericInstr::I32And => bytes.push(0x71),
            NumericInstr::I32Or => bytes.push(0x72),
            NumericInstr::I32Xor => bytes.push(0x73),
            NumericInstr::I32ShL => bytes.push(0x74),
            NumericInstr::I32ShrS => bytes.push(0x75),
            NumericInstr::I32ShrU => bytes.push(0x76),
            NumericInstr::I32Rotl => bytes.push(0x77),
            NumericInstr::I32Rotr => bytes.push(0x78),
            NumericInstr::I64Clz => bytes.push(0x79),
            NumericInstr::I64Ctz => bytes.push(0x7a),
            NumericInstr::I64Popcnt => bytes.push(0x7b),
            NumericInstr::I64Add => bytes.push(0x7c),
            NumericInstr::I64Sub => bytes.push(0x7d),
            NumericInstr::I64Mul => bytes.push(0x7e),
            NumericInstr::I64DivS => bytes.push(0x7f),
            NumericInstr::I64DivU => bytes.push(0x80),
            NumericInstr::I64RemS => bytes.push(0x81),
            NumericInstr::I64RemU => bytes.push(0x82),
            NumericInstr::I64And => bytes.push(0x83),
            NumericInstr::I64Or => bytes.push(0x84),
            NumericInstr::I64Xor => bytes.push(0x85),
            NumericInstr::I64ShL => bytes.push(0x86),
            NumericInstr::I64ShrS => bytes.push(0x87),
            NumericInstr::I64ShrU => bytes.push(0x88),
            NumericInstr::I64Rotl => bytes.push(0x89),
            NumericInstr::I64Rotr => bytes.push(0x8a),
            NumericInstr::F32Abs => bytes.push(0x8b),
            NumericInstr::F32Neg => bytes.push(0x8c),
            NumericInstr::F32Ceil => bytes.push(0x8d),
            NumericInstr::F32Floor => bytes.push(0x8e),
            NumericInstr::F32Trunc => bytes.push(0x8f),
            NumericInstr::F32Nearest => bytes.push(0x90),
            NumericInstr::F32Sqrt => bytes.push(0x91),
            NumericInstr::F32Add => bytes.push(0x92),
            NumericInstr::F32Sub => bytes.push(0x93),
            NumericInstr::F32Mul => bytes.push(0x94),
            NumericInstr::F32Div => bytes.push(0x95),
            NumericInstr::F32Min => bytes.push(0x96),
            NumericInstr::F32Max => bytes.push(0x97),
            NumericInstr::F32CopySign => bytes.push(0x98),
            NumericInstr::F64Abs => bytes.push(0x99),
            NumericInstr::F64Neg => bytes.push(0x9a),
            NumericInstr::F64Ceil => bytes.push(0x9b),
            NumericInstr::F64Floor => bytes.push(0x9c),
            NumericInstr::F64Trunc => bytes.push(0x9d),
            NumericInstr::F64Nearest => bytes.push(0x9e),
            NumericInstr::F64Sqrt => bytes.push(0x9f),
            NumericInstr::F64Add => bytes.push(0xa0),
            NumericInstr::F64Sub => bytes.push(0xa1),
            NumericInstr::F64Mul => bytes.push(0xa2),
            NumericInstr::F64Div => bytes.push(0xa3),
            NumericInstr::F64Min => bytes.push(0xa4),
            NumericInstr::F64Max => bytes.push(0xa5),
            NumericInstr::F64CopySign => bytes.push(0xa6),
            NumericInstr::I32WrapI64 => bytes.push(0xa7),
            NumericInstr::I32TruncF32S => bytes.push(0xa8),
            NumericInstr::I32TruncF32U => bytes.push(0xa9),
            NumericInstr::I32TruncF64S => bytes.push(0xaa),
            NumericInstr::I32TruncF64U => bytes.push(0xab),
            NumericInstr::I64ExtendI32S => bytes.push(0xac),
            NumericInstr::I64ExtendI32U => bytes.push(0xad),
            NumericInstr::I64TruncF32S => bytes.push(0xae),
            NumericInstr::I64TruncF32U => bytes.push(0xaf),
            NumericInstr::I64TruncF64S => bytes.push(0xb0),
            NumericInstr::I64TruncF64U => bytes.push(0xb1),
            NumericInstr::F32ConvertI32S => bytes.push(0xb2),
            NumericInstr::F32ConvertI32U => bytes.push(0xb3),
            NumericInstr::F32ConvertI64S => bytes.push(0xb4),
            NumericInstr::F32ConvertI64U => bytes.push(0xb5),
            NumericInstr::F32DemoteF64 => bytes.push(0xb6),
            NumericInstr::F64ConvertI32S => bytes.push(0xb7),
            NumericInstr::F64ConvertI32U => bytes.push(0xb8),
            NumericInstr::F64ConvertI64S => bytes.push(0xb9),
            NumericInstr::F64ConvertI64U => bytes.push(0xba),
            NumericInstr::F64PromoteF32 => bytes.push(0xbb),
            NumericInstr::I32ReinteretF32 => bytes.push(0xbc),
            NumericInstr::I64ReinteretF64 => bytes.push(0xbd),
            NumericInstr::F32ReinteretI32 => bytes.push(0xbe),
            NumericInstr::F64ReinteretI64 => bytes.push(0xbf),
        }
    }
}

impl Decoder for NumericInstr {
    fn decode(input: &[u8]) -> IResult<&[u8], NumericInstr> {
        alt_m!(
            map(tuple((parser::token(0x41), i32::decode)), |(_, n)| {
                NumericInstr::I32Const(n)
            }),
            map(tuple((parser::token(0x42), i64::decode)), |(_, n)| {
                NumericInstr::I64Const(n)
            }),
            map(tuple((parser::token(0x43), f32::decode)), |(_, z)| {
                NumericInstr::F32Const(z)
            }),
            map(tuple((parser::token(0x44), f64::decode)), |(_, z)| {
                NumericInstr::F64Const(z)
            }),
            map(parser::token(0x45), |_| NumericInstr::I32Eqz),
            map(parser::token(0x46), |_| NumericInstr::I32Eq),
            map(parser::token(0x47), |_| NumericInstr::I32Ne),
            map(parser::token(0x48), |_| NumericInstr::I32LtS),
            map(parser::token(0x49), |_| NumericInstr::I32LtU),
            map(parser::token(0x4a), |_| NumericInstr::I32GtS),
            map(parser::token(0x4b), |_| NumericInstr::I32GtU),
            map(parser::token(0x4c), |_| NumericInstr::I32LeS),
            map(parser::token(0x4d), |_| NumericInstr::I32LeU),
            map(parser::token(0x4e), |_| NumericInstr::I32GeS),
            map(parser::token(0x4f), |_| NumericInstr::I32GeU),
            map(parser::token(0x50), |_| NumericInstr::I64Eqz),
            map(parser::token(0x51), |_| NumericInstr::I64Eq),
            map(parser::token(0x52), |_| NumericInstr::I64Ne),
            map(parser::token(0x53), |_| NumericInstr::I64LtS),
            map(parser::token(0x54), |_| NumericInstr::I64LtU),
            map(parser::token(0x55), |_| NumericInstr::I64GtS),
            map(parser::token(0x56), |_| NumericInstr::I64GtU),
            map(parser::token(0x57), |_| NumericInstr::I64LeS),
            map(parser::token(0x58), |_| NumericInstr::I64LeU),
            map(parser::token(0x59), |_| NumericInstr::I64GeS),
            map(parser::token(0x5a), |_| NumericInstr::I64GeU),
            map(parser::token(0x5b), |_| NumericInstr::F32Eq),
            map(parser::token(0x5c), |_| NumericInstr::F32Ne),
            map(parser::token(0x5d), |_| NumericInstr::F32Lt),
            map(parser::token(0x5e), |_| NumericInstr::F32Gt),
            map(parser::token(0x5f), |_| NumericInstr::F32Le),
            map(parser::token(0x60), |_| NumericInstr::F32Ge),
            map(parser::token(0x61), |_| NumericInstr::F64Eq),
            map(parser::token(0x62), |_| NumericInstr::F64Ne),
            map(parser::token(0x63), |_| NumericInstr::F64Lt),
            map(parser::token(0x64), |_| NumericInstr::F64Gt),
            map(parser::token(0x65), |_| NumericInstr::F64Le),
            map(parser::token(0x66), |_| NumericInstr::F64Ge),
            map(parser::token(0x67), |_| NumericInstr::I32Clz),
            map(parser::token(0x68), |_| NumericInstr::I32Ctz),
            map(parser::token(0x69), |_| NumericInstr::I32Popcnt),
            map(parser::token(0x6a), |_| NumericInstr::I32Add),
            map(parser::token(0x6b), |_| NumericInstr::I32Sub),
            map(parser::token(0x6c), |_| NumericInstr::I32Mul),
            map(parser::token(0x6d), |_| NumericInstr::I32DivS),
            map(parser::token(0x6e), |_| NumericInstr::I32DivU),
            map(parser::token(0x6f), |_| NumericInstr::I32RemS),
            map(parser::token(0x70), |_| NumericInstr::I32RemU),
            map(parser::token(0x71), |_| NumericInstr::I32And),
            map(parser::token(0x72), |_| NumericInstr::I32Or),
            map(parser::token(0x73), |_| NumericInstr::I32Xor),
            map(parser::token(0x74), |_| NumericInstr::I32ShL),
            map(parser::token(0x75), |_| NumericInstr::I32ShrS),
            map(parser::token(0x76), |_| NumericInstr::I32ShrU),
            map(parser::token(0x77), |_| NumericInstr::I32Rotl),
            map(parser::token(0x78), |_| NumericInstr::I32Rotr),
            map(parser::token(0x79), |_| NumericInstr::I64Clz),
            map(parser::token(0x7a), |_| NumericInstr::I64Ctz),
            map(parser::token(0x7b), |_| NumericInstr::I64Popcnt),
            map(parser::token(0x7c), |_| NumericInstr::I64Add),
            map(parser::token(0x7d), |_| NumericInstr::I64Sub),
            map(parser::token(0x7e), |_| NumericInstr::I64Mul),
            map(parser::token(0x7f), |_| NumericInstr::I64DivS),
            map(parser::token(0x80), |_| NumericInstr::I64DivU),
            map(parser::token(0x81), |_| NumericInstr::I64RemS),
            map(parser::token(0x82), |_| NumericInstr::I64RemU),
            map(parser::token(0x83), |_| NumericInstr::I64And),
            map(parser::token(0x84), |_| NumericInstr::I64Or),
            map(parser::token(0x85), |_| NumericInstr::I64Xor),
            map(parser::token(0x86), |_| NumericInstr::I64ShL),
            map(parser::token(0x87), |_| NumericInstr::I64ShrS),
            map(parser::token(0x88), |_| NumericInstr::I64ShrU),
            map(parser::token(0x89), |_| NumericInstr::I64Rotl),
            map(parser::token(0x8a), |_| NumericInstr::I64Rotr),
            map(parser::token(0x8b), |_| NumericInstr::F32Abs),
            map(parser::token(0x8c), |_| NumericInstr::F32Neg),
            map(parser::token(0x8d), |_| NumericInstr::F32Ceil),
            map(parser::token(0x8e), |_| NumericInstr::F32Floor),
            map(parser::token(0x8f), |_| NumericInstr::F32Trunc),
            map(parser::token(0x90), |_| NumericInstr::F32Nearest),
            map(parser::token(0x91), |_| NumericInstr::F32Sqrt),
            map(parser::token(0x92), |_| NumericInstr::F32Add),
            map(parser::token(0x93), |_| NumericInstr::F32Sub),
            map(parser::token(0x94), |_| NumericInstr::F32Mul),
            map(parser::token(0x95), |_| NumericInstr::F32Div),
            map(parser::token(0x96), |_| NumericInstr::F32Min),
            map(parser::token(0x97), |_| NumericInstr::F32Max),
            map(parser::token(0x98), |_| NumericInstr::F32CopySign),
            map(parser::token(0x99), |_| NumericInstr::F64Abs),
            map(parser::token(0x9a), |_| NumericInstr::F64Neg),
            map(parser::token(0x9b), |_| NumericInstr::F64Ceil),
            map(parser::token(0x9c), |_| NumericInstr::F64Floor),
            map(parser::token(0x9d), |_| NumericInstr::F64Trunc),
            map(parser::token(0x9e), |_| NumericInstr::F64Nearest),
            map(parser::token(0x9f), |_| NumericInstr::F64Sqrt),
            map(parser::token(0xa0), |_| NumericInstr::F64Add),
            map(parser::token(0xa1), |_| NumericInstr::F64Sub),
            map(parser::token(0xa2), |_| NumericInstr::F64Mul),
            map(parser::token(0xa3), |_| NumericInstr::F64Div),
            map(parser::token(0xa4), |_| NumericInstr::F64Min),
            map(parser::token(0xa5), |_| NumericInstr::F64Max),
            map(parser::token(0xa6), |_| NumericInstr::F64CopySign),
            map(parser::token(0xa7), |_| NumericInstr::I32WrapI64),
            map(parser::token(0xa8), |_| NumericInstr::I32TruncF32S),
            map(parser::token(0xa9), |_| NumericInstr::I32TruncF32U),
            map(parser::token(0xaa), |_| NumericInstr::I32TruncF64S),
            map(parser::token(0xab), |_| NumericInstr::I32TruncF64U),
            map(parser::token(0xac), |_| NumericInstr::I64ExtendI32S),
            map(parser::token(0xad), |_| NumericInstr::I64ExtendI32U),
            map(parser::token(0xae), |_| NumericInstr::I64TruncF32S),
            map(parser::token(0xaf), |_| NumericInstr::I64TruncF32U),
            map(parser::token(0xb0), |_| NumericInstr::I64TruncF64S),
            map(parser::token(0xb1), |_| NumericInstr::I64TruncF64U),
            map(parser::token(0xb2), |_| NumericInstr::F32ConvertI32S),
            map(parser::token(0xb3), |_| NumericInstr::F32ConvertI32U),
            map(parser::token(0xb4), |_| NumericInstr::F32ConvertI64S),
            map(parser::token(0xb5), |_| NumericInstr::F32ConvertI64U),
            map(parser::token(0xb6), |_| NumericInstr::F32DemoteF64),
            map(parser::token(0xb7), |_| NumericInstr::F64ConvertI32S),
            map(parser::token(0xb8), |_| NumericInstr::F64ConvertI32U),
            map(parser::token(0xb9), |_| NumericInstr::F64ConvertI64S),
            map(parser::token(0xba), |_| NumericInstr::F64ConvertI64U),
            map(parser::token(0xbb), |_| NumericInstr::F64PromoteF32),
            map(parser::token(0xbc), |_| NumericInstr::I32ReinteretF32),
            map(parser::token(0xbd), |_| NumericInstr::I64ReinteretF64),
            map(parser::token(0xbe), |_| NumericInstr::F32ReinteretI32),
            map(parser::token(0xbf), |_| NumericInstr::F64ReinteretI64),
        )(input)
    }
}

impl Encoder for ParametricInstr {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self {
            ParametricInstr::Drop => bytes.push(0x1a),
            ParametricInstr::Select => bytes.push(0x1b),
        }
    }
}

impl Decoder for ParametricInstr {
    fn decode(input: &[u8]) -> IResult<&[u8], ParametricInstr> {
        alt_m!(
            map(parser::token(0x1a), |_| ParametricInstr::Drop),
            map(parser::token(0x1b), |_| ParametricInstr::Select),
        )(input)
    }
}

impl Encoder for VariableInstr {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self {
            VariableInstr::LocalGet(x) => {
                bytes.push(0x20);
                x.encode(bytes);
            }
            VariableInstr::LocalSet(x) => {
                bytes.push(0x21);
                x.encode(bytes);
            }
            VariableInstr::LocalTee(x) => {
                bytes.push(0x22);
                x.encode(bytes);
            }
            VariableInstr::GlobalGet(x) => {
                bytes.push(0x23);
                x.encode(bytes);
            }
            VariableInstr::GlobalSet(x) => {
                bytes.push(0x24);
                x.encode(bytes);
            }
        }
    }
}

impl Decoder for VariableInstr {
    fn decode(input: &[u8]) -> IResult<&[u8], VariableInstr> {
        alt_m!(
            map(tuple((parser::token(0x20), LocalIdx::decode)), |(_, x)| {
                VariableInstr::LocalGet(x)
            }),
            map(tuple((parser::token(0x21), LocalIdx::decode)), |(_, x)| {
                VariableInstr::LocalSet(x)
            }),
            map(tuple((parser::token(0x22), LocalIdx::decode)), |(_, x)| {
                VariableInstr::LocalTee(x)
            }),
            map(tuple((parser::token(0x23), GlobalIdx::decode)), |(_, x)| {
                VariableInstr::GlobalGet(x)
            }),
            map(tuple((parser::token(0x24), GlobalIdx::decode)), |(_, x)| {
                VariableInstr::GlobalSet(x)
            }),
        )(input)
    }
}

impl Encoder for MemoryInstr {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self {
            MemoryInstr::I32Load(m) => {
                bytes.push(0x28);
                m.encode(bytes);
            }
            MemoryInstr::I64Load(m) => {
                bytes.push(0x29);
                m.encode(bytes);
            }
            MemoryInstr::F32Load(m) => {
                bytes.push(0x2a);
                m.encode(bytes);
            }
            MemoryInstr::F64Load(m) => {
                bytes.push(0x2b);
                m.encode(bytes);
            }
            MemoryInstr::I32Load8S(m) => {
                bytes.push(0x2c);
                m.encode(bytes);
            }
            MemoryInstr::I32Load8U(m) => {
                bytes.push(0x2d);
                m.encode(bytes);
            }
            MemoryInstr::I32Load16S(m) => {
                bytes.push(0x2e);
                m.encode(bytes);
            }
            MemoryInstr::I32Load16U(m) => {
                bytes.push(0x2f);
                m.encode(bytes);
            }
            MemoryInstr::I64Load8S(m) => {
                bytes.push(0x30);
                m.encode(bytes);
            }
            MemoryInstr::I64Load8U(m) => {
                bytes.push(0x31);
                m.encode(bytes);
            }
            MemoryInstr::I64Load16S(m) => {
                bytes.push(0x32);
                m.encode(bytes);
            }
            MemoryInstr::I64Load16U(m) => {
                bytes.push(0x33);
                m.encode(bytes);
            }
            MemoryInstr::I64Load32S(m) => {
                bytes.push(0x34);
                m.encode(bytes);
            }
            MemoryInstr::I64Load32U(m) => {
                bytes.push(0x35);
                m.encode(bytes);
            }
            MemoryInstr::I32Store(m) => {
                bytes.push(0x36);
                m.encode(bytes);
            }
            MemoryInstr::I64Store(m) => {
                bytes.push(0x37);
                m.encode(bytes);
            }
            MemoryInstr::F32Store(m) => {
                bytes.push(0x38);
                m.encode(bytes);
            }
            MemoryInstr::F64Store(m) => {
                bytes.push(0x39);
                m.encode(bytes);
            }
            MemoryInstr::I32Store8(m) => {
                bytes.push(0x3a);
                m.encode(bytes);
            }
            MemoryInstr::I32Store16(m) => {
                bytes.push(0x3b);
                m.encode(bytes);
            }
            MemoryInstr::I64Store8(m) => {
                bytes.push(0x3c);
                m.encode(bytes);
            }
            MemoryInstr::I64Store16(m) => {
                bytes.push(0x3d);
                m.encode(bytes);
            }
            MemoryInstr::I64Store32(m) => {
                bytes.push(0x3e);
                m.encode(bytes);
            }
            MemoryInstr::MemorySize => {
                bytes.push(0x3f);
                bytes.push(0x00);
            }
            MemoryInstr::MemoryGrow => {
                bytes.push(0x40);
                bytes.push(0x00);
            }
        }
    }
}

impl Decoder for MemoryInstr {
    fn decode(input: &[u8]) -> IResult<&[u8], MemoryInstr> {
        alt_m!(
            map(tuple((parser::token(0x28), Memarg::decode)), |(_, m)| {
                MemoryInstr::I32Load(m)
            }),
            map(tuple((parser::token(0x29), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Load(m)
            }),
            map(tuple((parser::token(0x2a), Memarg::decode)), |(_, m)| {
                MemoryInstr::F32Load(m)
            }),
            map(tuple((parser::token(0x2a), Memarg::decode)), |(_, m)| {
                MemoryInstr::F32Load(m)
            }),
            map(tuple((parser::token(0x2b), Memarg::decode)), |(_, m)| {
                MemoryInstr::F64Load(m)
            }),
            map(tuple((parser::token(0x2c), Memarg::decode)), |(_, m)| {
                MemoryInstr::I32Load8S(m)
            }),
            map(tuple((parser::token(0x2d), Memarg::decode)), |(_, m)| {
                MemoryInstr::I32Load8U(m)
            }),
            map(tuple((parser::token(0x2e), Memarg::decode)), |(_, m)| {
                MemoryInstr::I32Load16S(m)
            }),
            map(tuple((parser::token(0x2f), Memarg::decode)), |(_, m)| {
                MemoryInstr::I32Load16U(m)
            }),
            map(tuple((parser::token(0x30), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Load8S(m)
            }),
            map(tuple((parser::token(0x31), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Load8U(m)
            }),
            map(tuple((parser::token(0x32), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Load16S(m)
            }),
            map(tuple((parser::token(0x33), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Load16U(m)
            }),
            map(tuple((parser::token(0x34), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Load32S(m)
            }),
            map(tuple((parser::token(0x35), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Load32U(m)
            }),
            map(tuple((parser::token(0x36), Memarg::decode)), |(_, m)| {
                MemoryInstr::I32Store(m)
            }),
            map(tuple((parser::token(0x37), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Store(m)
            }),
            map(tuple((parser::token(0x38), Memarg::decode)), |(_, m)| {
                MemoryInstr::F32Store(m)
            }),
            map(tuple((parser::token(0x39), Memarg::decode)), |(_, m)| {
                MemoryInstr::F64Store(m)
            }),
            map(tuple((parser::token(0x3a), Memarg::decode)), |(_, m)| {
                MemoryInstr::I32Store8(m)
            }),
            map(tuple((parser::token(0x3b), Memarg::decode)), |(_, m)| {
                MemoryInstr::I32Store16(m)
            }),
            map(tuple((parser::token(0x3c), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Store8(m)
            }),
            map(tuple((parser::token(0x3d), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Store16(m)
            }),
            map(tuple((parser::token(0x3e), Memarg::decode)), |(_, m)| {
                MemoryInstr::I64Store32(m)
            }),
            map(tuple((parser::token(0x3f), parser::token(0x00))), |_| {
                MemoryInstr::MemorySize
            }),
            map(tuple((parser::token(0x40), parser::token(0x00))), |_| {
                MemoryInstr::MemoryGrow
            }),
        )(input)
    }
}

impl Encoder for ControlInstr {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match &self {
            ControlInstr::Unreachable => bytes.push(0x00),
            ControlInstr::Nop => bytes.push(0x01),
            ControlInstr::Block(rt, is) => {
                bytes.push(0x02);
                rt.encode(bytes);
                loop_encode(is, bytes);
                bytes.push(0x0b);
            }
            ControlInstr::Loop(rt, is) => {
                bytes.push(0x03);
                rt.encode(bytes);
                loop_encode(is, bytes);
                bytes.push(0x0b);
            }
            ControlInstr::If(rt, is1, is2) => {
                bytes.push(0x04);
                rt.encode(bytes);
                loop_encode(is1, bytes);
                if !is2.is_empty() {
                    bytes.push(0x05);
                    loop_encode(is2, bytes);
                }
                bytes.push(0x0b);
            }
            ControlInstr::Br(l) => {
                bytes.push(0x0c);
                l.encode(bytes);
            }
            ControlInstr::BrIf(l) => {
                bytes.push(0x0d);
                l.encode(bytes);
            }
            ControlInstr::BrTable(ls, l) => {
                bytes.push(0x0e);
                ls.encode(bytes);
                l.encode(bytes);
            }
            ControlInstr::Return => bytes.push(0x0f),
            ControlInstr::Call(x) => {
                bytes.push(0x10);
                x.encode(bytes);
            }
            ControlInstr::CallIndirect(x) => {
                bytes.push(0x11);
                x.encode(bytes);
                bytes.push(0x00);
            }
        }
    }
}

impl Decoder for ControlInstr {
    fn decode(input: &[u8]) -> IResult<&[u8], ControlInstr> {
        alt_m!(
            map(parser::token(0x00), |_| ControlInstr::Unreachable),
            map(parser::token(0x01), |_| ControlInstr::Nop),
            map(
                tuple((
                    parser::token(0x02),
                    ResultType::decode,
                    many0(Instr::decode),
                    parser::token(0x0b),
                )),
                |(_, rt, is, _)| ControlInstr::Block(rt, is),
            ),
            map(
                tuple((
                    parser::token(0x03),
                    ResultType::decode,
                    many0(Instr::decode),
                    parser::token(0x0b),
                )),
                |(_, rt, is, _)| ControlInstr::Loop(rt, is),
            ),
            map(
                tuple((
                    parser::token(0x04),
                    ResultType::decode,
                    many0(Instr::decode),
                    opt(map(
                        tuple((parser::token(0x05), many0(Instr::decode))),
                        |(_, is2)| is2,
                    )),
                    parser::token(0x0b),
                )),
                |(_, rt, is1, is2, _)| ControlInstr::If(rt, is1, is2.unwrap_or_else(Vec::new)),
            ),
            map(tuple((parser::token(0x0c), LabelIdx::decode)), |(_, l)| {
                ControlInstr::Br(l)
            }),
            map(tuple((parser::token(0x0d), LabelIdx::decode)), |(_, l)| {
                ControlInstr::BrIf(l)
            }),
            map(
                tuple((
                    parser::token(0x0e),
                    Vec::<LabelIdx>::decode,
                    LabelIdx::decode
                )),
                |(_, ls, l)| ControlInstr::BrTable(ls, l),
            ),
            map(parser::token(0x0f), |_| ControlInstr::Return),
            map(tuple((parser::token(0x10), FuncIdx::decode)), |(_, f)| {
                ControlInstr::Call(f)
            }),
            map(
                tuple((parser::token(0x11), TypeIdx::decode, parser::token(0x00))),
                |(_, t, _)| ControlInstr::CallIndirect(t),
            ),
        )(input)
    }
}

impl Encoder for Instr {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            Instr::Numeric(x) => x.encode(bytes),
            Instr::Parametric(x) => x.encode(bytes),
            Instr::Variable(x) => x.encode(bytes),
            Instr::Memory(x) => x.encode(bytes),
            Instr::Control(x) => x.encode(bytes),
        }
    }
}

impl Decoder for Instr {
    fn decode(input: &[u8]) -> IResult<&[u8], Instr> {
        alt((
            map(NumericInstr::decode, Instr::Numeric),
            map(ParametricInstr::decode, Instr::Parametric),
            map(VariableInstr::decode, Instr::Variable),
            map(MemoryInstr::decode, Instr::Memory),
            map(ControlInstr::decode, Instr::Control),
        ))(input)
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
    // stackoverflowが発生する
    // test_helper::identity_encode_decode::<NumericInstr>();
    test_helper::identity_encode_decode::<ParametricInstr>();
    test_helper::identity_encode_decode::<VariableInstr>();
    test_helper::identity_encode_decode::<MemoryInstr>();
    // test_helper::identity_encode_decode::<ControlInstr>();
    // test_helper::identity_encode_decode::<Instr>();
    test_helper::identity_encode_decode::<Memarg>();
}
