use super::modules::{FuncIdx, GlobalIdx, LabelIdx, LocalIdx, TypeIdx};
use super::types::ResultType;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr(Instr);

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    I32UnOp(IUnOp),
    I64UnOp(IUnOp),
    F32UnOp(FUnOp),
    F64UnOp(FUnOp),
    I32BinOp(IBinOp),
    I64BinOp(IBinOp),
    F32BinOp(FBinOp),
    F64BinOp(FBinOp),
    I32TestOp(ITestOp),
    I64TestOp(ITestOp),
    I32RelOp(IRelOp),
    I64RelOp(IRelOp),
    F32RelOp(FRelOp),
    F64RelOp(FRelOp),
    I32WrapI64,
    I64ExtendI32S,
    I64ExtendI64S,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,
    I32DemoteF64,
    F64PromoteF32,
    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    I32ReinteretF32,
    I64ReinteretF64,
    F32ReinteretI32,
    F64ReinteretI64,
    Drop,
    Select,
    LocalGet(LocalIdx),
    LocalSet(LocalIdx),
    LocalTee(LocalIdx),
    GlobalGet(GlobalIdx),
    GlobalSet(GlobalIdx),
    I32Load(Memarg),
    I64Load(Memarg),
    F32Load(Memarg),
    F64Load(Memarg),
    I32Store(Memarg),
    I64Store(Memarg),
    F32Store(Memarg),
    F64Store(Memarg),
    I32Load8S(Memarg),
    I32Load8U(Memarg),
    I64Load8S(Memarg),
    I64Load8U(Memarg),
    I32Load16S(Memarg),
    I32Load16U(Memarg),
    I64Load16S(Memarg),
    I64Load16U(Memarg),
    I64Load32S(Memarg),
    I64Load32U(Memarg),
    I32Store8(Memarg),
    I64Store8(Memarg),
    I32Store16(Memarg),
    I64Store16(Memarg),
    I64Store32(Memarg),
    MemorySize,
    MemoryGrow,
    Nop,
    Unreachable,
    Block(ResultType, Vec<Instr>),
    Loop(ResultType, Vec<Instr>),
    If(ResultType, Vec<Instr>, Vec<Instr>),
    Br(LabelIdx),
    BrIf(LabelIdx),
    BrTable(Vec<LabelIdx>, LabelIdx),
    Return,
    Call(FuncIdx),
    CallIndirect(TypeIdx),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IUnOp {
    Clz,
    Ctz,
    Popcnt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IBinOp {
    Add,
    Sub,
    Mul,
    DivS,
    DivU,
    RemS,
    RemU,
    And,
    Or,
    Xor,
    ShL,
    ShrS,
    ShrU,
    Rotl,
    Rotr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FUnOp {
    Abs,
    Neg,
    Sqrt,
    Ceil,
    Floor,
    Trunc,
    Nearest,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Min,
    Max,
    CopySign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ITestOp {
    Eqz,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRelOp {
    Eq,
    Ne,
    LtS,
    LtU,
    GtS,
    GtU,
    LeS,
    LeU,
    GeS,
    GeU,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FRelOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Memarg {
    offset: u32,
    align: u32,
}
