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
    IUnOp(BitSize, IUnOp),
    FUnOp(BitSize, FUnOp),
    IBinOp(BitSize, IBinOp),
    F32BinOp(BitSize, FBinOp),
    ITestOp(BitSize, ITestOp),
    IRelOp(BitSize, IRelOp),
    FRelOp(BitSize, FRelOp),
    I32WrapI64,
    I64WrapI32(Sign),
    Trunc(/* i */ BitSize, /* f */ BitSize, Sign),
    I32DemoteF64,
    F64PromoteF32,
    Convert(/* f */ BitSize, /* i */ BitSize, Sign),
    IReinteret(BitSize),
    FReinteret(BitSize),
    Drop,
    Select,
    LocalGet(LocalIdx),
    LocalSet(LocalIdx),
    LocalTee(LocalIdx),
    GlobalGet(GlobalIdx),
    GlobalSet(GlobalIdx),
    ILoad(BitSize, Memarg),
    FLoad(BitSize, Memarg),
    IStore(BitSize, Memarg),
    FStore(BitSize, Memarg),
    ILoad8(BitSize, Sign, Memarg),
    ILoad16(BitSize, Sign, Memarg),
    I64Load32(Sign, Memarg),
    IStore8(BitSize, Memarg),
    IStore16(BitSize, Memarg),
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
pub enum BitSize {
    B32,
    B64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IUnOp {
    Clz,
    Ctz,
    Popcnt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sign {
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IBinOp {
    Add,
    Sub,
    Mul,
    Div(Sign),
    Rem(Sign),
    And,
    Or,
    Xor,
    ShL,
    Shr(Sign),
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
    Lt(Sign),
    Gt(Sign),
    Le(Sign),
    Ge(Sign),
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
