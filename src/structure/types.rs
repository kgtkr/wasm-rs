#[derive(Debug, Clone, PartialEq)]
pub struct FuncType(Vec<ValType>, Vec<ValType>);

#[derive(Debug, Clone, PartialEq)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Limits {
    min: u32,
    max: Option<u32>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemType(Limits);

#[derive(Debug, Clone, PartialEq)]
pub struct TableType(Limits, ElemType);

#[derive(Debug, Clone, PartialEq)]
pub enum ElemType {
    FuncRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalType(Mut, ValType);

#[derive(Debug, Clone, PartialEq)]
pub enum Mut {
    Const,
    Var,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResultType(Option<ValType>);
