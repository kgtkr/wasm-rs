#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct FuncType(pub Vec<ValType>, pub Vec<ValType>);

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct MemType(pub Limits);

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct TableType(pub Limits, pub ElemType);

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ElemType {
    FuncRef,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct GlobalType(pub Mut, pub ValType);

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum Mut {
    Const,
    Var,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct ResultType(pub Option<ValType>);
