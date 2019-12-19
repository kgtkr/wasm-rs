#[cfg(test)]
use proptest_derive::Arbitrary;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct FuncType(pub Vec<ValType>, pub Vec<ValType>);

impl FuncType {
    pub fn params(&self) -> &Vec<ValType> {
        &self.0
    }

    pub fn ret(&self) -> Option<&ValType> {
        self.1.first()
    }

    pub fn is_match(&self, other: &FuncType) -> bool {
        self == other
    }
}

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

impl Limits {
    pub fn is_match(&self, other: &Limits) -> bool {
        (self.min >= other.min)
            && other
                .max
                .map(|other_max| {
                    self.max
                        .map(|self_max| self_max <= other_max)
                        .unwrap_or(false)
                })
                .unwrap_or(true)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct MemType(pub Limits);

impl MemType {
    pub fn is_match(&self, other: &MemType) -> bool {
        self.0.is_match(&other.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct TableType(pub Limits, pub ElemType);

impl TableType {
    pub fn is_match(&self, other: &TableType) -> bool {
        self.0.is_match(&other.0) && self.1 == other.1
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ElemType {
    FuncRef,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct GlobalType(pub Mut, pub ValType);

impl GlobalType {
    pub fn is_match(&self, other: &GlobalType) -> bool {
        self == other
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum Mut {
    Const,
    Var,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct ResultType(pub Option<ValType>);
