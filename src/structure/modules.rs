use super::instructions::Expr;
use super::types::{FuncType, GlobalType, MemType, TableType, ValType};
use super::values::{Byte, Name};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub types: Vec<FuncType>,
    pub funcs: Vec<Func>,
    pub tables: Vec<Table>,
    pub mems: Vec<Mem>,
    pub globals: Vec<Global>,
    pub elem: Vec<Elem>,
    pub data: Vec<Data>,
    pub start: Option<Start>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct TypeIdx(pub u32);

impl Into<u32> for TypeIdx {
    fn into(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct FuncIdx(pub u32);

impl Into<u32> for FuncIdx {
    fn into(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct TableIdx(pub u32);

impl Into<u32> for TableIdx {
    fn into(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct MemIdx(pub u32);

impl Into<u32> for MemIdx {
    fn into(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct GlobalIdx(pub u32);

impl Into<u32> for GlobalIdx {
    fn into(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct LocalIdx(pub u32);

impl Into<u32> for LocalIdx {
    fn into(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct LabelIdx(pub u32);

impl Into<u32> for LabelIdx {
    fn into(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub type_: TypeIdx,
    pub locals: Vec<ValType>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct Table {
    pub type_: TableType,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct Mem {
    pub type_: MemType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
    pub type_: GlobalType,
    // constant expression
    pub init: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Elem {
    pub table: TableIdx,
    // constant expression
    pub offset: Expr,
    pub init: Vec<FuncIdx>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Data {
    pub data: MemIdx,
    // constant expression
    pub offset: Expr,
    pub init: Vec<Byte>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct Start {
    pub func: FuncIdx,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct Export {
    pub name: Name,
    pub desc: ExportDesc,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ExportDesc {
    Func(FuncIdx),
    Table(TableIdx),
    Mem(MemIdx),
    Global(GlobalIdx),
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct Import {
    pub module: Name,
    pub name: Name,
    pub desc: ImportDesc,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub enum ImportDesc {
    Func(TypeIdx),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}
