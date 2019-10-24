use super::instructions::Expr;
use super::types::{FuncType, GlobalType, MemType, TableType, ValType};
use super::values::{Byte, Name};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    types: Vec<FuncType>,
    funcs: Vec<Func>,
    tables: Vec<Table>,
    mems: Vec<Mem>,
    globals: Vec<Global>,
    elem: Vec<Elem>,
    data: Vec<Data>,
    start: Option<Start>,
    imports: Vec<Import>,
    exports: Vec<Export>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeIdx(u32);

#[derive(Debug, Clone, PartialEq)]
pub struct FuncIdx(u32);

#[derive(Debug, Clone, PartialEq)]
pub struct TableIdx(u32);

#[derive(Debug, Clone, PartialEq)]
pub struct MemIdx(u32);

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalIdx(u32);

#[derive(Debug, Clone, PartialEq)]
pub struct LocalIdx(u32);

#[derive(Debug, Clone, PartialEq)]
pub struct LabelIdx(u32);

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    type_: TypeIdx,
    locals: Vec<ValType>,
    body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Table {
    type_: TableType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Mem {
    type_: MemType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global {
    type_: GlobalType,
    // constant expression
    init: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Elem {
    table: TableIdx,
    // constant expression
    offset: Expr,
    init: Vec<FuncIdx>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Data {
    data: MemIdx,
    // constant expression
    offset: Expr,
    init: Vec<Byte>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Start {
    func: FuncIdx,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export {
    name: Name,
    desc: ExportDesc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExportDesc {
    Func(FuncIdx),
    Table(TypeIdx),
    Mem(MemIdx),
    Global(GlobalIdx),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    module: Name,
    name: Name,
    desc: ImportDesc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportDesc {
    Func(TypeIdx),
    Table(TableType),
    Mem(MemType),
    Global(GlobalType),
}
