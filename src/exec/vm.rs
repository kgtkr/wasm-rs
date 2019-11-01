use crate::binary::Decoder;
use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, GlobalIdx, LocalIdx, Mem, Module, Table,
    TypeIdx, TypedIdx,
};
use crate::structure::types::{FuncType, Limits, MemType, Mut, ResultType, TableType, ValType};

#[derive(Debug, Clone, PartialEq)]
struct FuncInst {
    type_: FuncType,
    code: Func,
}

#[derive(Debug, Clone, PartialEq)]
struct TableInst {
    max: Option<usize>,
    elem: Vec<Option<FuncIdx>>,
}

#[derive(Debug, Clone, PartialEq)]
struct MemInst {
    max: Option<usize>,
    data: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq)]
struct GlobalInst {
    value: Val,
    mut_: Mut,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Val {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone, PartialEq)]
struct Store {
    funcs: Vec<FuncInst>,
    table: Option<TableInst>,
    mem: Option<MemInst>,
    globals: Vec<GlobalInst>,
}

trait TypedIdxAccess<Idx>
where
    Idx: TypedIdx,
    Self: std::ops::Index<usize>,
{
    fn get_idx(&self, idx: Idx) -> &Self::Output {
        &self[idx.to_idx()]
    }
}

impl TypedIdxAccess<FuncIdx> for Vec<TableInst> {}
impl TypedIdxAccess<GlobalIdx> for Vec<GlobalInst> {}

#[derive(Debug, Clone, PartialEq)]
enum AdminInstr {
    Instr(Instr),
    Invoke(FuncIdx),
    Frame(bool, Frame),
    Label(bool, Vec<Instr>, Vec<Instr>),
}

#[derive(Debug, Clone, PartialEq)]
struct Frame {
    locals: Vec<Val>,
}
