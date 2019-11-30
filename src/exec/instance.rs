use super::stack::{mem_page_size, AdminInstr, Frame, FrameStack, Label, LabelStack, Stack};
use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, Global, GlobalIdx, LabelIdx, LocalIdx, Mem,
    Module, Table, TypeIdx, TypedIdx,
};
use crate::structure::types::{FuncType, Limits, MemType, Mut, ResultType, TableType, ValType};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use std::io::Cursor;

#[derive(Debug, Clone, PartialEq)]
pub struct Runtime {
    pub funcs: Vec<FuncInst>,
    pub tables: Vec<TableInst>,
    pub mems: Vec<MemInst>,
    pub globals: Vec<GlobalInst>,
}

impl Runtime {
    fn new() -> Runtime {
        Runtime {
            funcs: Vec::new(),
            tables: Vec::new(),
            mems: Vec::new(),
            globals: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FuncAddr(usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TableAddr(usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MemAddr(usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GlobalAddr(usize);

#[derive(Debug, Clone, PartialEq)]
pub struct FuncInst {
    pub type_: FuncType,
    pub code: Func,
}

impl FuncInst {
    fn new(func: Func, module: &Module) -> FuncInst {
        FuncInst {
            type_: module.types.get_idx(func.type_).clone(),
            code: func,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TableInst {
    pub max: Option<usize>,
    pub elem: Vec<Option<FuncIdx>>,
}

impl TableInst {
    fn new(table: &Table) -> TableInst {
        TableInst {
            max: table.type_.0.max.map(|x| x as usize),
            elem: {
                let min = table.type_.0.min as usize;
                let mut vec = Vec::with_capacity(min);
                vec.resize(min, None);
                vec
            },
        }
    }

    fn init_elem(&mut self, offset: usize, init: Vec<FuncIdx>) {
        let len = std::cmp::max(self.elem.len(), offset + init.len());
        self.elem.resize(len, None);
        for i in 0..init.len() {
            self.elem[offset + i] = Some(init[i].clone());
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemInst {
    pub max: Option<usize>,
    pub data: Vec<u8>,
}

impl MemInst {
    fn new(mem: &Mem) -> MemInst {
        let min = mem.type_.0.min as usize * mem_page_size;
        let max = mem.type_.0.max.map(|max| max as usize * mem_page_size);
        MemInst {
            max,
            data: {
                let mut vec = Vec::with_capacity(min);
                vec.resize(min, 0);
                vec
            },
        }
    }

    fn init_data(&mut self, offset: usize, init: Vec<u8>) {
        let len = std::cmp::max(self.data.len(), offset + init.len());
        self.data.resize(len, 0);
        for i in 0..init.len() {
            self.data[offset + i] = init[i];
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalInst {
    pub value: Val,
    pub mut_: Mut,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Val {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Val {
    pub fn unwrap_i32(&self) -> i32 {
        if let Val::I32(x) = self {
            *x
        } else {
            panic!();
        }
    }

    pub fn unwrap_i64(&self) -> i64 {
        if let Val::I64(x) = self {
            *x
        } else {
            panic!();
        }
    }

    pub fn unwrap_f32(&self) -> f32 {
        if let Val::F32(x) = self {
            *x
        } else {
            panic!();
        }
    }

    pub fn unwrap_f64(&self) -> f64 {
        if let Val::F64(x) = self {
            *x
        } else {
            panic!();
        }
    }
}

pub trait TypedIdxAccess<Idx>
where
    Idx: TypedIdx,
    Self: std::ops::Index<usize>,
{
    fn get_idx(&self, idx: Idx) -> &Self::Output {
        &self[idx.to_idx()]
    }
}

impl TypedIdxAccess<FuncIdx> for Vec<FuncInst> {}
impl TypedIdxAccess<GlobalIdx> for Vec<GlobalInst> {}
impl TypedIdxAccess<TypeIdx> for Vec<FuncType> {}

#[derive(Debug, PartialEq)]
pub struct ModuleInst<'a> {
    pub funcs: Vec<FuncInst>,
    pub table: Option<TableInst>,
    pub mem: Option<MemInst>,
    pub globals: Vec<GlobalInst>,
    pub module: &'a Module,
}

impl<'a> ModuleInst<'a> {
    fn new(module: &'a Module) -> ModuleInst {
        let mut result = ModuleInst {
            funcs: module
                .funcs
                .clone()
                .into_iter()
                .map(|f| FuncInst::new(f, &module))
                .collect(),
            table: module.tables.iter().next().map(|t| TableInst::new(t)),
            mem: module.mems.iter().next().map(|m| MemInst::new(m)),
            globals: Vec::new(),
            module,
        };

        for global in &result.module.globals {
            result.globals.push(GlobalInst {
                value: result.eval_const_expr(&global.init),
                mut_: global.type_.0,
            });
        }
        for elem in &result.module.elem {
            let offset = result.eval_const_expr(&elem.offset).unwrap_i32() as usize;
            result
                .table
                .as_mut()
                .unwrap()
                .init_elem(offset, elem.init.clone());
        }
        for data in &result.module.data {
            let offset = result.eval_const_expr(&data.offset).unwrap_i32() as usize;
            result
                .mem
                .as_mut()
                .unwrap()
                .init_data(offset, data.init.clone().into_iter().map(|x| x.0).collect());
        }

        if let Some(start) = &result.module.start {
            result.call_func(start.func, vec![]);
        }
        result
    }

    fn eval_const_expr(&self, expr: &Expr) -> Val {
        match &expr.0[..] {
            &[Instr::I32Const(x)] => Val::I32(x),
            &[Instr::I64Const(x)] => Val::I64(x),
            &[Instr::F32Const(x)] => Val::F32(x),
            &[Instr::F64Const(x)] => Val::F64(x),
            &[Instr::GlobalGet(i)] => self.globals[i.to_idx()].value,
            _ => panic!(),
        }
    }

    fn call_func(&mut self, idx: FuncIdx, params: Vec<Val>) -> Option<Val> {
        let mut stack = Stack {
            stack: vec![FrameStack {
                frame: Frame { locals: Vec::new() },
                stack: vec![LabelStack {
                    label: Label { instrs: vec![] },
                    instrs: vec![AdminInstr::Invoke(idx)],
                    stack: params,
                }],
            }],
        };

        loop {
            stack.step(self);
            if stack.stack.len() == 1
                && stack.stack.first().unwrap().stack.len() == 1
                && stack
                    .stack
                    .first()
                    .unwrap()
                    .stack
                    .first()
                    .unwrap()
                    .instrs
                    .is_empty()
            {
                break;
            }
        }

        stack.stack.pop().unwrap().stack.pop().unwrap().stack.pop()
    }

    pub fn export_call_func(&mut self, name: &str, params: Vec<Val>) -> Option<Val> {
        self.call_func(
            self.module
                .exports
                .iter()
                .find(|e| e.name.0.as_str() == name)
                .unwrap()
                .desc
                .unwrap_func(),
            params,
        )
    }
}

use crate::binary::Decoder;

#[test]
fn test_add() {
    let module = Module::decode_end(&std::fs::read("./example/add.wasm").unwrap()).unwrap();
    let mut instance = ModuleInst::new(&module);
    assert_eq!(
        instance.export_call_func("add", vec![Val::I32(3), Val::I32(5)]),
        Some(Val::I32(8))
    );
}

#[test]
fn test_gcd() {
    let module = Module::decode_end(&std::fs::read("./example/gcd.wasm").unwrap()).unwrap();
    let mut instance = ModuleInst::new(&module);

    assert_eq!(
        instance.export_call_func("gcd", vec![Val::I32(182), Val::I32(1029)]),
        Some(Val::I32(7))
    );
}

#[test]
fn test_pow() {
    let module = Module::decode_end(&std::fs::read("./example/pow.wasm").unwrap()).unwrap();
    let mut instance = ModuleInst::new(&module);

    assert_eq!(
        instance.export_call_func("pow", vec![Val::I32(2), Val::I32(10)]),
        Some(Val::I32(1024))
    );
}

#[test]
fn test_br_table() {
    let module = Module::decode_end(&std::fs::read("./example/br_table.wasm").unwrap()).unwrap();
    let mut instance = ModuleInst::new(&module);

    assert_eq!(
        instance.export_call_func("br_table", vec![Val::I32(0)]),
        Some(Val::I32(10))
    );
    assert_eq!(
        instance.export_call_func("br_table", vec![Val::I32(10)]),
        Some(Val::I32(30))
    );
}

#[test]
fn test_md5() {
    use std::ffi::CString;

    let module = Module::decode_end(&std::fs::read("./example/md5.wasm").unwrap()).unwrap();
    let mut instance = ModuleInst::new(&module);

    let input_bytes = CString::new("abc").unwrap().into_bytes();
    let input_ptr = instance
        .export_call_func("alloc", vec![Val::I32(input_bytes.len() as i32)])
        .unwrap()
        .unwrap_i32() as usize;
    for i in 0..input_bytes.len() {
        instance.mem.as_mut().unwrap().data[input_ptr + i] = input_bytes[i];
    }

    let output_ptr = instance
        .export_call_func("md5", vec![Val::I32(input_ptr as i32)])
        .unwrap()
        .unwrap_i32() as usize;
    assert_eq!(
        CString::new(
            instance
                .mem
                .unwrap()
                .data
                .into_iter()
                .skip(output_ptr)
                .take_while(|x| *x != 0)
                .collect::<Vec<_>>(),
        )
        .unwrap()
        .into_string()
        .unwrap(),
        "900150983cd24fb0d6963f7d28e17f72".to_string()
    );
}
