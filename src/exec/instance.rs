use super::stack::{mem_page_size, AdminInstr, Frame, FrameStack, Label, LabelStack, Stack};
use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, Global, GlobalIdx, ImportDesc, LabelIdx,
    LocalIdx, Mem, Module, Table, TypeIdx, TypedIdx,
};
use crate::structure::types::{FuncType, Limits, MemType, Mut, ResultType, TableType, ValType};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use std::cell::RefCell;
use std::io::Cursor;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum ExternalVal {
    Func(FuncAddr),
    Table(TableAddr),
    Mem(MemAddr),
    Global(GlobalAddr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExportInst {
    name: String,
    value: ExternalVal,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncInst {
    pub type_: FuncType,
    pub code: Func,
    pub module: Rc<ModuleInst>,
}

impl FuncInst {
    fn new(func: Func, module: Rc<ModuleInst>) -> FuncInst {
        FuncInst {
            type_: module.types.get_idx(func.type_).clone(),
            code: func,
            module,
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
impl TypedIdxAccess<FuncIdx> for Vec<FuncAddr> {}
impl TypedIdxAccess<GlobalIdx> for Vec<GlobalAddr> {}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncAddr(pub Rc<RefCell<FuncInst>>);

impl FuncAddr {
    fn call(&self, params: Vec<Val>) -> Option<Val> {
        let mut stack = Stack {
            stack: vec![FrameStack {
                frame: Frame { locals: Vec::new() },
                stack: vec![LabelStack {
                    label: Label { instrs: vec![] },
                    instrs: vec![AdminInstr::Invoke(self.clone())],
                    stack: params,
                }],
            }],
        };

        loop {
            stack.step(self.0.borrow().module.as_ref());
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct TableAddr(pub Rc<RefCell<TableInst>>);

#[derive(Clone, Debug, PartialEq)]
pub struct MemAddr(pub Rc<RefCell<MemInst>>);

#[derive(Clone, Debug, PartialEq)]
pub struct GlobalAddr(pub Rc<RefCell<GlobalInst>>);

#[derive(Debug, PartialEq)]
pub struct ModuleInst {
    pub types: Vec<FuncType>,
    pub funcs: Vec<FuncAddr>,
    pub table: Option<TableAddr>,
    pub mem: Option<MemAddr>,
    pub globals: Vec<GlobalAddr>,
    pub exports: Vec<ExportInst>,
}

impl ModuleInst {
    fn new(module: &Module) -> Rc<ModuleInst> {
        let mut result = ModuleInst {
            types: module.types.clone(),
            funcs: Vec::new(),
            table: module
                .tables
                .iter()
                .next()
                .map(|t| TableAddr(Rc::new(RefCell::new(TableInst::new(t))))),
            mem: module
                .mems
                .iter()
                .next()
                .map(|m| MemAddr(Rc::new(RefCell::new(MemInst::new(m))))),
            globals: Vec::new(),
            exports: Vec::new(),
        };

        for _ in &module.funcs {
            let dummy_func = FuncInst {
                type_: FuncType(Vec::new(), Vec::new()),
                code: Func {
                    type_: TypeIdx(0),
                    locals: Vec::new(),
                    body: Expr(Vec::new()),
                },
                module: Rc::new(ModuleInst {
                    types: Vec::new(),
                    funcs: Vec::new(),
                    table: None,
                    mem: None,
                    globals: Vec::new(),
                    exports: Vec::new(),
                }),
            };
            result
                .funcs
                .push(FuncAddr(Rc::new(RefCell::new(dummy_func.clone()))));
        }

        for global in &module.globals {
            result
                .globals
                .push(GlobalAddr(Rc::new(RefCell::new(GlobalInst {
                    value: result.eval_const_expr(&global.init),
                    mut_: global.type_.0,
                }))));
        }
        for elem in &module.elem {
            let offset = result.eval_const_expr(&elem.offset).unwrap_i32() as usize;
            result.table.as_mut().unwrap();
            result
                .table
                .as_ref()
                .unwrap()
                .0
                .borrow_mut()
                .init_elem(offset, elem.init.clone());
        }
        for data in &module.data {
            let offset = result.eval_const_expr(&data.offset).unwrap_i32() as usize;
            result
                .mem
                .as_ref()
                .unwrap()
                .0
                .borrow_mut()
                .init_data(offset, data.init.clone().into_iter().map(|x| x.0).collect());
        }

        for export in &module.exports {
            result.exports.push(ExportInst {
                name: export.name.0.clone(),
                value: match export.desc {
                    ExportDesc::Func(idx) => ExternalVal::Func(result.funcs.get_idx(idx).clone()),
                    ExportDesc::Global(idx) => {
                        ExternalVal::Global(result.globals.get_idx(idx).clone())
                    }
                    ExportDesc::Mem(idx) => ExternalVal::Mem(result.mem.as_ref().unwrap().clone()),
                    ExportDesc::Table(idx) => {
                        ExternalVal::Table(result.table.as_ref().unwrap().clone())
                    }
                },
            });
        }

        let result = Rc::new(result);

        for (i, func) in module.funcs.iter().enumerate() {
            let idx = i + module
                .imports
                .iter()
                .map(|x| {
                    if let ImportDesc::Func(_) = x.desc {
                        1
                    } else {
                        0
                    }
                })
                .sum::<usize>();
            let mut dummy = result.funcs[idx].0.borrow_mut();
            *dummy = FuncInst::new(func.clone(), result.clone());
        }

        if let Some(start) = &module.start {
            result.funcs.get_idx(start.func).clone().call(vec![]);
        }

        result
    }

    fn eval_const_expr(&self, expr: &Expr) -> Val {
        match &expr.0[..] {
            &[Instr::I32Const(x)] => Val::I32(x),
            &[Instr::I64Const(x)] => Val::I64(x),
            &[Instr::F32Const(x)] => Val::F32(x),
            &[Instr::F64Const(x)] => Val::F64(x),
            &[Instr::GlobalGet(i)] => self.globals[i.to_idx()].0.borrow().value,
            _ => panic!(),
        }
    }

    pub fn export_call_func(&self, name: &str, params: Vec<Val>) -> Option<Val> {
        if let ExternalVal::Func(func) = &self
            .exports
            .iter()
            .find(|e| e.name.as_str() == name)
            .unwrap()
            .value
        {
            func.call(params)
        } else {
            panic!()
        }
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
    let instance = ModuleInst::new(&module);

    let input_bytes = CString::new("abc").unwrap().into_bytes();
    let input_ptr = instance
        .export_call_func("alloc", vec![Val::I32(input_bytes.len() as i32)])
        .unwrap()
        .unwrap_i32() as usize;
    for i in 0..input_bytes.len() {
        let mut mem = instance.mem.as_ref().unwrap().0.borrow_mut();
        mem.data[input_ptr + i] = input_bytes[i];
    }

    let output_ptr = instance
        .export_call_func("md5", vec![Val::I32(input_ptr as i32)])
        .unwrap()
        .unwrap_i32() as usize;
    let raw = &instance.mem.as_ref().unwrap().0.borrow_mut().data;
    assert_eq!(
        CString::new(
            raw.into_iter()
                .skip(output_ptr)
                .cloned()
                .take_while(|x| *x != 0)
                .collect::<Vec<_>>(),
        )
        .unwrap()
        .into_string()
        .unwrap(),
        "900150983cd24fb0d6963f7d28e17f72".to_string()
    );
}
