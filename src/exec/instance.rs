use super::stack::{AdminInstr, Frame, FrameStack, Label, LabelStack, Stack};
use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, Global, GlobalIdx, ImportDesc, LabelIdx,
    LocalIdx, Mem, Module, Table, TypeIdx, TypedIdx,
};
use crate::structure::types::{FuncType, Limits, MemType, Mut, ResultType, TableType, ValType};

use std::cell::RefCell;
use std::io::Cursor;
use std::rc::{Rc, Weak};

#[derive(Debug, Clone)]
pub enum ExternalVal {
    Func(FuncAddr),
    Table(TableAddr),
    Mem(MemAddr),
    Global(GlobalAddr),
}

impl ExternalVal {
    fn unwrap_func(self) -> FuncAddr {
        if let ExternalVal::Func(x) = self {
            x
        } else {
            panic!();
        }
    }

    fn unwrap_table(self) -> TableAddr {
        if let ExternalVal::Table(x) = self {
            x
        } else {
            panic!();
        }
    }

    fn unwrap_mem(self) -> MemAddr {
        if let ExternalVal::Mem(x) = self {
            x
        } else {
            panic!();
        }
    }

    fn unwrap_global(self) -> GlobalAddr {
        if let ExternalVal::Global(x) = self {
            x
        } else {
            panic!();
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExportInst {
    name: String,
    value: ExternalVal,
}

#[derive(Debug, Clone)]
pub enum FuncInst {
    RuntimeFunc {
        type_: FuncType,
        code: Func,
        module: Weak<ModuleInst>,
    },
    HostFunc {
        type_: FuncType,
        host_code: fn(Vec<Val>) -> Option<Val>,
    },
}

impl FuncInst {
    fn new(func: Func, module: Weak<ModuleInst>) -> FuncInst {
        FuncInst::RuntimeFunc {
            type_: module.upgrade().unwrap().types.get_idx(func.type_).clone(),
            code: func,
            module,
        }
    }

    pub fn type_(&self) -> &FuncType {
        match self {
            FuncInst::RuntimeFunc { type_, .. } => type_,
            FuncInst::HostFunc { type_, .. } => type_,
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
    const PAGE_SIZE: usize = 65536;

    fn new(mem: &Mem) -> MemInst {
        let min = mem.type_.0.min as usize * MemInst::PAGE_SIZE;
        let max = mem.type_.0.max.map(|max| max as usize * MemInst::PAGE_SIZE);
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

    pub fn page_size(&self) -> i32 {
        (self.data.len() / MemInst::PAGE_SIZE) as i32
    }

    pub fn grow(&mut self, add_size: i32) -> i32 {
        let prev = self.page_size();
        self.data
            .resize((prev as usize + add_size as usize) * MemInst::PAGE_SIZE, 0);
        prev
    }

    pub fn mut_buffer(&mut self) -> &mut [u8] {
        &mut self.data[..]
    }

    pub fn buffer(&self) -> &[u8] {
        &self.data[..]
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

#[derive(Clone, Debug)]
pub struct FuncAddr(pub Rc<RefCell<FuncInst>>);

impl FuncAddr {
    fn call(&self, params: Vec<Val>) -> Option<Val> {
        let mut stack = Stack {
            stack: vec![FrameStack {
                frame: Frame {
                    locals: Vec::new(),
                    module: Weak::new(),
                },
                stack: vec![LabelStack {
                    label: Label { instrs: vec![] },
                    instrs: vec![AdminInstr::Invoke(self.clone())],
                    stack: params,
                }],
            }],
        };

        loop {
            stack.step();
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

impl MemAddr {
    pub fn with_mut_cursor<T>(&self, f: impl FnOnce(Cursor<&mut Vec<u8>>) -> T) -> T {
        let raw = &mut self.0.borrow_mut().data;
        f(Cursor::new(raw))
    }

    pub fn with_cursor<T>(&self, f: impl FnOnce(Cursor<&Vec<u8>>) -> T) -> T {
        let raw = &self.0.borrow().data;
        f(Cursor::new(raw))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GlobalAddr(pub Rc<RefCell<GlobalInst>>);

#[derive(Debug)]
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
            table: None,
            mem: None,
            globals: Vec::new(),
            exports: Vec::new(),
        };

        for _ in &module.funcs {
            let dummy_func = FuncInst::RuntimeFunc {
                type_: FuncType(Vec::new(), Vec::new()),
                code: Func {
                    type_: TypeIdx(0),
                    locals: Vec::new(),
                    body: Expr(Vec::new()),
                },
                module: Weak::new(),
            };
            result
                .funcs
                .push(FuncAddr(Rc::new(RefCell::new(dummy_func.clone()))));
        }

        if let Some(table) = module.tables.iter().next() {
            let _ = result
                .table
                .replace(TableAddr(Rc::new(RefCell::new(TableInst::new(table)))));
        }

        if let Some(mem) = module.mems.iter().next() {
            let _ = result
                .mem
                .replace(MemAddr(Rc::new(RefCell::new(MemInst::new(mem)))));
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
                    ExportDesc::Mem(_idx) => ExternalVal::Mem(result.mem.as_ref().unwrap().clone()),
                    ExportDesc::Table(_idx) => {
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
            *dummy = FuncInst::new(func.clone(), Rc::downgrade(&result));
        }

        if let Some(start) = &module.start {
            result.funcs.get_idx(start.func).clone().call(vec![]);
        }

        result
    }

    fn dummy() -> ModuleInst {
        ModuleInst {
            types: Vec::new(),
            funcs: Vec::new(),
            table: None,
            mem: None,
            globals: Vec::new(),
            exports: Vec::new(),
        }
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

    pub fn export(&self, name: &str) -> ExternalVal {
        self.exports
            .iter()
            .find(|e| e.name.as_str() == name)
            .map(|x| x.value.clone())
            .unwrap()
            .clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::Decoder;

    #[test]
    fn test_add() {
        let module = Module::decode_end(&std::fs::read("./example/add.wasm").unwrap()).unwrap();
        let instance = ModuleInst::new(&module);
        assert_eq!(
            instance
                .export("add")
                .unwrap_func()
                .call(vec![Val::I32(3), Val::I32(5)]),
            Some(Val::I32(8))
        );
    }

    #[test]
    fn test_gcd() {
        let module = Module::decode_end(&std::fs::read("./example/gcd.wasm").unwrap()).unwrap();
        let instance = ModuleInst::new(&module);

        assert_eq!(
            instance
                .export("gcd")
                .unwrap_func()
                .call(vec![Val::I32(182), Val::I32(1029)]),
            Some(Val::I32(7))
        );
    }

    #[test]
    fn test_pow() {
        let module = Module::decode_end(&std::fs::read("./example/pow.wasm").unwrap()).unwrap();
        let instance = ModuleInst::new(&module);
        assert_eq!(
            instance
                .export("pow")
                .unwrap_func()
                .call(vec![Val::I32(2), Val::I32(10)]),
            Some(Val::I32(1024))
        );
    }

    #[test]
    fn test_br_table() {
        let module =
            Module::decode_end(&std::fs::read("./example/br_table.wasm").unwrap()).unwrap();
        let instance = ModuleInst::new(&module);

        assert_eq!(
            instance
                .export("br_table")
                .unwrap_func()
                .call(vec![Val::I32(0)]),
            Some(Val::I32(10))
        );
        assert_eq!(
            instance
                .export("br_table")
                .unwrap_func()
                .call(vec![Val::I32(10)]),
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
            .export("alloc")
            .unwrap_func()
            .call(vec![Val::I32(input_bytes.len() as i32)])
            .unwrap()
            .unwrap_i32() as usize;
        for i in 0..input_bytes.len() {
            let mem = instance.export("memory").unwrap_mem();
            let mut mem = mem.0.borrow_mut();
            let buf = mem.mut_buffer();
            buf[input_ptr + i] = input_bytes[i];
        }

        let output_ptr = instance
            .export("md5")
            .unwrap_func()
            .call(vec![Val::I32(input_ptr as i32)])
            .unwrap()
            .unwrap_i32() as usize;

        let mem = instance.export("memory").unwrap_mem();
        let mem = mem.0.borrow();
        let raw = mem.buffer();
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
}
