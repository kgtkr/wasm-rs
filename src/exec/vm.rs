use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, Global, GlobalIdx, LocalIdx, Mem, Module, Table,
    TypeIdx, TypedIdx,
};
use crate::structure::types::{FuncType, Limits, MemType, Mut, ResultType, TableType, ValType};

#[derive(Debug, Clone, PartialEq)]
struct FuncInst {
    type_: FuncType,
    code: Func,
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
struct TableInst {
    max: Option<usize>,
    elem: Vec<Option<FuncIdx>>,
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

const mem_page_size: usize = 65536;

#[derive(Debug, Clone, PartialEq)]
struct MemInst {
    max: Option<usize>,
    data: Vec<u8>,
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

impl Val {
    fn unwrap_i32(&self) -> i32 {
        if let Val::I32(x) = self {
            *x
        } else {
            panic!();
        }
    }

    fn unwrap_i64(&self) -> i64 {
        if let Val::I64(x) = self {
            *x
        } else {
            panic!();
        }
    }

    fn unwrap_f32(&self) -> f32 {
        if let Val::F32(x) = self {
            *x
        } else {
            panic!();
        }
    }

    fn unwrap_f64(&self) -> f64 {
        if let Val::F64(x) = self {
            *x
        } else {
            panic!();
        }
    }
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
impl TypedIdxAccess<TypeIdx> for Vec<FuncType> {}

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

impl Store {
    pub fn new(module: &Module) -> Store {
        let mut store = Store {
            funcs: module
                .funcs
                .clone()
                .into_iter()
                .map(|f| FuncInst::new(f, module))
                .collect(),
            table: module.tables.iter().next().map(|t| TableInst::new(t)),
            mem: module.mems.iter().next().map(|m| MemInst::new(m)),
            globals: Vec::new(),
        };

        for global in &module.globals {
            store.globals.push(GlobalInst {
                value: store.eval_const_expr(&global.init),
                mut_: global.type_.0,
            });
        }

        for elem in &module.elem {
            let offset = store.eval_const_expr(&elem.offset).unwrap_i32() as usize;
            store
                .table
                .as_mut()
                .unwrap()
                .init_elem(offset, elem.init.clone());
        }

        for data in &module.data {
            let offset = store.eval_const_expr(&data.offset).unwrap_i32() as usize;
            store
                .mem
                .as_mut()
                .unwrap()
                .init_data(offset, data.init.clone().into_iter().map(|x| x.0).collect());
        }

        store
    }

    pub fn eval_const_expr(&self, expr: &Expr) -> Val {
        match &expr.0[..] {
            &[Instr::I32Const(x)] => Val::I32(x),
            &[Instr::I64Const(x)] => Val::I64(x),
            &[Instr::F32Const(x)] => Val::F32(x),
            &[Instr::F64Const(x)] => Val::F64(x),
            &[Instr::GlobalGet(i)] => self.globals[i.to_idx()].value,
            _ => panic!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct VM<'a> {
    store: &'a mut Store,
    stack: Vec<Val>,
    code: Vec<AdminInstr>,
    frame: Vec<Frame>,
}

impl<'a> VM<'a> {
    fn step(&mut self) {
        match self.code.pop() {
            _ => unimplemented!(),
        }
    }
}

struct VMModule {
    store: Store,
    module: Module,
}

impl VMModule {
    fn new(module: Module) -> VMModule {
        let store = Store::new(&module);
        let mut result = VMModule { store, module };
        if let Some(start) = &result.module.start {
            result.call_func(start.func, vec![]);
        }
        result
    }

    fn call_func(&mut self, idx: FuncIdx, params: Vec<Val>) -> Option<Val> {
        let mut vm = VM {
            store: &mut self.store,
            stack: params,
            code: vec![AdminInstr::Invoke(idx)],
            frame: vec![Frame { locals: Vec::new() }],
        };

        loop {
            vm.step();
            if vm.frame.len() == 1 && vm.code.is_empty() {
                break;
            }
        }

        vm.stack.pop()
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
    let mut vm = VMModule::new(module);
    assert_eq!(
        vm.export_call_func("add", vec![Val::I32(3), Val::I32(5)]),
        Some(Val::I32(8))
    );
}

#[test]
fn test_gcd() {
    let module = Module::decode_end(&std::fs::read("./example/gcd.wasm").unwrap()).unwrap();
    let mut vm = VMModule::new(module);

    assert_eq!(
        vm.export_call_func("gcd", vec![Val::I32(182), Val::I32(1029)]),
        Some(Val::I32(7))
    );
}
