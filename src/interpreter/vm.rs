use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, GlobalIdx, Mem, Module, Table,
};
use crate::structure::types::{Limits, MemType, TableType};

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone, PartialEq)]
struct VMTable {
    max: Option<usize>,
    data: Vec<Option<FuncIdx>>,
}

impl VMTable {
    fn new(min: usize, max: Option<usize>) -> VMTable {
        VMTable {
            max,
            data: {
                let mut vec = Vec::with_capacity(min);
                vec.resize(min, None);
                vec
            },
        }
    }

    fn set_elem(&mut self, offset: usize, init: Vec<FuncIdx>) {
        let len = std::cmp::max(self.data.len(), offset + init.len());
        self.data.resize(len, None);
        for i in 0..init.len() {
            self.data[offset + i] = Some(init[i].clone());
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct VMMem {
    max: Option<usize>,
    data: Vec<u8>,
}

impl VMMem {
    fn new(min: usize, max: Option<usize>) -> VMMem {
        VMMem {
            max,
            data: {
                let mut vec = Vec::with_capacity(min);
                vec.resize(min, 0);
                vec
            },
        }
    }

    fn set_data(&mut self, offset: usize, init: Vec<u8>) {
        let len = std::cmp::max(self.data.len(), offset + init.len());
        self.data.resize(len, 0);
        for i in 0..init.len() {
            self.data[offset + i] = init[i];
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VM {
    funcs: Vec<Func>,
    globals: Vec<Val>,
    stack: Vec<Val>,
    table: Option<VMTable>,
    mem: Option<VMMem>,
    exports: Vec<Export>,
}

impl VM {
    pub fn new(module: Module) -> VM {
        let mut vm = VM {
            funcs: Vec::new(),
            globals: Vec::new(),
            mem: None,
            stack: Vec::new(),
            table: None,
            exports: Vec::new(),
        };

        vm.funcs = module.funcs;
        vm.table = module
            .tables
            .get(0)
            .map(
                |Table {
                     type_: TableType(limits, _),
                 }| {
                    Some(VMTable::new(
                        limits.min as usize,
                        limits.max.map(|x| x as usize),
                    ))
                },
            )
            .unwrap_or(None);
        vm.mem = module
            .mems
            .get(0)
            .map(
                |Mem {
                     type_: MemType(limits),
                 }| {
                    Some(VMMem::new(
                        limits.min as usize,
                        limits.max.map(|x| x as usize),
                    ))
                },
            )
            .unwrap_or(None);
        for global in &module.globals {
            vm.globals.push(vm.eval_const_expr(&global.init));
        }

        for elem in &module.elem {
            let offset = if let Val::I32(x) = vm.eval_const_expr(&elem.offset) {
                x as usize
            } else {
                panic!()
            };
            if let Some(table) = &mut vm.table {
                table.set_elem(offset, elem.init.clone());
            } else {
                panic!();
            }
        }

        for d in &module.data {
            let offset = if let Val::I32(x) = vm.eval_const_expr(&d.offset) {
                x as usize
            } else {
                panic!()
            };
            if let Some(mem) = &mut vm.mem {
                mem.set_data(offset, d.init.clone().into_iter().map(|x| x.0).collect());
            } else {
                panic!();
            }
        }

        if let Some(start) = &module.start {
            vm.call_func(start.func.clone());
        }

        if module.imports.len() != 0 {
            panic!();
        }

        vm.exports = module.exports.clone();

        vm
    }

    fn eval_const_expr(&self, expr: &Expr) -> Val {
        match &expr.0[..] {
            &[Instr::I32Const(x), Instr::End] => Val::I32(x),
            &[Instr::I64Const(x), Instr::End] => Val::I64(x),
            &[Instr::F32Const(x), Instr::End] => Val::F32(x),
            &[Instr::F64Const(x), Instr::End] => Val::F64(x),
            &[Instr::GlobalGet(GlobalIdx(i)), Instr::End] => self.globals[i as usize].clone(),
            _ => panic!(),
        }
    }

    fn call_func(&mut self, idx: FuncIdx) -> Val {
        unimplemented!();
    }

    pub fn export_call_func(&mut self, name: &str) -> Val {
        if let ExportDesc::Func(idx) = self.find_export(name).desc {
            self.call_func(idx)
        } else {
            panic!();
        }
    }

    fn find_export(&self, name: &str) -> Export {
        self.exports
            .iter()
            .find(|x| &x.name.0 == name)
            .unwrap()
            .clone()
    }
}
