use crate::binary::Decoder;
use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, GlobalIdx, LocalIdx, Mem, Module, Table,
};
use crate::structure::types::{FuncType, Limits, MemType, TableType, ValType};

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Val {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone, PartialEq, Copy)]
enum StackVal {
    Val(Val),
    PC((FuncIdx, usize)),
    Stack(usize),
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
    // 現在実行中の関数のフレームポインタ(旧フレームポインタが入ってるスタックのアドレス。最初のローカル変数の一個前のアドレス)
    fp: usize,
    // 次に実行する命令のアドレス
    pc: (FuncIdx, usize),
    globals: Vec<Val>,
    stack: Vec<StackVal>,
    sp: usize,
    table: Option<VMTable>,
    mem: Option<VMMem>,
    exports: Vec<Export>,
    types: Vec<FuncType>,
}

impl VM {
    fn push(&mut self, x: StackVal) {
        self.stack[self.sp] = x;
        self.sp += 1;
    }

    fn peak(&self) -> StackVal {
        self.stack[self.sp - 1]
    }

    fn pop(&mut self) -> StackVal {
        let x = self.peak();
        self.sp -= 1;
        x
    }

    pub fn new(module: Module) -> VM {
        let mut vm = VM {
            funcs: Vec::new(),
            globals: Vec::new(),
            mem: None,
            stack: {
                let stack_size = 100000;
                let mut vec = Vec::with_capacity(stack_size);
                vec.resize(stack_size, StackVal::Val(Val::I32(0)));
                vec
            },
            sp: 0,
            table: None,
            exports: Vec::new(),
            fp: 0,
            pc: (FuncIdx(0), 0),
            types: module.types,
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
            vm.call_func(start.func.clone(), &vec![]);
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

    fn call_func(&mut self, idx: FuncIdx, params: &Vec<Val>) -> Option<Val> {
        let func = &self.funcs[idx.0 as usize];
        let is_rets = self.types[func.type_.0 as usize].1.len() != 0;

        self.sp = 0;

        for p in params {
            self.push(StackVal::Val(p.clone()));
        }
        self.push(StackVal::PC((FuncIdx(0), 0)));
        self.cmd_frame(idx);
        self.run_cmd();
        while self.sp != if is_rets { 1 } else { 0 } {
            self.run_cmd();
        }

        if is_rets {
            Some(if let StackVal::Val(x) = self.pop() {
                x
            } else {
                panic!()
            })
        } else {
            None
        }
    }

    fn get_local_idx(&self, idx: LocalIdx) -> usize {
        let func = &self.funcs[(self.pc.0).0 as usize];
        let func_type = &self.types[func.type_.0 as usize];
        let params_len = func_type.0.len();
        let locals_len = func.locals.len();
        self.fp - params_len - locals_len + idx.0 as usize - 1
    }

    fn run_cmd(&mut self) {
        let istr = self.funcs[(self.pc.0).0 as usize].body.0[self.pc.1].clone();

        match istr {
            Instr::I32Const(x) => {
                self.push(StackVal::Val(Val::I32(x)));
                self.pc.1 += 1;
            }
            Instr::LocalGet(idx) => {
                self.push(self.stack[self.get_local_idx(idx)]);
                self.pc.1 += 1;
            }
            Instr::I32Add => {
                let b = if let StackVal::Val(Val::I32(x)) = self.pop() {
                    x
                } else {
                    panic!()
                };
                let a = if let StackVal::Val(Val::I32(x)) = self.pop() {
                    x
                } else {
                    panic!()
                };
                self.push(StackVal::Val(Val::I32(a + b)));
                self.pc.1 += 1;
            }
            Instr::End => {
                // TODO: loopとかのendと区別
                self.cmd_ret();
            }
            x => unimplemented!("{:?}", x),
        }
    }

    // frame命令
    fn cmd_frame(&mut self, idx: FuncIdx) {
        self.push(StackVal::Stack(self.fp));
        self.fp = self.sp - 1;
        let func = &self.funcs[idx.0 as usize];
        let func_type = &self.types[func.type_.0 as usize];
        let params_len = func_type.0.len();
        let locals = func.locals.clone();
        for i in 0..params_len {
            self.push(self.stack[self.fp - 1 - params_len + i].clone());
        }
        for l in locals {
            self.push(StackVal::Val(match l {
                ValType::I32 => Val::I32(0),
                ValType::I64 => Val::I64(0),
                ValType::F32 => Val::F32(0.0),
                ValType::F64 => Val::F64(0.0),
            }));
        }
        self.pc = (idx, 0);
    }

    // ret + fopr
    fn cmd_ret(&mut self) {
        let func = &self.funcs[(self.pc.0).0 as usize];
        let locals_count = func.locals.len();
        let params_count = self.types[func.type_.0 as usize].0.len();
        let is_rets = self.types[func.type_.0 as usize].1.len() != 0;

        let ret = if is_rets { Some(self.peak()) } else { None };
        self.sp = self.fp + 1;
        self.fp = if let StackVal::Stack(x) = self.pop() {
            x
        } else {
            panic!()
        };
        self.pc = if let StackVal::PC(x) = self.pop() {
            x
        } else {
            panic!()
        };
        self.sp -= locals_count + params_count;

        if let Some(ret) = ret {
            self.push(ret);
        }
    }

    pub fn export_call_func(&mut self, name: &str, params: &Vec<Val>) -> Option<Val> {
        if let ExportDesc::Func(idx) = self.find_export(name).desc {
            self.call_func(idx, params)
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

#[test]
fn test_add() {
    let module = Module::decode_end(&std::fs::read("./example/add.wasm").unwrap()).unwrap();
    let mut vm = VM::new(module);
    assert_eq!(
        vm.export_call_func("add", &vec![Val::I32(3), Val::I32(5)]),
        Some(Val::I32(8))
    );
}
