use crate::binary::Decoder;
use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, GlobalIdx, LocalIdx, Mem, Module, Table,
    TypeIdx, TypedIdx,
};
use crate::structure::types::{FuncType, Limits, MemType, ResultType, TableType, ValType};

/*
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

trait TypedIdxAccess<Idx>
where
    Idx: TypedIdx,
    Self: std::ops::Index<usize>,
{
    fn get_idx(&self, idx: Idx) -> &Self::Output {
        &self[idx.to_idx()]
    }
}

impl TypedIdxAccess<FuncIdx> for Vec<Func> {}
impl TypedIdxAccess<TypeIdx> for Vec<FuncType> {}

#[derive(Debug, Clone, PartialEq)]
struct Frame {
    locals: Vec<Val>,
    ret: (FuncIdx, usize),
    old_fp: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum StackVal {
    Val(Val),
    Frame(Frame),
}

impl StackVal {
    fn unwrap_val(&self) -> Val {
        if let StackVal::Val(x) = self {
            *x
        } else {
            panic!();
        }
    }

    fn unwrap_frame(&self) -> Frame {
        if let StackVal::Frame(x) = self {
            x.clone()
        } else {
            panic!();
        }
    }

    fn unwrap_frame_mut(&mut self) -> &mut Frame {
        if let StackVal::Frame(x) = self {
            x
        } else {
            panic!();
        }
    }
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
    nest_infos: Vec<Vec<(usize, usize)>>,
}

impl VM {
    fn push(&mut self, x: StackVal) {
        self.stack[self.sp] = x;
        self.sp += 1;
    }

    fn peak(&self) -> &StackVal {
        &self.stack[self.sp - 1]
    }

    fn pop(&mut self) -> &StackVal {
        self.sp -= 1;
        &self.stack[self.sp]
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
            nest_infos: Vec::new(),
        };

        vm.funcs = module.funcs;
        vm.nest_infos = vm
            .funcs
            .iter()
            .map(|x| block_nest_info(&x.body.0))
            .collect::<Vec<_>>();
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
            let offset = vm.eval_const_expr(&elem.offset).unwrap_i32() as usize;
            vm.table
                .as_mut()
                .unwrap()
                .set_elem(offset, elem.init.clone());
        }

        for d in &module.data {
            let offset = vm.eval_const_expr(&d.offset).unwrap_i32() as usize;
            vm.mem
                .as_mut()
                .unwrap()
                .set_data(offset, d.init.clone().into_iter().map(|x| x.0).collect());
        }

        if let Some(start) = &module.start {
            vm.call_func(start.func.clone(), &vec![]);
        }

        if module.imports.len() != 0 {
            unimplemented!("");
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
            &[Instr::GlobalGet(i), Instr::End] => self.globals[i.to_idx()].clone(),
            _ => panic!(),
        }
    }

    fn call_func(&mut self, idx: FuncIdx, params: &Vec<Val>) -> Option<Val> {
        let func = self.funcs.get_idx(idx);
        let is_rets = self.types.get_idx(func.type_).1.len() != 0;

        self.sp = 0;

        for p in params {
            self.push(StackVal::Val(p.clone()));
        }
        let ret = (FuncIdx(self.funcs.len() as u32), 0);
        self.cmd_frame(idx, ret);
        while self.pc != ret {
            self.run_cmd();
        }

        if is_rets {
            Some(self.pop().unwrap_val())
        } else {
            None
        }
    }

    fn get_cur_func(&self) -> &Func {
        self.funcs.get_idx(self.pc.0)
    }

    fn run_cmd(&mut self) {
        let istr = self.get_cur_func().body.0[self.pc.1].clone();

        match istr {
            Instr::I32Const(x) => {
                self.push(StackVal::Val(Val::I32(x)));
                self.pc.1 += 1;
            }
            Instr::LocalGet(idx) => {
                self.push(StackVal::Val(
                    self.stack[self.fp].unwrap_frame().locals[idx.to_idx()].clone(),
                ));
                self.pc.1 += 1;
            }
            Instr::I32Add => {
                let b = self.pop().unwrap_val().unwrap_i32();
                let a = self.pop().unwrap_val().unwrap_i32();
                self.push(StackVal::Val(Val::I32(a + b)));
                self.pc.1 += 1;
            }
            Instr::I32RemS => {
                let b = self.pop().unwrap_val().unwrap_i32();
                let a = self.pop().unwrap_val().unwrap_i32();
                self.push(StackVal::Val(Val::I32(a % b)));
                self.pc.1 += 1;
            }
            Instr::I32Eqz => {
                let a = self.pop().unwrap_val().unwrap_i32();
                self.push(StackVal::Val(Val::I32(if a == 0 { 1 } else { 0 })));
                self.pc.1 += 1;
            }
            Instr::Call(x) => {
                self.cmd_frame(x, (self.pc.0, self.pc.1 + 1));
            }
            Instr::If(_) => {
                let x = self.pop().unwrap_val().unwrap_i32();
                if x != 0 {
                    self.pc.1 += 1;
                } else {
                    let is = &self.get_cur_func().body.0;
                    let nest_infos = &self.nest_infos[self.pc.0.to_idx()];
                    let cur_info = nest_infos[self.pc.1 + 1];
                    for i in self.pc.1 + 1..is.len() {
                        if cur_info == nest_infos[i] {
                            match is[i] {
                                Instr::Else => {
                                    self.pc.1 = i;
                                    break;
                                }
                                Instr::End => {
                                    self.pc.1 = i;
                                    break;
                                }
                                _ => {}
                            }
                        }
                    }
                    self.pc.1 += 1;
                }
            }
            Instr::Else => {
                self.pc.1 = self.nest_infos[self.pc.0.to_idx()][self.pc.1].1;
            }
            Instr::Return => {
                self.cmd_ret();
            }
            Instr::End if self.get_cur_func().body.0.len() - 1 == self.pc.1 => {
                // 関数のend
                self.cmd_ret();
            }
            Instr::End
                if if let Instr::If(_) = self.get_cur_func().body.0
                    [self.nest_infos[self.pc.0.to_idx()][self.pc.1 - 1].0 - 1]
                {
                    true
                } else {
                    false
                } =>
            {
                // ifのend
                self.pc.1 += 1;
            }
            x => unimplemented!("{:?}", x),
        }
    }

    // frame命令
    fn cmd_frame(&mut self, idx: FuncIdx, ret: (FuncIdx, usize)) {
        let func = self.funcs.get_idx(idx);
        let func_type = self.types.get_idx(func.type_);
        let params_len = func_type.0.len();
        let local_types = func.locals.clone();

        let mut locals = Vec::new();
        for i in 1..=params_len {
            locals.push(
                self.stack[self.sp - params_len + i - 1]
                    .unwrap_val()
                    .clone(),
            );
        }
        self.sp -= params_len;
        for l in local_types {
            locals.push(match l {
                ValType::I32 => Val::I32(0),
                ValType::I64 => Val::I64(0),
                ValType::F32 => Val::F32(0.0),
                ValType::F64 => Val::F64(0.0),
            });
        }

        self.push(StackVal::Frame(Frame {
            ret,
            old_fp: self.fp,
            locals,
        }));

        self.fp = self.sp - 1;
        self.pc = (idx, 0);
    }

    // ret + fopr
    fn cmd_ret(&mut self) {
        let func = self.get_cur_func();
        let is_rets = self.types.get_idx(func.type_).1.len() != 0;

        let ret = if is_rets {
            Some(self.pop().clone())
        } else {
            None
        };
        let frame = self.pop().unwrap_frame();
        self.fp = frame.old_fp;
        self.pc = frame.ret;

        if let Some(ret) = ret {
            self.push(ret.clone());
        }
    }

    pub fn export_call_func(&mut self, name: &str, params: &Vec<Val>) -> Option<Val> {
        self.call_func(self.find_export(name).desc.unwrap_func(), params)
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

#[test]
fn test_gcd() {
    let module = Module::decode_end(&std::fs::read("./example/gcd.wasm").unwrap()).unwrap();
    let mut vm = VM::new(module);

    assert_eq!(
        vm.export_call_func("gcd", &vec![Val::I32(182), Val::I32(1029)]),
        Some(Val::I32(7))
    );
}
*/
