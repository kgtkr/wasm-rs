use super::stack::{AdminInstr, Frame, FrameStack, Label, LabelStack, Stack};
use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    ExportDesc, Func, FuncIdx, GlobalIdx, ImportDesc, Module, Table, TypeIdx, TypedIdx,
};
use crate::structure::types::{ElemType, FuncType, GlobalType, Limits, Mut, TableType, ValType};
use crate::WasmError;

use super::instance::{FromVecVal, ModuleInst, ToOptionVal, TypedIdxAccess, Val, ValTypeable};
use super::mem::MemAddr;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use frunk::{from_generic, hlist::HList, into_generic, Generic, HCons, HNil};
use std::cell::Ref;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Cursor;
use std::rc::{Rc, Weak};

#[derive(Clone, Debug)]
pub struct FuncAddr(Rc<RefCell<FuncInst>>);

impl FuncAddr {
    pub fn call(&self, params: Vec<Val>) -> Result<Option<Val>, WasmError> {
        let mut stack = Stack {
            stack: vec![FrameStack {
                frame: Frame {
                    locals: Vec::new(),
                    module: Weak::new(),
                    n: 0,
                },
                stack: vec![LabelStack {
                    label: Label {
                        instrs: vec![],
                        n: 0,
                    },
                    instrs: vec![AdminInstr::Invoke(self.clone())],
                    stack: params,
                }],
            }],
        };

        loop {
            stack.step()?;
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

        Ok(stack.stack.pop().unwrap().stack.pop().unwrap().stack.pop())
    }

    pub(super) fn borrow(&self) -> Ref<FuncInst> {
        self.0.borrow()
    }

    pub(super) fn alloc_dummy() -> FuncAddr {
        FuncAddr(Rc::new(RefCell::new(FuncInst::RuntimeFunc {
            type_: FuncType(Vec::new(), Vec::new()),
            code: Func {
                type_: TypeIdx(0),
                locals: Vec::new(),
                body: Expr(Vec::new()),
            },
            module: Weak::new(),
        })))
    }

    pub(super) fn replace_dummy(&self, func: Func, module: Weak<ModuleInst>) {
        *self.0.borrow_mut() = FuncInst::new(func, module);
    }

    pub fn alloc_host<P: Generic, R: Generic>(
        f: impl Fn(P) -> Result<R, WasmError> + 'static,
    ) -> FuncAddr
    where
        P::Repr: ValTypeable + FromVecVal,
        R::Repr: ValTypeable + ToOptionVal,
    {
        let type_ = FuncType(P::Repr::to_valtype(), R::Repr::to_valtype());
        FuncAddr(Rc::new(RefCell::new(FuncInst::HostFunc {
            type_,
            host_code: Rc::new(move |params| {
                let p = P::Repr::from_vec_val(params);
                let r = into_generic(f(from_generic(p))?);
                Ok(r.to_option_val())
            }),
        })))
    }

    pub fn type_(&self) -> FuncType {
        match &*self.0.borrow() {
            FuncInst::RuntimeFunc { type_, .. } => type_.clone(),
            FuncInst::HostFunc { type_, .. } => type_.clone(),
        }
    }
}

#[derive(Clone)]
pub(super) enum FuncInst {
    RuntimeFunc {
        type_: FuncType,
        code: Func,
        module: Weak<ModuleInst>,
    },
    HostFunc {
        type_: FuncType,
        host_code: Rc<dyn Fn(Vec<Val>) -> Result<Option<Val>, WasmError>>,
    },
}

impl std::fmt::Debug for FuncInst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<FuncInst>")
    }
}

impl FuncInst {
    pub fn new(func: Func, module: Weak<ModuleInst>) -> FuncInst {
        FuncInst::RuntimeFunc {
            type_: module.upgrade().unwrap().types.get_idx(func.type_).clone(),
            code: func,
            module,
        }
    }
}
