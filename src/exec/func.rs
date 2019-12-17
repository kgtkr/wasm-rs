use super::instance::InterpretPrimitive;
use super::instance::{ModuleInst, PrimitiveVal, TypedIdxAccess, Val};
use super::stack::{AdminInstr, Frame, FrameStack, Label, LabelStack, Stack};
use crate::structure::instructions::Expr;
use crate::structure::modules::{Func, TypeIdx};
use crate::structure::types::{FuncType, ValType};
use crate::WasmError;
use frunk::{from_generic, into_generic, Generic};
use frunk::{hlist::HList, HCons, HNil};
use std::cell::Ref;
use std::cell::RefCell;
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

pub trait ValTypeable {
    fn write_valtype(types: &mut Vec<ValType>);
    fn to_valtype() -> Vec<ValType> {
        let mut types = Vec::new();
        Self::write_valtype(&mut types);
        types
    }
}

impl<T: InterpretPrimitive> ValTypeable for T {
    fn write_valtype(types: &mut Vec<ValType>) {
        types.push(T::Primitive::type_());
    }
}

impl ValTypeable for HNil {
    fn write_valtype(_: &mut Vec<ValType>) {}
}

impl<H: ValTypeable, T: HList + ValTypeable> ValTypeable for HCons<H, T> {
    fn write_valtype(types: &mut Vec<ValType>) {
        H::write_valtype(types);
        T::write_valtype(types);
    }
}

pub trait ToOptionVal {
    fn to_option_val(self) -> Option<Val>;
}

impl ToOptionVal for HNil {
    fn to_option_val(self) -> Option<Val> {
        None
    }
}

impl<T: ToOptionVal> ToOptionVal for HCons<T, HNil> {
    fn to_option_val(self) -> Option<Val> {
        self.head.to_option_val()
    }
}

impl<T: InterpretPrimitive> ToOptionVal for T {
    fn to_option_val(self) -> Option<Val> {
        Some(self.to_primitive().wrap_val())
    }
}

pub trait FromVecVal: Sized {
    fn from_vec_val_pop_tail(vals: &mut Vec<Val>) -> Self;

    fn from_vec_val(mut vals: Vec<Val>) -> Self {
        let res = Self::from_vec_val_pop_tail(&mut vals);
        assert_eq!(vals.len(), 0);
        res
    }
}

impl FromVecVal for HNil {
    fn from_vec_val_pop_tail(_: &mut Vec<Val>) -> Self {
        HNil
    }
}

impl<H: FromVecVal, T: HList + FromVecVal> FromVecVal for HCons<H, T> {
    fn from_vec_val_pop_tail(vals: &mut Vec<Val>) -> Self {
        let t = T::from_vec_val_pop_tail(vals);
        let h = H::from_vec_val_pop_tail(vals);
        HCons { head: h, tail: t }
    }
}

impl<T: InterpretPrimitive> FromVecVal for T {
    fn from_vec_val_pop_tail(vals: &mut Vec<Val>) -> Self {
        T::reinterpret(T::Primitive::try_from_val(vals.pop().unwrap()).unwrap())
    }
}
