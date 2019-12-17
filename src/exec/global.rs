use crate::structure::types::{GlobalType, Mut};

use super::val::Val;
use std::cell::RefCell;
use std::cell::{Ref, RefMut};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct GlobalInst {
    value: Val,
    mut_: Mut,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GlobalAddr(Rc<RefCell<GlobalInst>>);

impl GlobalAddr {
    fn mut_inst(&self) -> RefMut<GlobalInst> {
        self.0.borrow_mut()
    }

    fn inst(&self) -> Ref<GlobalInst> {
        self.0.borrow()
    }

    pub fn type_(&self) -> GlobalType {
        let inst = self.inst();
        GlobalType(inst.mut_.clone(), inst.value.val_type())
    }

    pub fn new(mut_: Mut, val: Val) -> GlobalAddr {
        GlobalAddr(Rc::new(RefCell::new(GlobalInst { value: val, mut_ })))
    }

    pub(super) fn alloc(type_: GlobalType, val: Val) -> GlobalAddr {
        GlobalAddr::new(type_.0, val)
    }

    pub fn get(&self) -> Val {
        self.inst().value
    }

    pub fn set(&self, val: Val) -> Option<()> {
        let mut inst = self.mut_inst();
        if inst.mut_ == Mut::Var && inst.value.val_type() == val.val_type() {
            inst.value = val;
            Some(())
        } else {
            None
        }
    }
}
