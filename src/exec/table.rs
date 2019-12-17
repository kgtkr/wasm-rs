use super::FuncAddr;
use crate::exec::instance::TypedIdxAccess;
use crate::structure::modules::FuncIdx;
use crate::structure::types::{ElemType, Limits, TableType};
use crate::WasmError;
use std::cell::RefCell;
use std::cell::{Ref, RefMut};
use std::rc::Rc;

#[derive(Debug, Clone)]
struct TableInst {
    max: Option<usize>,
    elem: Vec<Option<FuncAddr>>,
}

#[derive(Clone, Debug)]
pub struct TableAddr(Rc<RefCell<TableInst>>);

impl TableAddr {
    fn mut_inst(&self) -> RefMut<TableInst> {
        self.0.borrow_mut()
    }

    fn inst(&self) -> Ref<TableInst> {
        self.0.borrow()
    }

    pub fn new(min: u32, max: Option<u32>) -> TableAddr {
        TableAddr(Rc::new(RefCell::new(TableInst {
            max: max.map(|x| x as usize),
            elem: {
                let min = min as usize;
                let mut vec = Vec::with_capacity(min);
                vec.resize(min, None);
                vec
            },
        })))
    }

    pub(super) fn alloc(table: &TableType) -> TableAddr {
        TableAddr::new(table.0.min, table.0.max)
    }

    pub fn type_(&self) -> TableType {
        let inst = self.inst();
        TableType(
            Limits {
                min: inst.elem.len() as u32,
                max: inst.max.map(|x| x as u32),
            },
            ElemType::FuncRef,
        )
    }

    pub(super) fn instantiation_valid(
        &self,
        offset: usize,
        init: Vec<FuncIdx>,
    ) -> Result<(), WasmError> {
        let inst = self.inst();
        if offset
            .checked_add(init.len())
            .map(|x| x > inst.elem.len())
            .unwrap_or(true)
        {
            Err(WasmError::LinkError)
        } else {
            Ok(())
        }
    }

    pub(super) fn init_elem(&self, funcs: &Vec<FuncAddr>, offset: usize, init: Vec<FuncIdx>) {
        let mut inst = self.mut_inst();
        for (i, x) in init.into_iter().enumerate() {
            inst.elem[offset + i] = Some(funcs.get_idx(x).clone());
        }
    }

    pub fn len(&self) -> i32 {
        self.inst().elem.len() as i32
    }

    pub fn get(&self, i: i32) -> Option<Option<FuncAddr>> {
        let i = i as usize;
        if i < self.len() as usize {
            Some(self.inst().elem[i].clone())
        } else {
            None
        }
    }

    pub fn set(&self, i: i32, x: Option<FuncAddr>) -> Option<()> {
        let i = i as usize;
        if i < self.len() as usize {
            self.mut_inst().elem[i] = x;
            Some(())
        } else {
            None
        }
    }

    pub fn grow(&self, add_size: i32) -> Option<i32> {
        let cur_len = self.len();
        if self
            .inst()
            .max
            .map(|max| max as i32)
            .map(|max| max >= cur_len + add_size)
            .unwrap_or(true)
        {
            self.mut_inst()
                .elem
                .resize((cur_len + add_size) as usize, None);
            Some(cur_len)
        } else {
            None
        }
    }
}
