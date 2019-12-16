use super::stack::{AdminInstr, Frame, FrameStack, Label, LabelStack, Stack};
use crate::structure::instructions::{Expr, Instr, Memarg};
use crate::structure::modules::{
    ExportDesc, Func, FuncIdx, GlobalIdx, ImportDesc, Mem, Module, Table, TypeIdx, TypedIdx,
};
use crate::structure::types::{
    ElemType, FuncType, GlobalType, Limits, MemType, Mut, TableType, ValType,
};
use crate::WasmError;
use typenum::Unsigned;

use super::numerics::Byteable;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use frunk::{from_generic, hlist::HList, into_generic, Generic, HCons, HNil};
use generic_array::GenericArray;
use std::cell::RefCell;
use std::cell::{Ref, RefMut};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::io::Cursor;
use std::rc::{Rc, Weak};

#[derive(Debug, Clone, PartialEq)]
struct MemInst {
    max: Option<usize>,
    data: Vec<u8>,
}

impl MemInst {
    const PAGE_SIZE: usize = 65536;
    const MAX_PAGE_SIZE: i32 = 65536;
}

#[derive(Clone, Debug, PartialEq)]
pub struct MemAddr(Rc<RefCell<MemInst>>);

impl MemAddr {
    fn mut_inst(&self) -> RefMut<MemInst> {
        self.0.borrow_mut()
    }

    fn inst(&self) -> Ref<MemInst> {
        self.0.borrow()
    }

    fn to_pos(memarg: &Memarg, ptr: i32) -> Option<i32> {
        ptr.checked_add(i32::try_from(memarg.offset).ok()?)
    }

    pub fn read_buffer(&self, pos: i32, len: i32) -> Option<Vec<u8>> {
        let pos = pos as usize;
        let len = len as usize;
        let raw = &self.inst().data;
        if pos.checked_add(len).map(|x| raw.len() < x).unwrap_or(true) {
            return None;
        }
        Some(Vec::from(&raw[pos..pos + len]))
    }

    pub fn get(&self, i: i32) -> Option<u8> {
        Some(self.read_buffer(i, 1)?[0])
    }

    pub fn write_buffer(&self, pos: i32, buf: &[u8]) -> Option<()> {
        let pos = pos as usize;
        let raw = &mut self.mut_inst().data;
        if pos
            .checked_add(buf.len())
            .map(|x| raw.len() < x)
            .unwrap_or(true)
        {
            return None;
        }
        for (i, x) in buf.into_iter().enumerate() {
            raw[pos + i] = *x;
        }
        Some(())
    }

    pub fn set(&self, i: i32, x: u8) -> Option<()> {
        self.write_buffer(i, &[x])
    }

    pub fn read<T: Byteable>(&self, memarg: &Memarg, ptr: i32) -> Result<T, WasmError> {
        let pos = MemAddr::to_pos(memarg, ptr).ok_or_else(|| WasmError::RuntimeError)?;
        Ok(T::from_bytes(GenericArray::from_slice(
            &self
                .read_buffer(pos, T::N::to_usize() as i32)
                .ok_or_else(|| WasmError::RuntimeError)?[..],
        )))
    }

    pub fn write<T: Byteable>(&self, memarg: &Memarg, ptr: i32, x: T) -> Result<(), WasmError> {
        let pos = MemAddr::to_pos(memarg, ptr).ok_or_else(|| WasmError::RuntimeError)?;
        let buf = x.to_bytes();
        self.write_buffer(pos, &buf[..])
            .ok_or_else(|| WasmError::RuntimeError)
    }

    pub fn type_(&self) -> MemType {
        MemType(Limits {
            min: self.page_size() as u32,
            max: self.inst().max.map(|x| x as u32),
        })
    }

    pub fn alloc(type_: &MemType) -> MemAddr {
        let min = type_.0.min;
        let max = type_.0.max;
        MemAddr::new(min, max)
    }

    pub fn new(min: u32, max: Option<u32>) -> MemAddr {
        let min = min as usize * MemInst::PAGE_SIZE;
        let max = max.map(|max| max as usize);
        MemAddr(Rc::new(RefCell::new(MemInst {
            max,
            data: {
                let mut vec = Vec::with_capacity(min);
                vec.resize(min, 0);
                vec
            },
        })))
    }

    pub fn instantiation_valid(&self, offset: usize, init: Vec<u8>) -> Result<(), WasmError> {
        if offset
            .checked_add(init.len())
            .map(|x| x > self.inst().data.len())
            .unwrap_or(true)
        {
            Err(WasmError::LinkError)
        } else {
            Ok(())
        }
    }

    pub fn init_data(&self, offset: usize, init: Vec<u8>) {
        let inst = &mut *self.mut_inst();
        for (i, x) in init.into_iter().enumerate() {
            inst.data[offset + i] = x;
        }
    }

    pub fn page_size(&self) -> i32 {
        (self.inst().data.len() / MemInst::PAGE_SIZE) as i32
    }

    pub fn grow(&self, add_size: i32) -> i32 {
        let prev = self.page_size();
        let new_size = prev + add_size;
        if self
            .inst()
            .max
            .map(|max| new_size as usize > max)
            .unwrap_or(false)
            || new_size > MemInst::MAX_PAGE_SIZE
        {
            -1
        } else {
            self.mut_inst()
                .data
                .resize(new_size as usize * MemInst::PAGE_SIZE, 0);
            prev
        }
    }
}

pub struct MemAddrIterator(MemAddr, i32);

impl Iterator for MemAddrIterator {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.0.get(self.1)?;
        self.1 += 1;
        Some(res)
    }
}

impl IntoIterator for MemAddr {
    type Item = u8;
    type IntoIter = MemAddrIterator;

    fn into_iter(self) -> Self::IntoIter {
        MemAddrIterator(self, 0)
    }
}
