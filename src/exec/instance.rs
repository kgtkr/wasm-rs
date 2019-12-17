use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    ExportDesc, FuncIdx, GlobalIdx, ImportDesc, Module, TypeIdx, TypedIdx,
};
use crate::structure::types::{FuncType, ValType};
use crate::WasmError;

use super::global::GlobalAddr;
use super::mem::MemAddr;
use super::table::TableAddr;
use super::FuncAddr;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use frunk::{hlist::HList, HCons, HNil};
use std::collections::HashMap;
use std::io::Cursor;
use std::rc::Rc;

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

pub trait PrimitiveVal: Sized {
    fn try_from_val(val: Val) -> Option<Self>;
    fn wrap_val(self) -> Val;
    fn type_() -> ValType;
}

impl PrimitiveVal for i32 {
    fn try_from_val(value: Val) -> Option<i32> {
        if let Val::I32(x) = value {
            Some(x)
        } else {
            None
        }
    }

    fn wrap_val(self) -> Val {
        Val::I32(self)
    }

    fn type_() -> ValType {
        ValType::I32
    }
}

impl PrimitiveVal for i64 {
    fn try_from_val(value: Val) -> Option<i64> {
        if let Val::I64(x) = value {
            Some(x)
        } else {
            None
        }
    }

    fn wrap_val(self) -> Val {
        Val::I64(self)
    }

    fn type_() -> ValType {
        ValType::I64
    }
}

impl PrimitiveVal for f32 {
    fn try_from_val(value: Val) -> Option<f32> {
        if let Val::F32(x) = value {
            Some(x)
        } else {
            None
        }
    }

    fn wrap_val(self) -> Val {
        Val::F32(self)
    }

    fn type_() -> ValType {
        ValType::F32
    }
}

impl PrimitiveVal for f64 {
    fn try_from_val(value: Val) -> Option<f64> {
        if let Val::F64(x) = value {
            Some(x)
        } else {
            None
        }
    }

    fn wrap_val(self) -> Val {
        Val::F64(self)
    }

    fn type_() -> ValType {
        ValType::F64
    }
}

pub trait InterpretPrimitive {
    type Primitive: PrimitiveVal;

    fn to_primitive(self) -> Self::Primitive;
    fn reinterpret(primitive: Self::Primitive) -> Self;
}

impl<T: PrimitiveVal> InterpretPrimitive for T {
    type Primitive = T;

    fn to_primitive(self) -> T {
        self
    }
    fn reinterpret(primitive: T) -> T {
        primitive
    }
}

impl InterpretPrimitive for bool {
    type Primitive = i32;

    fn to_primitive(self) -> i32 {
        if self {
            1
        } else {
            0
        }
    }

    fn reinterpret(primitive: i32) -> bool {
        primitive != 0
    }
}

impl InterpretPrimitive for u32 {
    type Primitive = i32;

    fn to_primitive(self) -> i32 {
        let mut wtr = vec![];
        wtr.write_u32::<LittleEndian>(self).unwrap();
        let mut rdr = Cursor::new(wtr);
        rdr.read_i32::<LittleEndian>().unwrap()
    }

    fn reinterpret(primitive: i32) -> u32 {
        let mut wtr = vec![];
        wtr.write_i32::<LittleEndian>(primitive).unwrap();
        let mut rdr = Cursor::new(wtr);
        rdr.read_u32::<LittleEndian>().unwrap()
    }
}

impl InterpretPrimitive for u64 {
    type Primitive = i64;

    fn to_primitive(self) -> i64 {
        let mut wtr = vec![];
        wtr.write_u64::<LittleEndian>(self).unwrap();
        let mut rdr = Cursor::new(wtr);
        rdr.read_i64::<LittleEndian>().unwrap()
    }

    fn reinterpret(primitive: i64) -> u64 {
        let mut wtr = vec![];
        wtr.write_i64::<LittleEndian>(primitive).unwrap();
        let mut rdr = Cursor::new(wtr);
        rdr.read_u64::<LittleEndian>().unwrap()
    }
}

pub trait InterpretVal: Sized {
    fn try_interpret_val(val: Val) -> Option<Self>;
    fn to_val(self) -> Val;
}

impl<T: InterpretPrimitive> InterpretVal for T {
    fn try_interpret_val(val: Val) -> Option<T> {
        Some(T::reinterpret(T::Primitive::try_from_val(val)?))
    }
    fn to_val(self) -> Val {
        self.to_primitive().wrap_val()
    }
}

impl InterpretVal for Val {
    fn try_interpret_val(val: Val) -> Option<Val> {
        Some(val)
    }
    fn to_val(self) -> Val {
        self
    }
}

pub type ExternalModule = HashMap<String, ExternalVal>;
pub type ImportObjects = HashMap<String, ExternalModule>;

#[derive(Debug, Clone)]
pub enum ExternalVal {
    Func(FuncAddr),
    Table(TableAddr),
    Mem(MemAddr),
    Global(GlobalAddr),
}

impl ExternalVal {
    pub fn as_func(self) -> Option<FuncAddr> {
        if let ExternalVal::Func(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub fn unwrap_func(self) -> FuncAddr {
        self.as_func().unwrap()
    }

    pub fn as_table(self) -> Option<TableAddr> {
        if let ExternalVal::Table(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub fn unwrap_table(self) -> TableAddr {
        self.as_table().unwrap()
    }

    pub fn as_mem(self) -> Option<MemAddr> {
        if let ExternalVal::Mem(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub fn unwrap_mem(self) -> MemAddr {
        self.as_mem().unwrap()
    }

    pub fn as_global(self) -> Option<GlobalAddr> {
        if let ExternalVal::Global(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub fn unwrap_global(self) -> GlobalAddr {
        self.as_global().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct ExportInst {
    name: String,
    value: ExternalVal,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Val {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Val {
    pub fn val_type(&self) -> ValType {
        match self {
            Val::I32(_) => ValType::I32,
            Val::I64(_) => ValType::I64,
            Val::F32(_) => ValType::F32,
            Val::F64(_) => ValType::F64,
        }
    }

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

impl TypedIdxAccess<TypeIdx> for Vec<FuncType> {}
impl TypedIdxAccess<FuncIdx> for Vec<FuncAddr> {}
impl TypedIdxAccess<GlobalIdx> for Vec<GlobalAddr> {}

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
    pub fn new(
        module: &Module,
        imports_objects: ImportObjects,
    ) -> Result<Rc<ModuleInst>, WasmError> {
        let mut result = ModuleInst {
            types: module.types.clone(),
            funcs: Vec::new(),
            table: None,
            mem: None,
            globals: Vec::new(),
            exports: Vec::new(),
        };

        for import in &module.imports {
            let val = imports_objects
                .get(&import.module.0)
                .and_then(|module| module.get(&import.name.0))
                .cloned()
                .ok_or_else(|| WasmError::LinkError)?;
            match &import.desc {
                ImportDesc::Func(idx) => {
                    result.funcs.push(
                        val.as_func()
                            .filter(|func| func.type_().is_match(result.types.get_idx(*idx)))
                            .ok_or_else(|| WasmError::LinkError)?,
                    );
                }
                ImportDesc::Table(type_) => {
                    let _ = result.table.replace(
                        val.as_table()
                            .filter(|table| table.type_().is_match(type_))
                            .ok_or_else(|| WasmError::LinkError)?,
                    );
                }
                ImportDesc::Mem(type_) => {
                    let _ = result.mem.replace(
                        val.as_mem()
                            .filter(|mem| mem.type_().is_match(type_))
                            .ok_or_else(|| WasmError::LinkError)?,
                    );
                }
                ImportDesc::Global(type_) => {
                    result.globals.push(
                        val.as_global()
                            .filter(|global| global.type_().is_match(type_))
                            .ok_or_else(|| WasmError::LinkError)?,
                    );
                }
            }
        }

        for _ in &module.funcs {
            result.funcs.push(FuncAddr::alloc_dummy());
        }

        if let Some(table) = module.tables.iter().next() {
            let _ = result.table.replace(TableAddr::alloc(&table.type_));
        }

        if let Some(mem) = module.mems.iter().next() {
            let _ = result.mem.replace(MemAddr::alloc(&mem.type_));
        }

        for global in &module.globals {
            result.globals.push(GlobalAddr::alloc(
                global.type_.clone(),
                result.eval_const_expr(&global.init),
            ));
        }

        for elem in &module.elem {
            let offset = result.eval_const_expr(&elem.offset).unwrap_i32() as usize;
            result
                .table
                .as_ref()
                .unwrap()
                .instantiation_valid(offset, elem.init.clone())?;
        }
        for data in &module.data {
            let offset = result.eval_const_expr(&data.offset).unwrap_i32() as usize;
            result.mem.as_ref().unwrap().instantiation_valid(
                offset,
                data.init.clone().into_iter().map(|x| x.0).collect(),
            )?;
        }

        for elem in &module.elem {
            let offset = result.eval_const_expr(&elem.offset).unwrap_i32() as usize;
            result
                .table
                .as_ref()
                .unwrap()
                .init_elem(&result.funcs, offset, elem.init.clone());
        }
        for data in &module.data {
            let offset = result.eval_const_expr(&data.offset).unwrap_i32() as usize;
            result
                .mem
                .as_ref()
                .unwrap()
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
            result.funcs[idx].replace_dummy(func.clone(), Rc::downgrade(&result));
        }

        if let Some(start) = &module.start {
            result.funcs.get_idx(start.func).clone().call(vec![])?;
        }

        Ok(result)
    }

    fn eval_const_expr(&self, expr: &Expr) -> Val {
        match &expr.0[..] {
            &[Instr::I32Const(x)] => Val::I32(x),
            &[Instr::I64Const(x)] => Val::I64(x),
            &[Instr::F32Const(x)] => Val::F32(x),
            &[Instr::F64Const(x)] => Val::F64(x),
            &[Instr::GlobalGet(i)] => self.globals[i.to_idx()].get(),
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

    pub fn exports(&self) -> ExternalModule {
        self.exports
            .clone()
            .into_iter()
            .map(|x| (x.name, x.value))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::Decoder;
    use maplit::hashmap;

    #[test]
    #[ignore]
    fn test_add() {
        let module = Module::decode_end(&std::fs::read("./example/add.wasm").unwrap()).unwrap();
        let instance = ModuleInst::new(&module, HashMap::new()).unwrap();
        assert_eq!(
            instance
                .export("add")
                .unwrap_func()
                .call(vec![Val::I32(3), Val::I32(5)]),
            Ok(Some(Val::I32(8)))
        );
    }

    #[test]
    #[ignore]
    fn test_gcd() {
        let module = Module::decode_end(&std::fs::read("./example/gcd.wasm").unwrap()).unwrap();
        let instance = ModuleInst::new(&module, HashMap::new()).unwrap();

        assert_eq!(
            instance
                .export("gcd")
                .unwrap_func()
                .call(vec![Val::I32(182), Val::I32(1029)]),
            Ok(Some(Val::I32(7)))
        );
    }

    #[test]
    #[ignore]
    fn test_pow() {
        let module = Module::decode_end(&std::fs::read("./example/pow.wasm").unwrap()).unwrap();
        let instance = ModuleInst::new(&module, HashMap::new()).unwrap();
        assert_eq!(
            instance
                .export("pow")
                .unwrap_func()
                .call(vec![Val::I32(2), Val::I32(10)]),
            Ok(Some(Val::I32(1024)))
        );
    }

    #[test]
    #[ignore]
    fn test_br_table() {
        let module =
            Module::decode_end(&std::fs::read("./example/br_table.wasm").unwrap()).unwrap();
        let instance = ModuleInst::new(&module, HashMap::new()).unwrap();

        assert_eq!(
            instance
                .export("br_table")
                .unwrap_func()
                .call(vec![Val::I32(0)]),
            Ok(Some(Val::I32(10)))
        );
        assert_eq!(
            instance
                .export("br_table")
                .unwrap_func()
                .call(vec![Val::I32(10)]),
            Ok(Some(Val::I32(30)))
        );
    }

    #[test]
    #[ignore]
    fn test_md5() {
        use std::ffi::CString;

        let module = Module::decode_end(&std::fs::read("./example/md5.wasm").unwrap()).unwrap();
        let instance = ModuleInst::new(&module, HashMap::new()).unwrap();

        let input_bytes = CString::new("abc").unwrap().into_bytes();
        let input_ptr = instance
            .export("alloc")
            .unwrap_func()
            .call(vec![Val::I32(input_bytes.len() as i32)])
            .unwrap()
            .unwrap()
            .unwrap_i32();

        instance
            .export("memory")
            .unwrap_mem()
            .write_buffer(input_ptr, &input_bytes[..]);

        let output_ptr = instance
            .export("md5")
            .unwrap_func()
            .call(vec![Val::I32(input_ptr as i32)])
            .unwrap()
            .unwrap()
            .unwrap_i32() as usize;

        let mem = instance.export("memory").unwrap_mem();
        assert_eq!(
            CString::new(
                mem.into_iter()
                    .skip(output_ptr)
                    .take_while(|x| *x != 0)
                    .collect::<Vec<_>>(),
            )
            .unwrap()
            .into_string()
            .unwrap(),
            "900150983cd24fb0d6963f7d28e17f72".to_string()
        );
    }

    #[test]
    #[ignore]
    fn test_cl8w_gcd() {
        let memory = ExternalVal::Mem(MemAddr::new(10, None));
        let print = ExternalVal::Func(FuncAddr::alloc_host(|(x,): (i32,)| {
            println!("{}", x);
            Ok(())
        }));

        let memory_module =
            Module::decode_end(&std::fs::read("./example/memory.wasm").unwrap()).unwrap();
        let memory_instance = ModuleInst::new(
            &memory_module,
            hashmap! {
                "resource".to_string() => hashmap!{
                    "memory".to_string() => memory.clone()
                }
            },
        )
        .unwrap();

        let main_module =
            Module::decode_end(&std::fs::read("./example/cl8w-gcd.wasm").unwrap()).unwrap();
        let main_instance = ModuleInst::new(
            &main_module,
            hashmap! {
                "resource".to_string() => hashmap!{
                    "memory".to_string() => memory.clone()
                },
                "memory".to_string() => memory_instance.exports(),
                "io".to_string() => hashmap!{
                    "print".to_string() => print.clone()
                }
            },
        )
        .unwrap();

        main_instance
            .export("main")
            .unwrap_func()
            .call(vec![])
            .unwrap();
    }

    #[test]
    #[ignore]
    fn test_cl8w_ex() {
        let memory = ExternalVal::Mem(MemAddr::new(10, None));
        let print = ExternalVal::Func(FuncAddr::alloc_host(|(x,): (i32,)| {
            println!("{}", x);
            Ok(())
        }));

        let memory_module =
            Module::decode_end(&std::fs::read("./example/memory.wasm").unwrap()).unwrap();
        let memory_instance = ModuleInst::new(
            &memory_module,
            hashmap! {
                "resource".to_string() => hashmap!{
                    "memory".to_string() => memory.clone()
                }
            },
        )
        .unwrap();

        let main_module =
            Module::decode_end(&std::fs::read("./example/cl8w-ex.wasm").unwrap()).unwrap();
        let main_instance = ModuleInst::new(
            &main_module,
            hashmap! {
                "resource".to_string() => hashmap!{
                    "memory".to_string() => memory.clone()
                },
                "memory".to_string() => memory_instance.exports(),
                "io".to_string() => hashmap!{
                    "print".to_string() => print.clone()
                }
            },
        )
        .unwrap();

        main_instance
            .export("main")
            .unwrap_func()
            .call(vec![])
            .unwrap();
    }

    #[test]
    #[ignore]
    fn test_self_host() {
        let print = ExternalVal::Func(FuncAddr::alloc_host(|(x,): (i32,)| {
            println!("{}", x);
            Ok(())
        }));

        let module =
            Module::decode_end(&std::fs::read("./example/wasm-rs-self-host.wasm").unwrap())
                .unwrap();
        let instance = ModuleInst::new(
            &module,
            hashmap! {
                "env".to_string() => hashmap!{
                    "print".to_string() => print
                }
            },
        )
        .unwrap();

        instance.export("run").unwrap_func().call(vec![]).unwrap();
    }
}
