use crate::structure::instructions::{Expr, Instr};
use crate::structure::modules::{
    ExportDesc, FuncIdx, GlobalIdx, ImportDesc, Module, TypeIdx, TypedIdx,
};
use crate::structure::types::FuncType;
use crate::WasmError;

use super::global::GlobalAddr;
use super::mem::MemAddr;
use super::table::TableAddr;
use super::val::Val;
use super::FuncAddr;
use std::collections::HashMap;
use std::rc::Rc;

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
                .instantiation_valid(offset, &elem.init)?;
        }
        for data in &module.data {
            let offset = result.eval_const_expr(&data.offset).unwrap_i32() as usize;
            result
                .mem
                .as_ref()
                .unwrap()
                .instantiation_valid(offset, &data.init.iter().map(|x| x.0).collect())?;
        }

        for elem in &module.elem {
            let offset = result.eval_const_expr(&elem.offset).unwrap_i32() as usize;
            result
                .table
                .as_ref()
                .unwrap()
                .init_elem(&result.funcs, offset, &elem.init);
        }
        for data in &module.data {
            let offset = result.eval_const_expr(&data.offset).unwrap_i32() as usize;
            result
                .mem
                .as_ref()
                .unwrap()
                .init_data(offset, &data.init.iter().map(|x| x.0).collect());
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
            result.funcs.get_idx(start.func).call(vec![])?;
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
    }

    pub fn exports(&self) -> ExternalModule {
        self.exports
            .iter()
            .map(|x| (x.name.clone(), x.value.clone()))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::decode_module;
    use maplit::hashmap;

    #[test]
    #[ignore]
    fn test_add() {
        let module = decode_module(&std::fs::read("./example/add.wasm").unwrap()).unwrap();
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
        let module = decode_module(&std::fs::read("./example/gcd.wasm").unwrap()).unwrap();
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
        let module = decode_module(&std::fs::read("./example/pow.wasm").unwrap()).unwrap();
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
        let module = decode_module(&std::fs::read("./example/br_table.wasm").unwrap()).unwrap();
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

        let module = decode_module(&std::fs::read("./example/md5.wasm").unwrap()).unwrap();
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
            decode_module(&std::fs::read("./example/memory.wasm").unwrap()).unwrap();
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
            decode_module(&std::fs::read("./example/cl8w-gcd.wasm").unwrap()).unwrap();
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
            decode_module(&std::fs::read("./example/memory.wasm").unwrap()).unwrap();
        let memory_instance = ModuleInst::new(
            &memory_module,
            hashmap! {
                "resource".to_string() => hashmap!{
                    "memory".to_string() => memory.clone()
                }
            },
        )
        .unwrap();

        let main_module = decode_module(&std::fs::read("./example/cl8w-ex.wasm").unwrap()).unwrap();
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
            decode_module(&std::fs::read("./example/wasm-rs-self-host.wasm").unwrap()).unwrap();
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
