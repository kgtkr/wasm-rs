#![feature(backtrace)]

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use lazy_static::lazy_static;
use maplit::hashmap;
use serde_json::Value;
use std::collections::HashMap;
use std::io::Cursor;
use std::panic;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::Path;
use std::rc::Rc;
use wasm_rs::binary::decode_module;
use wasm_rs::exec::FuncAddr;
use wasm_rs::exec::GlobalAddr;
use wasm_rs::exec::MemAddr;
use wasm_rs::exec::TableAddr;
use wasm_rs::exec::Val;
use wasm_rs::exec::{ExternalVal, ImportObjects, ModuleInst};
use wasm_rs::structure::types::Mut;
use wasm_rs::WasmError;

#[derive(Debug, Clone, Copy)]
struct SpecVal(Val);
impl PartialEq for SpecVal {
    fn eq(&self, other: &SpecVal) -> bool {
        match (self.0, other.0) {
            (Val::I32(a), Val::I32(b)) => a == b,
            (Val::I64(a), Val::I64(b)) => a == b,
            (Val::F32(a), Val::F32(b)) => {
                let mut a_bytes = Vec::new();
                a_bytes.write_f32::<LittleEndian>(a).unwrap();

                let mut b_bytes = Vec::new();
                b_bytes.write_f32::<LittleEndian>(b).unwrap();
                a_bytes == b_bytes
            }
            (Val::F64(a), Val::F64(b)) => {
                let mut a_bytes = Vec::new();
                a_bytes.write_f64::<LittleEndian>(a).unwrap();

                let mut b_bytes = Vec::new();
                b_bytes.write_f64::<LittleEndian>(b).unwrap();
                a_bytes == b_bytes
            }
            _ => false,
        }
    }
}

impl FromJSON for SpecVal {
    fn from_json(json: &Value) -> SpecVal {
        SpecVal(Val::from_json(json))
    }
}

trait FromJSON {
    fn from_json(json: &Value) -> Self;
}

impl FromJSON for Val {
    fn from_json(json: &Value) -> Val {
        let json_obj = json.as_object().unwrap();
        let mut buf = Vec::new();
        buf.write_u64::<LittleEndian>(
            json_obj
                .get("value")
                .unwrap()
                .as_str()
                .unwrap()
                .parse::<u64>()
                .unwrap(),
        )
        .unwrap();
        let mut rdr = Cursor::new(buf);
        match json_obj.get("type").unwrap().as_str().unwrap() {
            "i32" => Val::I32(rdr.read_i32::<LittleEndian>().unwrap()),
            "i64" => Val::I64(rdr.read_i64::<LittleEndian>().unwrap()),
            "f32" => Val::F32(rdr.read_f32::<LittleEndian>().unwrap()),
            "f64" => Val::F64(rdr.read_f64::<LittleEndian>().unwrap()),
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ActionKind {
    Invoke { field: String, args: Vec<Val> },
    Get { field: String },
}

impl FromJSON for ActionKind {
    fn from_json(json: &Value) -> ActionKind {
        let json_obj = json.as_object().unwrap();
        match json_obj.get("type").unwrap().as_str().unwrap() {
            "invoke" => ActionKind::Invoke {
                field: json_obj.get("field").unwrap().as_str().unwrap().to_string(),
                args: json_obj
                    .get("args")
                    .unwrap()
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(Val::from_json)
                    .collect::<Vec<_>>(),
            },
            "get" => ActionKind::Get {
                field: json_obj.get("field").unwrap().as_str().unwrap().to_string(),
            },
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Action {
    kind: ActionKind,
    module: Option<String>,
}

impl Action {
    fn run(&self, state: &mut SpecState) -> Result<Vec<SpecVal>, WasmError> {
        match &self.kind {
            ActionKind::Invoke { field, args } => state
                .instance(&self.module)
                .export(field)
                .unwrap_func()
                .call(args.clone())
                .map(|x| x.into_iter().map(SpecVal).collect::<Vec<_>>()),
            ActionKind::Get { field } => Ok(vec![SpecVal(
                state
                    .instance(&self.module)
                    .export(field)
                    .unwrap_global()
                    .get(),
            )]),
        }
    }
}

impl FromJSON for Action {
    fn from_json(json: &Value) -> Action {
        Action {
            kind: ActionKind::from_json(json),
            module: json
                .as_object()
                .unwrap()
                .get("module")
                .map(|x| x.as_str().unwrap().to_string()),
        }
    }
}

#[derive(Debug)]
struct SpecState {
    instances: Vec<Rc<ModuleInst>>,
    instance_map: HashMap<String, Rc<ModuleInst>>,
    registers: ImportObjects,
}

impl SpecState {
    fn new() -> SpecState {
        SpecState {
            instances: Vec::new(),
            instance_map: HashMap::new(),
            registers: hashmap! {
                "spectest".to_string() => hashmap! {
                    "print".to_string() => ExternalVal::Func(FuncAddr::alloc_host(|(): ()| Ok(()))),
                    "print_i32".to_string() => ExternalVal::Func(FuncAddr::alloc_host(|(_,): (i32,)| Ok(()))),
                    "print_i32_f32".to_string() => ExternalVal::Func(FuncAddr::alloc_host(|(_, _): (i32, f32)| Ok(()))),
                    "print_f64_f64".to_string() => ExternalVal::Func(FuncAddr::alloc_host(|(_, _): (f64, f64)| Ok(()))),
                    "print_f32".to_string() => ExternalVal::Func(FuncAddr::alloc_host(|(_,): (f32, )| Ok(()))),
                    "print_f64".to_string() => ExternalVal::Func(FuncAddr::alloc_host(|(_,): (f64,)| Ok(()))),
                    "global_i32".to_string() => ExternalVal::Global(GlobalAddr::new(Mut::Const, Val::I32(666))),
                    "global_f32".to_string() => ExternalVal::Global(GlobalAddr::new(Mut::Const, Val::F32(666.0))),
                    "global_f64".to_string() => ExternalVal::Global(GlobalAddr::new(Mut::Const, Val::F64(666.0))),
                    "table".to_string() => ExternalVal::Table(TableAddr::new(10, Some(20))),
                    "memory".to_string() => ExternalVal::Mem(MemAddr::new(1,Some(2))),
                }
            },
        }
    }

    fn instance(&self, name: &Option<String>) -> Rc<ModuleInst> {
        name.as_ref()
            .map(|name| self.instance_map.get(name).unwrap().clone())
            .unwrap_or_else(|| self.instances.last().unwrap().clone())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spec {
    commands: Vec<Command>,
    source_filename: String,
}

impl FromJSON for Spec {
    fn from_json(json: &Value) -> Spec {
        let json_obj = json.as_object().unwrap();
        Spec {
            commands: json_obj
                .get("commands")
                .unwrap()
                .as_array()
                .unwrap()
                .iter()
                .map(Command::from_json)
                .collect::<Vec<_>>(),
            source_filename: json_obj
                .get("source_filename")
                .unwrap()
                .as_str()
                .unwrap()
                .to_string(),
        }
    }
}

impl Spec {
    fn run(&self, base_dir: &Path, line: &mut i32) {
        let mut state = SpecState::new();

        for command in &self.commands {
            *line = command.line;
            command.run(base_dir, &mut state);
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Command {
    line: i32,
    paylaod: CommandKind,
}

impl Command {
    fn run(&self, base_dir: &Path, state: &mut SpecState) {
        match &self.paylaod {
            CommandKind::Module { filename, name } => {
                let module = ModuleInst::new(
                    &decode_module(&std::fs::read(base_dir.join(filename)).unwrap()).unwrap(),
                    state.registers.clone(),
                )
                .unwrap();
                state.instances.push(module.clone());
                if let Some(name) = name {
                    state.instance_map.insert(name.clone(), module.clone());
                }
            }
            CommandKind::AssertReturn { action, expected } => {
                assert_eq!(&action.run(state).unwrap(), expected);
            }
            CommandKind::AssertTrap { action } => {
                assert_eq!(action.run(state), Err(WasmError::RuntimeError));
            }
            CommandKind::Register { as_, name } => {
                state
                    .registers
                    .insert(as_.clone(), state.instance(name).exports());
            }
            CommandKind::Action { action, expected } => {
                assert_eq!(&action.run(state).unwrap(), expected);
            }
            CommandKind::AssertUninstantiable { filename } => {
                assert_eq!(
                    ModuleInst::new(
                        &decode_module(&std::fs::read(base_dir.join(filename)).unwrap()).unwrap(),
                        state.registers.clone(),
                    )
                    .unwrap_err(),
                    WasmError::RuntimeError
                );
            }
            CommandKind::AssertUnlinkable { filename } => {
                assert_eq!(
                    ModuleInst::new(
                        &decode_module(&std::fs::read(base_dir.join(filename)).unwrap()).unwrap(),
                        state.registers.clone(),
                    )
                    .unwrap_err(),
                    WasmError::LinkError
                );
            }
            CommandKind::Skip { .. } => {}
        }
    }
}

impl FromJSON for Command {
    fn from_json(json: &Value) -> Command {
        Command {
            line: json
                .as_object()
                .unwrap()
                .get("line")
                .unwrap()
                .as_i64()
                .unwrap() as i32,
            paylaod: CommandKind::from_json(json),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum CommandKind {
    Module {
        filename: String,
        name: Option<String>,
    },
    AssertReturn {
        action: Action,
        expected: Vec<SpecVal>,
    },
    Register {
        as_: String,
        name: Option<String>,
    },
    AssertTrap {
        action: Action,
    },
    Skip {
        type_: String,
    },
    AssertUnlinkable {
        filename: String,
    },
    AssertUninstantiable {
        filename: String,
    },
    Action {
        action: Action,
        expected: Vec<SpecVal>,
    },
}

impl FromJSON for CommandKind {
    fn from_json(json: &Value) -> CommandKind {
        let json_obj = json.as_object().unwrap();
        match json_obj.get("type").unwrap().as_str().unwrap() {
            "module" => CommandKind::Module {
                filename: json_obj
                    .get("filename")
                    .unwrap()
                    .as_str()
                    .unwrap()
                    .to_string(),
                name: json
                    .as_object()
                    .unwrap()
                    .get("name")
                    .map(|x| x.as_str().unwrap().to_string()),
            },
            "assert_return" => CommandKind::AssertReturn {
                action: Action::from_json(json_obj.get("action").unwrap()),
                expected: json_obj
                    .get("expected")
                    .unwrap()
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(SpecVal::from_json)
                    .collect::<Vec<_>>(),
            },
            "register" => CommandKind::Register {
                as_: json_obj.get("as").unwrap().as_str().unwrap().to_string(),
                name: json
                    .as_object()
                    .unwrap()
                    .get("name")
                    .map(|x| x.as_str().unwrap().to_string()),
            },
            "action" => CommandKind::Action {
                action: Action::from_json(json_obj.get("action").unwrap()),
                expected: json_obj
                    .get("expected")
                    .unwrap()
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(SpecVal::from_json)
                    .collect::<Vec<_>>(),
            },
            "assert_trap" => CommandKind::AssertTrap {
                action: Action::from_json(json_obj.get("action").unwrap()),
            },
            "assert_uninstantiable" => CommandKind::AssertUninstantiable {
                filename: json_obj
                    .get("filename")
                    .unwrap()
                    .as_str()
                    .unwrap()
                    .to_string(),
            },
            "assert_unlinkable" => CommandKind::AssertUnlinkable {
                filename: json_obj
                    .get("filename")
                    .unwrap()
                    .as_str()
                    .unwrap()
                    .to_string(),
            },
            "assert_malformed" => CommandKind::Skip {
                type_: "assert_malformed".to_string(),
            },
            "assert_invalid" => CommandKind::Skip {
                type_: "assert_invalid".to_string(),
            },
            "assert_exhaustion" => CommandKind::Skip {
                type_: "assert_exhaustion".to_string(),
            },
            "assert_return_canonical_nan" => CommandKind::Skip {
                type_: "assert_return_canonical_nan".to_string(),
            },
            "assert_return_arithmetic_nan" => CommandKind::Skip {
                type_: "assert_return_arithmetic_nan".to_string(),
            },
            ty => panic!("unknown type: {}", ty),
        }
    }
}

use std::sync::RwLock;

lazy_static! {
    pub static ref LAST_PANIC_MSG: RwLock<Option<String>> = { RwLock::new(None) };
}

#[test]
fn test_specs() {
    let default_hook = panic::take_hook();

    panic::set_hook(Box::new(move |panic_info| {
        *self::LAST_PANIC_MSG.write().unwrap() = Some(format!(
            "{}\n{}",
            panic_info,
            std::backtrace::Backtrace::capture()
        ));
        default_hook(panic_info);
    }));

    let base_dir = Path::new("./wast-dist");
    let mut passed_count = 0;
    let mut failed_count = 0;
    let mut fail_msgs = Vec::new();

    for file in std::fs::read_dir(base_dir).unwrap() {
        let filename = file.unwrap().file_name().into_string().unwrap();
        if filename.ends_with(".json") {
            let json_path = base_dir.join(filename);
            let mut line = 0;
            let spec = Spec::from_json(
                &serde_json::from_slice::<Value>(&std::fs::read(&json_path).unwrap()).unwrap(),
            );

            let line_ref = AssertUnwindSafe(&mut line);
            let spec_ref = &spec;
            match catch_unwind(move || {
                spec_ref.run(base_dir, line_ref.0);
            }) {
                Ok(_) => {
                    passed_count += 1;
                    println!("[passed]{}", spec.source_filename);
                }
                Err(e) => {
                    failed_count += 1;
                    fail_msgs.push(format!(
                        "[{}:{}] {}\n{}",
                        spec.source_filename,
                        line,
                        any_to_string(&*e),
                        self::LAST_PANIC_MSG.read().unwrap().as_ref().unwrap()
                    ));
                    println!("[failed]{}:{}", spec.source_filename, line);
                }
            }
        }
    }

    for msg in fail_msgs {
        println!("{}", msg);
    }

    println!("{} passed; {} failed;", passed_count, failed_count);
    if failed_count != 0 {
        panic!("test failed");
    }
}

fn any_to_string(any: &dyn std::any::Any) -> String {
    if let Some(s) = any.downcast_ref::<String>() {
        s.clone()
    } else if let Some(s) = any.downcast_ref::<&str>() {
        s.to_string()
    } else {
        "Any".to_string()
    }
}
