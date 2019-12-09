use crate::binary::Decoder;
use crate::exec::instance::{ModuleInst, RuntimeError, Val};
use crate::structure::modules::Module;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use serde_json::Value;
use std::collections::HashMap;
use std::io::Cursor;
use std::panic::{catch_unwind, resume_unwind, AssertUnwindSafe};
use std::rc::{Rc, Weak};

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
pub enum ActionPayload {
    Invoke { field: String, args: Vec<Val> },
    Get { field: String },
}

impl FromJSON for ActionPayload {
    fn from_json(json: &Value) -> ActionPayload {
        let json_obj = json.as_object().unwrap();
        match json_obj.get("type").unwrap().as_str().unwrap() {
            "invoke" => ActionPayload::Invoke {
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
            "get" => ActionPayload::Get {
                field: json_obj.get("field").unwrap().as_str().unwrap().to_string(),
            },
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Action {
    payload: ActionPayload,
    module: Option<String>,
}

impl Action {
    fn run(&self, state: &mut SpecState) -> Result<Vec<SpecVal>, RuntimeError> {
        match &self.payload {
            ActionPayload::Invoke { field, args } => state
                .instances
                .get(&self.module)
                .unwrap()
                .export(field)
                .unwrap_func()
                .call(args.clone())
                .map(|x| x.into_iter().map(SpecVal).collect::<Vec<_>>()),
            ActionPayload::Get { field } => Ok(vec![SpecVal(
                state
                    .instances
                    .get(&self.module)
                    .unwrap()
                    .export(field)
                    .unwrap_global()
                    .0
                    .borrow()
                    .value,
            )]),
        }
    }
}

impl FromJSON for Action {
    fn from_json(json: &Value) -> Action {
        Action {
            payload: ActionPayload::from_json(json),
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
    instances: HashMap<Option<String>, Rc<ModuleInst>>,
    registers: HashMap<String, Rc<ModuleInst>>,
}

impl SpecState {
    fn new() -> SpecState {
        SpecState {
            instances: HashMap::new(),
            registers: HashMap::new(),
        }
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
    fn run(&self, line: &mut i32) {
        let mut state = SpecState::new();

        for command in &self.commands {
            *line = command.line;
            command.run(&mut state);
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Command {
    line: i32,
    paylaod: CommandPayload,
}

impl Command {
    fn run(&self, state: &mut SpecState) {
        match &self.paylaod {
            CommandPayload::Module { filename, name } => {
                state.instances.insert(
                    name.clone(),
                    ModuleInst::new(
                        &Module::decode_end(&std::fs::read(format!("spec/{}", filename)).unwrap())
                            .unwrap(),
                        state
                            .registers
                            .iter()
                            .map(|(name, inst)| (name.clone(), inst.exports()))
                            .collect(),
                    ),
                );
            }
            CommandPayload::AssertReturn { action, expected } => {
                assert_eq!(&action.run(state).unwrap(), expected);
            }
            CommandPayload::AssertTrap { action } => {
                assert_eq!(action.run(state), Err(RuntimeError::Trap));
            }
            CommandPayload::Register { as_, name } => {
                state
                    .registers
                    .insert(as_.clone(), state.instances.get(name).unwrap().clone());
            }
            CommandPayload::Action { action, expected } => {
                assert_eq!(&action.run(state).unwrap(), expected);
            }
            CommandPayload::Skip { .. } => {}
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
            paylaod: CommandPayload::from_json(json),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum CommandPayload {
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
    Action {
        action: Action,
        expected: Vec<SpecVal>,
    },
}

impl FromJSON for CommandPayload {
    fn from_json(json: &Value) -> CommandPayload {
        let json_obj = json.as_object().unwrap();
        match json_obj.get("type").unwrap().as_str().unwrap() {
            "module" => CommandPayload::Module {
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
            "assert_return" => CommandPayload::AssertReturn {
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
            "register" => CommandPayload::Register {
                as_: json_obj.get("as").unwrap().as_str().unwrap().to_string(),
                name: json
                    .as_object()
                    .unwrap()
                    .get("name")
                    .map(|x| x.as_str().unwrap().to_string()),
            },
            "action" => CommandPayload::Action {
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
            "assert_trap" => CommandPayload::AssertTrap {
                action: Action::from_json(json_obj.get("action").unwrap()),
            },
            "assert_malformed" => CommandPayload::Skip {
                type_: "assert_malformed".to_string(),
            },
            "assert_invalid" => CommandPayload::Skip {
                type_: "assert_invalid".to_string(),
            },
            "assert_unlinkable" => CommandPayload::Skip {
                type_: "assert_unlinkable".to_string(),
            },
            "assert_exhaustion" => CommandPayload::Skip {
                type_: "assert_exhaustion".to_string(),
            },
            "assert_return_canonical_nan" => CommandPayload::Skip {
                type_: "assert_return_canonical_nan".to_string(),
            },
            "assert_return_arithmetic_nan" => CommandPayload::Skip {
                type_: "assert_return_arithmetic_nan".to_string(),
            },
            "assert_uninstantiable" => CommandPayload::Skip {
                type_: "assert_uninstantiable".to_string(),
            },
            ty => panic!("unknown type: {}", ty),
        }
    }
}

#[test]
fn spec_test() {
    let mut passed_count = 0;
    let mut failed_count = 0;
    let mut fail_msgs = Vec::new();

    for file in std::fs::read_dir("spec").unwrap() {
        let name = file.unwrap().file_name().into_string().unwrap();
        if name.ends_with(".json") {
            let mut line = 0;
            let spec = Spec::from_json(
                &serde_json::from_slice::<Value>(&std::fs::read(format!("spec/{}", name)).unwrap())
                    .unwrap(),
            );
            let line_ref = AssertUnwindSafe(&mut line);
            let spec_ref = &spec;
            match catch_unwind(move || {
                spec_ref.run(line_ref.0);
            }) {
                Ok(_) => {
                    passed_count += 1;
                    println!("[passed]{}", spec.source_filename);
                }
                Err(e) => {
                    failed_count += 1;
                    fail_msgs.push(format!("[{}:{}]\n{:?}", spec.source_filename, line, e));
                    println!("[failed]{}:{}", spec.source_filename, line);
                }
            }
        }
    }

    for msg in fail_msgs {
        println!("{}", msg);
    }

    println!("passed:{} failed:{}", passed_count, failed_count);
    if failed_count != 0 {
        panic!("test failed");
    }
}
