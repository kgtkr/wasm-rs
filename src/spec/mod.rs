use crate::binary::Decoder;
use crate::exec::instance::{ModuleInst, Val};
use crate::structure::modules::Module;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use serde_json::Value;
use std::io::Cursor;

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

#[derive(Debug, Clone, PartialEq)]
pub struct Spec {
    commands: Vec<Command>,
}

impl FromJSON for Spec {
    fn from_json(json: &Value) -> Spec {
        Spec {
            commands: json
                .as_object()
                .unwrap()
                .get("commands")
                .unwrap()
                .as_array()
                .unwrap()
                .iter()
                .map(Command::from_json)
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Command {
    paylaod: CommandPayload,
    name: Option<String>,
}

impl FromJSON for Command {
    fn from_json(json: &Value) -> Command {
        Command {
            paylaod: CommandPayload::from_json(json),
            name: json
                .as_object()
                .unwrap()
                .get("name")
                .map(|x| x.as_str().unwrap().to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CommandPayload {
    Module { filename: String },
    AssertReturn { action: Action, expected: Vec<Val> },
    Register { as_: String },
    AssertTrap { action: Action },
    Skip { type_: String },
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
            },
            "assert_return" => CommandPayload::AssertReturn {
                action: Action::from_json(json_obj.get("action").unwrap()),
                expected: json_obj
                    .get("expected")
                    .unwrap()
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(Val::from_json)
                    .collect::<Vec<_>>(),
            },
            "register" => CommandPayload::Register {
                as_: json_obj.get("as").unwrap().as_str().unwrap().to_string(),
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
            _ => panic!(),
        }
    }
}

pub fn run_assert(instance: &ModuleInst, action: &Action, expected: &Vec<Val>) {
    match &action.payload {
        ActionPayload::Invoke { field, args } => {
            assert_eq!(
                &instance
                    .export(field)
                    .unwrap_func()
                    .call(args.clone())
                    .unwrap()
                    .into_iter()
                    .collect::<Vec<_>>(),
                expected
            );
        }
        _ => panic!(),
    }
}

pub fn run_test(filename: String) {
    let spec = Spec::from_json(
        &serde_json::from_slice::<Value>(&std::fs::read(format!("spec/{}", filename)).unwrap())
            .unwrap(),
    );

    let mut instance = None;

    for cmd in &spec.commands {
        println!("[begin]{:?}", cmd);
        match &cmd.paylaod {
            CommandPayload::Module { filename: name } => {
                instance = Some(ModuleInst::new(
                    &Module::decode_end(&std::fs::read(format!("spec/{}", name)).unwrap()).unwrap(),
                    std::collections::HashMap::new(),
                ));
                println!("[[[success module]]]{}", name);
            }
            CommandPayload::AssertReturn { action, expected } => {
                run_assert(instance.as_ref().unwrap(), &action, &expected);
                println!("[success test]{}", filename);
            }
            _ => {}
        }
    }
}

#[test]
fn spec_test() {
    for file in std::fs::read_dir("spec").unwrap() {
        let name = file.unwrap().file_name().into_string().unwrap();
        if name.ends_with(".json") {
            println!("========[begin]{}", name);
            match std::panic::catch_unwind(|| {
                run_test(name.to_string());
            }) {
                Ok(_) => {
                    println!("========[success]{}", name);
                }
                Err(e) => {
                    println!("=========[error]{} {:?}", name, e);
                }
            }
        }
    }
}
