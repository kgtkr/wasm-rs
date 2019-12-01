use crate::binary::Decoder;
use crate::exec::instance::{ModuleInst, Val};
use crate::structure::modules::Module;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num::BigInt;
use num_traits::cast::FromPrimitive;
use num_traits::cast::ToPrimitive;
use serde_json::Value;
use std::io::Cursor;
use std::str::FromStr;

#[derive(Debug)]
pub enum Cmd {
    Module(String),
    AssertReturn {
        name: String,
        args: Vec<Val>,
        expected: Vec<Val>,
    },
    Skip(String),
}

impl Cmd {
    pub fn to_cmds(json: &Value) -> Vec<Cmd> {
        json.as_object()
            .unwrap()
            .get("commands")
            .unwrap()
            .as_array()
            .unwrap()
            .iter()
            .map(Cmd::from_json)
            .collect::<Vec<_>>()
    }

    fn big_to_i32(big: BigInt) -> i32 {
        (big.clone() & BigInt::from_i32(i32::max_value()).unwrap())
            .to_i32()
            .unwrap()
    }

    fn big_to_i64(big: BigInt) -> i64 {
        (big.clone() & BigInt::from_i64(i64::max_value()).unwrap())
            .to_i64()
            .unwrap()
    }

    fn json_to_value(json: &Value) -> Val {
        let json_obj = json.as_object().unwrap();
        let big = BigInt::from_str(json_obj.get("value").unwrap().as_str().unwrap()).unwrap();
        match json_obj.get("type").unwrap().as_str().unwrap() {
            "i32" => Val::I32(Cmd::big_to_i32(big)),
            "i64" => Val::I64(Cmd::big_to_i64(big)),
            "f32" => {
                let mut buf = Vec::new();
                buf.write_i32::<LittleEndian>(Cmd::big_to_i32(big)).unwrap();
                let mut rdr = Cursor::new(buf);
                Val::F32(rdr.read_f32::<LittleEndian>().unwrap())
            }
            "f64" => {
                let mut buf = Vec::new();
                buf.write_i64::<LittleEndian>(Cmd::big_to_i64(big)).unwrap();
                let mut rdr = Cursor::new(buf);
                Val::F64(rdr.read_f64::<LittleEndian>().unwrap())
            }
            _ => panic!(),
        }
    }

    pub fn from_json(json: &Value) -> Cmd {
        let json_obj = json.as_object().unwrap();
        match json_obj.get("type").unwrap().as_str().unwrap() {
            "module" => Cmd::Module(
                json_obj
                    .get("filename")
                    .unwrap()
                    .as_str()
                    .unwrap()
                    .to_string(),
            ),
            "assert_return" => {
                let action = json_obj.get("action").unwrap().as_object().unwrap();
                assert_eq!(action.get("type").unwrap().as_str().unwrap(), "invoke");
                Cmd::AssertReturn {
                    name: action.get("field").unwrap().as_str().unwrap().to_string(),
                    args: action
                        .get("args")
                        .unwrap()
                        .as_array()
                        .unwrap()
                        .iter()
                        .map(Cmd::json_to_value)
                        .collect::<Vec<_>>(),
                    expected: json_obj
                        .get("expected")
                        .unwrap()
                        .as_array()
                        .unwrap()
                        .iter()
                        .map(Cmd::json_to_value)
                        .collect::<Vec<_>>(),
                }
            }
            ty => Cmd::Skip(ty.to_string()),
        }
    }
}

pub fn run_assert(instance: &ModuleInst, name: &String, args: &Vec<Val>, expected: &Vec<Val>) {
    assert_eq!(
        &instance
            .export(name)
            .unwrap_func()
            .call(args.clone())
            .into_iter()
            .collect::<Vec<_>>(),
        expected
    );
}

pub fn run_test(filename: String) {
    let cmds = Cmd::to_cmds(
        &serde_json::from_slice::<Value>(&std::fs::read(format!("spec/{}", filename)).unwrap())
            .unwrap(),
    );

    let mut instance = None;

    for cmd in &cmds {
        println!("[begin]{:?}", cmd);
        match cmd {
            Cmd::Module(name) => {
                instance = Some(ModuleInst::new(
                    &Module::decode_end(&std::fs::read(format!("spec/{}", name)).unwrap()).unwrap(),
                    std::collections::HashMap::new(),
                ));
                println!("[[[success module]]]{}", name);
            }
            Cmd::AssertReturn {
                name,
                args,
                expected,
            } => {
                run_assert(instance.as_ref().unwrap(), name, args, expected);
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
                    println!("=========[error]{}{:?}", name, e);
                }
            }
        }
    }
}
