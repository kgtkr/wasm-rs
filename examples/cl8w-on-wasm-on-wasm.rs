use maplit::hashmap;
use wasm_rs::binary::decode_module;
use wasm_rs::exec::{ExternalVal, FuncAddr, ModuleInst};
fn main() {
    let print = ExternalVal::Func(FuncAddr::alloc_host(|(x,): (i32,)| {
        println!("{}", x);
        Ok(())
    }));

    let module = decode_module(
        &std::fs::read(
            "cl8w-on-wasm-bin/target/wasm32-unknown-unknown/release/cl8w-on-wasm-bin.wasm",
        )
        .unwrap(),
    )
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
