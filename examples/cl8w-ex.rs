use wasm_rs::binary::decode_module;
use wasm_rs::exec::{ModuleInst, ExternalVal, FuncAddr, MemAddr};
use maplit::hashmap;

fn main(){
    let memory = ExternalVal::Mem(MemAddr::new(10, None));
    let print = ExternalVal::Func(FuncAddr::alloc_host(|(x,): (i32,)| {
        println!("{}", x);
        Ok(())
    }));

    let memory_module =
        decode_module(&std::fs::read("cl8w-wasm/memory.wasm").unwrap()).unwrap();
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
        decode_module(&std::fs::read("cl8w-wasm/cl8w-ex.wasm").unwrap()).unwrap();
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