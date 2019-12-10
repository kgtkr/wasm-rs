#[derive(Debug, Clone, PartialEq)]
pub enum WasmError {
    RuntimeError,
    LinkError,
    CompileError,
}
