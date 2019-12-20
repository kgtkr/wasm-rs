mod decoder;
mod encoder;
mod instructions;
mod modules;
mod parser;
mod types;
mod util;
mod values;

use decoder::Decoder;
use encoder::Encoder;
#[cfg(test)]
mod test_helper;

use super::structure::modules::Module;

pub fn encode_module(module: &Module) -> Vec<u8> {
    module.encode_to_vec()
}

pub fn decode_module(bytes: &[u8]) -> Result<Module, nom::Err<(&[u8], nom::error::ErrorKind)>> {
    Module::decode_to_end(bytes)
}
