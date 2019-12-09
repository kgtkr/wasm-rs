#![recursion_limit = "1024"]
#![type_length_limit = "2097152"]

pub mod binary;
pub mod exec;
pub mod structure;
pub mod validation;

#[cfg(test)]
#[macro_use]
extern crate proptest_derive;

#[cfg(test)]
mod spec;
