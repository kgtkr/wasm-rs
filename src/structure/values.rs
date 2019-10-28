#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct Byte(pub u8);

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct Name(pub String);
