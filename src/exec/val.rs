use crate::structure::types::ValType;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use std::io::Cursor;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Val {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Val {
    pub fn val_type(&self) -> ValType {
        match self {
            Val::I32(_) => ValType::I32,
            Val::I64(_) => ValType::I64,
            Val::F32(_) => ValType::F32,
            Val::F64(_) => ValType::F64,
        }
    }

    pub fn unwrap_i32(&self) -> i32 {
        if let Val::I32(x) = self {
            *x
        } else {
            panic!();
        }
    }

    pub fn unwrap_i64(&self) -> i64 {
        if let Val::I64(x) = self {
            *x
        } else {
            panic!();
        }
    }

    pub fn unwrap_f32(&self) -> f32 {
        if let Val::F32(x) = self {
            *x
        } else {
            panic!();
        }
    }

    pub fn unwrap_f64(&self) -> f64 {
        if let Val::F64(x) = self {
            *x
        } else {
            panic!();
        }
    }
}

pub trait PrimitiveVal: Sized {
    fn try_from_val(val: Val) -> Option<Self>;
    fn wrap_val(self) -> Val;
    fn type_() -> ValType;
}

impl PrimitiveVal for i32 {
    fn try_from_val(value: Val) -> Option<i32> {
        if let Val::I32(x) = value {
            Some(x)
        } else {
            None
        }
    }

    fn wrap_val(self) -> Val {
        Val::I32(self)
    }

    fn type_() -> ValType {
        ValType::I32
    }
}

impl PrimitiveVal for i64 {
    fn try_from_val(value: Val) -> Option<i64> {
        if let Val::I64(x) = value {
            Some(x)
        } else {
            None
        }
    }

    fn wrap_val(self) -> Val {
        Val::I64(self)
    }

    fn type_() -> ValType {
        ValType::I64
    }
}

impl PrimitiveVal for f32 {
    fn try_from_val(value: Val) -> Option<f32> {
        if let Val::F32(x) = value {
            Some(x)
        } else {
            None
        }
    }

    fn wrap_val(self) -> Val {
        Val::F32(self)
    }

    fn type_() -> ValType {
        ValType::F32
    }
}

impl PrimitiveVal for f64 {
    fn try_from_val(value: Val) -> Option<f64> {
        if let Val::F64(x) = value {
            Some(x)
        } else {
            None
        }
    }

    fn wrap_val(self) -> Val {
        Val::F64(self)
    }

    fn type_() -> ValType {
        ValType::F64
    }
}

pub trait InterpretPrimitive {
    type Primitive: PrimitiveVal;

    fn to_primitive(self) -> Self::Primitive;
    fn reinterpret(primitive: Self::Primitive) -> Self;
}

impl<T: PrimitiveVal> InterpretPrimitive for T {
    type Primitive = T;

    fn to_primitive(self) -> T {
        self
    }
    fn reinterpret(primitive: T) -> T {
        primitive
    }
}

impl InterpretPrimitive for bool {
    type Primitive = i32;

    fn to_primitive(self) -> i32 {
        if self {
            1
        } else {
            0
        }
    }

    fn reinterpret(primitive: i32) -> bool {
        primitive != 0
    }
}

impl InterpretPrimitive for u32 {
    type Primitive = i32;

    fn to_primitive(self) -> i32 {
        let mut wtr = vec![];
        wtr.write_u32::<LittleEndian>(self).unwrap();
        let mut rdr = Cursor::new(wtr);
        rdr.read_i32::<LittleEndian>().unwrap()
    }

    fn reinterpret(primitive: i32) -> u32 {
        let mut wtr = vec![];
        wtr.write_i32::<LittleEndian>(primitive).unwrap();
        let mut rdr = Cursor::new(wtr);
        rdr.read_u32::<LittleEndian>().unwrap()
    }
}

impl InterpretPrimitive for u64 {
    type Primitive = i64;

    fn to_primitive(self) -> i64 {
        let mut wtr = vec![];
        wtr.write_u64::<LittleEndian>(self).unwrap();
        let mut rdr = Cursor::new(wtr);
        rdr.read_i64::<LittleEndian>().unwrap()
    }

    fn reinterpret(primitive: i64) -> u64 {
        let mut wtr = vec![];
        wtr.write_i64::<LittleEndian>(primitive).unwrap();
        let mut rdr = Cursor::new(wtr);
        rdr.read_u64::<LittleEndian>().unwrap()
    }
}

pub trait InterpretVal: Sized {
    fn try_interpret_val(val: Val) -> Option<Self>;
    fn to_val(self) -> Val;
}

impl<T: InterpretPrimitive> InterpretVal for T {
    fn try_interpret_val(val: Val) -> Option<T> {
        Some(T::reinterpret(T::Primitive::try_from_val(val)?))
    }
    fn to_val(self) -> Val {
        self.to_primitive().wrap_val()
    }
}

impl InterpretVal for Val {
    fn try_interpret_val(val: Val) -> Option<Val> {
        Some(val)
    }
    fn to_val(self) -> Val {
        self
    }
}
