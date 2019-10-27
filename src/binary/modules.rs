use super::values::{p_u32, p_vec};
use super::Encoder;
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, Global, GlobalIdx, Import, ImportDesc, LabelIdx,
    LocalIdx, Mem, MemIdx, Module, Start, Table, TableIdx, TypeIdx,
};
use crate::structure::values::Byte;
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{flat_map, map, map_res},
    multi::many_m_n,
    sequence::tuple,
    IResult,
};

trait Zero {
    type NonZero;

    fn is_zero(&self) -> bool {
        self.get_non_zero().is_none()
    }

    fn get_non_zero(&self) -> Option<&Self::NonZero>;
}

impl<T> Zero for Option<T> {
    type NonZero = T;

    fn get_non_zero(&self) -> Option<&Self::NonZero> {
        self.as_ref()
    }
}

impl<T> Zero for Vec<T> {
    type NonZero = Vec<T>;

    fn get_non_zero(&self) -> Option<&Self::NonZero> {
        if self.is_empty() {
            None
        } else {
            Some(self)
        }
    }
}

impl<'a, T> Zero for &'a T
where
    T: Zero,
{
    type NonZero = T::NonZero;

    fn get_non_zero(&self) -> Option<&Self::NonZero> {
        (*self).get_non_zero()
    }
}

impl Encoder for TypeIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

pub fn p_typeidx(input: &[u8]) -> IResult<&[u8], TypeIdx> {
    map(p_u32, TypeIdx)(input)
}

impl Encoder for FuncIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

pub fn p_funcidx(input: &[u8]) -> IResult<&[u8], FuncIdx> {
    map(p_u32, FuncIdx)(input)
}

impl Encoder for GlobalIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

pub fn p_globalidx(input: &[u8]) -> IResult<&[u8], GlobalIdx> {
    map(p_u32, GlobalIdx)(input)
}

impl Encoder for LocalIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

pub fn p_localidx(input: &[u8]) -> IResult<&[u8], LocalIdx> {
    map(p_u32, LocalIdx)(input)
}

impl Encoder for LabelIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

pub fn p_labelidx(input: &[u8]) -> IResult<&[u8], LabelIdx> {
    map(p_u32, LabelIdx)(input)
}

impl Encoder for MemIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

pub fn p_memidx(input: &[u8]) -> IResult<&[u8], MemIdx> {
    map(p_u32, MemIdx)(input)
}

impl Encoder for TableIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

pub fn p_tableidx(input: &[u8]) -> IResult<&[u8], TableIdx> {
    map(p_u32, TableIdx)(input)
}

impl Encoder for Import {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.module.encode(bytes);
        self.name.encode(bytes);
        self.desc.encode(bytes);
    }
}

impl Encoder for ImportDesc {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            ImportDesc::Func(x) => x.encode(bytes),
            ImportDesc::Table(tt) => tt.encode(bytes),
            ImportDesc::Mem(mt) => mt.encode(bytes),
            ImportDesc::Global(gt) => gt.encode(bytes),
        }
    }
}

impl Encoder for Table {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.type_.encode(bytes);
    }
}

impl Encoder for Mem {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.type_.encode(bytes);
    }
}

impl Encoder for Global {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.type_.encode(bytes);
        self.init.encode(bytes);
    }
}

impl Encoder for Export {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.name.encode(bytes);
        self.desc.encode(bytes);
    }
}

impl Encoder for ExportDesc {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            ExportDesc::Func(x) => x.encode(bytes),
            ExportDesc::Global(gt) => gt.encode(bytes),
            ExportDesc::Mem(mt) => mt.encode(bytes),
            ExportDesc::Table(tt) => tt.encode(bytes),
        }
    }
}

impl Encoder for Start {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.func.encode(bytes)
    }
}

impl Encoder for Elem {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.table.encode(bytes);
        self.offset.encode(bytes);
        self.init.encode(bytes);
    }
}

impl Encoder for Data {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.data.encode(bytes);
        self.offset.encode(bytes);
        self.init.encode(bytes);
    }
}

fn group_by_count<T: PartialEq>(xs: &Vec<T>) -> Vec<(u32, &T)> {
    xs.iter().fold(Vec::new(), |mut prev, x| {
        match prev.last_mut() {
            Some((prev_c, prev_x)) if &x == prev_x => {
                (*prev_c) += 1;
            }
            _ => {
                prev.push((1, x));
            }
        }
        prev
    })
}

#[test]
fn test_group_by_count() {
    assert_eq!(
        group_by_count(&Vec::<i32>::new()),
        Vec::<(u32, &i32)>::new()
    );

    assert_eq!(group_by_count(&vec![2]), vec![(1, &2)]);
    assert_eq!(
        group_by_count(&vec![2, 3, 4, 4, 4, 3, 2, 2]),
        vec![(1, &2), (1, &3), (3, &4), (1, &3), (2, &2)]
    );
}

struct Code<'a>(&'a Func);

impl<'a> Encoder for Code<'a> {
    fn encode(&self, bytes: &mut Vec<u8>) {
        let body = (group_by_count(&self.0.locals), &self.0.body).encode_to_vec();
        (body.len() as u32).encode(bytes);
        bytes.extend_from_slice(&body);
    }
}

fn encode_section<T: Zero>(id: Byte, cont: T, bytes: &mut Vec<u8>)
where
    T::NonZero: Encoder,
{
    if let Some(x) = cont.get_non_zero() {
        id.encode(bytes);
        let body = x.encode_to_vec();
        (body.len() as u32).encode(bytes);
        bytes.extend_from_slice(&body);
    }
}

impl Encoder for Module {
    fn encode(&self, bytes: &mut Vec<u8>) {
        bytes.push(0x00);
        bytes.push(0x61);
        bytes.push(0x73);
        bytes.push(0x6d);

        bytes.push(0x01);
        bytes.push(0x00);
        bytes.push(0x00);
        bytes.push(0x00);

        encode_section(Byte(1), &self.types, bytes);
        encode_section(Byte(2), &self.imports, bytes);
        encode_section(
            Byte(3),
            self.funcs.iter().map(|x| &x.type_).collect::<Vec<_>>(),
            bytes,
        );
        encode_section(Byte(4), &self.tables, bytes);
        encode_section(Byte(5), &self.mems, bytes);
        encode_section(Byte(6), &self.mems, bytes);
        encode_section(Byte(7), &self.exports, bytes);
        encode_section(Byte(8), &self.start, bytes);
        encode_section(Byte(9), &self.elem, bytes);
        encode_section(
            Byte(10),
            self.funcs.iter().map(|x| Code(x)).collect::<Vec<_>>(),
            bytes,
        );
        encode_section(Byte(11), &self.data, bytes);
    }
}
