use super::parser;
use super::Decoder;
use super::Encoder;
use crate::structure::instructions::Expr;
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, Global, GlobalIdx, Import, ImportDesc, LabelIdx,
    LocalIdx, Mem, MemIdx, Module, Start, Table, TableIdx, TypeIdx,
};
use crate::structure::types::{
    ElemType, FuncType, GlobalType, Limits, MemType, Mut, ResultType, TableType, ValType,
};
use crate::structure::values::{Byte, Name};

use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while_m_n},
    combinator::{flat_map, map, map_res},
    eof,
    multi::{many0, many_m_n},
    sequence::tuple,
    IResult,
};

trait Zero {
    type NonZero;
    type Zero;

    fn is_zero(&self) -> bool {
        self.get_non_zero().is_none()
    }

    fn get_non_zero(&self) -> Option<&Self::NonZero>;

    fn zero() -> Self::Zero;
}

impl<T> Zero for Option<T> {
    type NonZero = T;
    type Zero = Option<T>;

    fn get_non_zero(&self) -> Option<&Self::NonZero> {
        self.as_ref()
    }

    fn zero() -> Self::Zero {
        None
    }
}

impl<T> Zero for Vec<T> {
    type NonZero = Vec<T>;
    type Zero = Vec<T>;

    fn get_non_zero(&self) -> Option<&Self::NonZero> {
        if self.is_empty() {
            None
        } else {
            Some(self)
        }
    }

    fn zero() -> Self::Zero {
        Vec::new()
    }
}

impl<'a, T> Zero for &'a T
where
    T: Zero,
{
    type NonZero = T::NonZero;
    type Zero = T::Zero;

    fn get_non_zero(&self) -> Option<&Self::NonZero> {
        (*self).get_non_zero()
    }

    fn zero() -> Self::Zero {
        T::zero()
    }
}

impl Encoder for TypeIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Decoder for TypeIdx {
    fn decode(input: &[u8]) -> IResult<&[u8], TypeIdx> {
        map(u32::decode, TypeIdx)(input)
    }
}

impl Encoder for FuncIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Decoder for FuncIdx {
    fn decode(input: &[u8]) -> IResult<&[u8], FuncIdx> {
        map(u32::decode, FuncIdx)(input)
    }
}

impl Encoder for GlobalIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Decoder for GlobalIdx {
    fn decode(input: &[u8]) -> IResult<&[u8], GlobalIdx> {
        map(u32::decode, GlobalIdx)(input)
    }
}

impl Encoder for LocalIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Decoder for LocalIdx {
    fn decode(input: &[u8]) -> IResult<&[u8], LocalIdx> {
        map(u32::decode, LocalIdx)(input)
    }
}

impl Encoder for LabelIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Decoder for LabelIdx {
    fn decode(input: &[u8]) -> IResult<&[u8], LabelIdx> {
        map(u32::decode, LabelIdx)(input)
    }
}

impl Encoder for MemIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Decoder for MemIdx {
    fn decode(input: &[u8]) -> IResult<&[u8], MemIdx> {
        map(u32::decode, MemIdx)(input)
    }
}

impl Encoder for TableIdx {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.0.encode(bytes);
    }
}

impl Decoder for TableIdx {
    fn decode(input: &[u8]) -> IResult<&[u8], TableIdx> {
        map(u32::decode, TableIdx)(input)
    }
}

impl Encoder for Import {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.module.encode(bytes);
        self.name.encode(bytes);
        self.desc.encode(bytes);
    }
}

impl Decoder for Import {
    fn decode(input: &[u8]) -> IResult<&[u8], Import> {
        map(
            tuple((Name::decode, Name::decode, ImportDesc::decode)),
            |(module, name, desc)| Import { module, name, desc },
        )(input)
    }
}

impl Encoder for ImportDesc {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            ImportDesc::Func(x) => {
                bytes.push(0x00);
                x.encode(bytes);
            }
            ImportDesc::Table(tt) => {
                bytes.push(0x01);
                tt.encode(bytes);
            }
            ImportDesc::Mem(mt) => {
                bytes.push(0x02);
                mt.encode(bytes);
            }
            ImportDesc::Global(gt) => {
                bytes.push(0x03);
                gt.encode(bytes);
            }
        }
    }
}

impl Decoder for ImportDesc {
    fn decode(input: &[u8]) -> IResult<&[u8], ImportDesc> {
        alt((
            map(tuple((parser::token(0x00), TypeIdx::decode)), |(_, x)| {
                ImportDesc::Func(x)
            }),
            map(tuple((parser::token(0x01), TableType::decode)), |(_, x)| {
                ImportDesc::Table(x)
            }),
            map(tuple((parser::token(0x02), MemType::decode)), |(_, x)| {
                ImportDesc::Mem(x)
            }),
            map(
                tuple((parser::token(0x03), GlobalType::decode)),
                |(_, x)| ImportDesc::Global(x),
            ),
        ))(input)
    }
}
impl Encoder for Table {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.type_.encode(bytes);
    }
}

impl Decoder for Table {
    fn decode(input: &[u8]) -> IResult<&[u8], Table> {
        map(TableType::decode, |type_| Table { type_ })(input)
    }
}

impl Encoder for Mem {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.type_.encode(bytes);
    }
}

impl Decoder for Mem {
    fn decode(input: &[u8]) -> IResult<&[u8], Mem> {
        map(MemType::decode, |type_| Mem { type_ })(input)
    }
}

impl Encoder for Global {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.type_.encode(bytes);
        self.init.encode(bytes);
    }
}

impl Decoder for Global {
    fn decode(input: &[u8]) -> IResult<&[u8], Global> {
        map(
            tuple((GlobalType::decode, Expr::decode)),
            |(type_, init)| Global { type_, init },
        )(input)
    }
}

impl Encoder for Export {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.name.encode(bytes);
        self.desc.encode(bytes);
    }
}

impl Decoder for Export {
    fn decode(input: &[u8]) -> IResult<&[u8], Export> {
        map(tuple((Name::decode, ExportDesc::decode)), |(name, desc)| {
            Export { name, desc }
        })(input)
    }
}

impl Encoder for ExportDesc {
    fn encode(&self, bytes: &mut Vec<u8>) {
        match self {
            ExportDesc::Func(x) => {
                bytes.push(0x00);
                x.encode(bytes);
            }
            ExportDesc::Table(tt) => {
                bytes.push(0x01);
                tt.encode(bytes);
            }
            ExportDesc::Mem(mt) => {
                bytes.push(0x02);
                mt.encode(bytes);
            }
            ExportDesc::Global(gt) => {
                bytes.push(0x03);
                gt.encode(bytes);
            }
        }
    }
}

impl Decoder for ExportDesc {
    fn decode(input: &[u8]) -> IResult<&[u8], ExportDesc> {
        alt((
            map(tuple((parser::token(0x00), FuncIdx::decode)), |(_, x)| {
                ExportDesc::Func(x)
            }),
            map(tuple((parser::token(0x01), TableIdx::decode)), |(_, x)| {
                ExportDesc::Table(x)
            }),
            map(tuple((parser::token(0x02), MemIdx::decode)), |(_, x)| {
                ExportDesc::Mem(x)
            }),
            map(tuple((parser::token(0x03), GlobalIdx::decode)), |(_, x)| {
                ExportDesc::Global(x)
            }),
        ))(input)
    }
}

impl Encoder for Start {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.func.encode(bytes)
    }
}

impl Decoder for Start {
    fn decode(input: &[u8]) -> IResult<&[u8], Start> {
        map(FuncIdx::decode, |func| Start { func })(input)
    }
}

impl Encoder for Elem {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.table.encode(bytes);
        self.offset.encode(bytes);
        self.init.encode(bytes);
    }
}

impl Decoder for Elem {
    fn decode(input: &[u8]) -> IResult<&[u8], Elem> {
        map(
            tuple((TableIdx::decode, Expr::decode, Vec::<FuncIdx>::decode)),
            |(table, offset, init)| Elem {
                table,
                offset,
                init,
            },
        )(input)
    }
}

impl Encoder for Data {
    fn encode(&self, bytes: &mut Vec<u8>) {
        self.data.encode(bytes);
        self.offset.encode(bytes);
        self.init.encode(bytes);
    }
}

impl Decoder for Data {
    fn decode(input: &[u8]) -> IResult<&[u8], Data> {
        map(
            tuple((MemIdx::decode, Expr::decode, Vec::<Byte>::decode)),
            |(data, offset, init)| Data { data, offset, init },
        )(input)
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

pub fn p_code(input: &[u8]) -> IResult<&[u8], (Vec<ValType>, Expr)> {
    let (input, size) = u32::decode(input)?;
    let size = size as usize;
    let (code_input, input) = input.split_at(size);
    let (_, result) = map(
        tuple((Vec::<(u32, ValType)>::decode, Expr::decode, parser::eof())),
        |(locals, body, _)| {
            let locals = locals
                .into_iter()
                .map(|(l, t)| {
                    let l = l as usize;
                    let mut result = Vec::with_capacity(l);
                    result.resize(l, t);
                    result
                })
                .collect::<Vec<_>>()
                .concat();
            (locals, body)
        },
    )(code_input)?;
    Ok((input, result))
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

fn p_section<'a, R>(
    id: Byte,
    p: impl Fn(&'a [u8]) -> IResult<&'a [u8], R>,
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Option<R>> {
    move |input| match Byte::decode(input) {
        Ok((_, id_)) if id_ != id => Ok((input, None)),
        Err(_) => Ok((input, None)),
        Ok((input, _id)) => {
            let (input, size) = u32::decode(input)?;
            let size = size as usize;
            let (body_input, input) = input.split_at(size);
            let (_, result) = map(tuple((&p, parser::eof())), |(x, _)| x)(body_input)?;
            Ok((input, Some(result)))
        }
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
        encode_section(Byte(6), &self.globals, bytes);
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

fn p_costoms(input: &[u8]) -> IResult<&[u8], ()> {
    map(
        many0(flat_map(
            tuple((parser::token(0x00), u32::decode)),
            |(_, size)| many_m_n(size as usize, size as usize, Byte::decode),
        )),
        |_| (),
    )(input)
}

impl Decoder for Module {
    fn decode(input: &[u8]) -> IResult<&[u8], Module> {
        map(
            tuple((
                tuple((
                    parser::token(0x00),
                    parser::token(0x61),
                    parser::token(0x73),
                    parser::token(0x6d),
                    parser::token(0x01),
                    parser::token(0x00),
                    parser::token(0x00),
                    parser::token(0x00),
                )),
                tuple((p_costoms, p_section(Byte(1), Vec::<FuncType>::decode))),
                tuple((p_costoms, p_section(Byte(2), Vec::<Import>::decode))),
                tuple((p_costoms, p_section(Byte(3), Vec::<TypeIdx>::decode))),
                tuple((p_costoms, p_section(Byte(4), Vec::<Table>::decode))),
                tuple((p_costoms, p_section(Byte(5), Vec::<Mem>::decode))),
                tuple((p_costoms, p_section(Byte(6), Vec::<Global>::decode))),
                tuple((p_costoms, p_section(Byte(7), Vec::<Export>::decode))),
                tuple((p_costoms, p_section(Byte(8), Start::decode))),
                tuple((p_costoms, p_section(Byte(9), Vec::<Elem>::decode))),
                tuple((p_costoms, p_section(Byte(10), super::values::p_vec(p_code)))),
                tuple((p_costoms, p_section(Byte(11), Vec::<Data>::decode))),
                p_costoms,
            )),
            |(
                _,
                (_, types),
                (_, imports),
                (_, funcs),
                (_, tables),
                (_, mems),
                (_, globals),
                (_, exports),
                (_, start),
                (_, elem),
                (_, code),
                (_, data),
                _,
            )| Module {
                types: types.unwrap_or_else(Vec::new),
                funcs: funcs
                    .unwrap_or_else(Vec::new)
                    .into_iter()
                    .zip(code.unwrap_or_else(Vec::new))
                    .map(|(type_, (locals, body))| Func {
                        type_,
                        locals,
                        body,
                    })
                    .collect::<Vec<_>>(),
                imports: imports.unwrap_or_else(Vec::new),
                tables: tables.unwrap_or_else(Vec::new),
                mems: mems.unwrap_or_else(Vec::new),
                globals: globals.unwrap_or_else(Vec::new),
                exports: exports.unwrap_or_else(Vec::new),
                start,
                elem: elem.unwrap_or_else(Vec::new),
                data: data.unwrap_or_else(Vec::new),
            },
        )(input)
    }
}

#[test]
fn test_add() {
    use crate::structure::instructions::Instr;
    assert_eq!(
        Module::decode_end(&std::fs::read("./example/add.wasm").unwrap()).unwrap(),
        Module {
            types: vec![FuncType(
                vec![ValType::I32, ValType::I32],
                vec![ValType::I32],
            )],
            funcs: vec![Func {
                type_: TypeIdx(0),
                locals: vec![],
                body: Expr(vec![
                    Instr::LocalGet(LocalIdx(0)),
                    Instr::LocalGet(LocalIdx(1)),
                    Instr::I32Add,
                ]),
            }],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            elem: vec![],
            data: vec![],
            start: None,
            imports: vec![],
            exports: vec![Export {
                name: Name("add".to_string()),
                desc: ExportDesc::Func(FuncIdx(0)),
            }],
        }
    );
}

#[test]
fn test_md5() {
    assert!(Module::decode_end(&std::fs::read("./example/md5.wasm").unwrap()).is_ok(),);
}

#[test]
fn tests() {
    use super::test_helper;

    // test_helper::identity_encode_decode::<Module>();
    test_helper::identity_encode_decode::<TypeIdx>();
    test_helper::identity_encode_decode::<FuncIdx>();
    test_helper::identity_encode_decode::<TableIdx>();
    test_helper::identity_encode_decode::<MemIdx>();
    test_helper::identity_encode_decode::<GlobalIdx>();
    test_helper::identity_encode_decode::<LocalIdx>();
    test_helper::identity_encode_decode::<LabelIdx>();
    // test_helper::identity_encode_decode::<Func>();
    test_helper::identity_encode_decode::<Table>();
    test_helper::identity_encode_decode::<Mem>();
    // test_helper::identity_encode_decode::<Elem>();
    // test_helper::identity_encode_decode::<Data>();
    test_helper::identity_encode_decode::<Start>();
    test_helper::identity_encode_decode::<Export>();
    test_helper::identity_encode_decode::<ExportDesc>();
    test_helper::identity_encode_decode::<Import>();
    test_helper::identity_encode_decode::<ImportDesc>();
}
