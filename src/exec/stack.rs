use super::instance::{FuncAddr, FuncInst, ModuleInst, RuntimeError, TypedIdxAccess, Val};
use crate::structure::instructions::Instr;
use crate::structure::modules::{
    Data, Elem, Export, ExportDesc, Func, FuncIdx, Global, GlobalIdx, LabelIdx, LocalIdx, Mem,
    Module, Table, TypeIdx, TypedIdx,
};
use crate::structure::types::ValType;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};

use std::io::Cursor;
use std::rc::Weak;

#[derive(Debug, Clone)]
pub enum FrameLevelInstr {
    Label(Label, /* 前から */ Vec<Instr>),
    Br(LabelIdx),
    LabelEnd,
    Invoke(FuncAddr),
    Return,
}

#[derive(Debug, Clone)]
pub enum ModuleLevelInstr {
    Invoke(FuncAddr),
    Return,
}

#[derive(Debug, Clone)]
pub enum AdminInstr {
    Instr(Instr),
    Invoke(FuncAddr),
    Label(Label, Vec<Instr>),
    Br(LabelIdx),
    Return,
}

#[derive(Debug, Clone)]
pub struct Frame {
    pub module: Weak<ModuleInst>,
    pub locals: Vec<Val>,
    pub n: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    // 前から実行
    pub instrs: Vec<Instr>,
    pub n: usize,
}

#[derive(Debug, Clone)]
pub struct FrameStack {
    pub frame: Frame,
    // not empty
    // stack[0]の継続は空
    pub stack: Vec<LabelStack>,
}

impl FrameStack {
    pub fn step(&mut self) -> Result<Option<ModuleLevelInstr>, RuntimeError> {
        let cur_lavel = self.stack.last_mut().unwrap();
        Ok(if let Some(instr) = cur_lavel.step(&mut self.frame)? {
            match instr {
                FrameLevelInstr::Invoke(idx) => Some(ModuleLevelInstr::Invoke(idx)),
                FrameLevelInstr::Return => Some(ModuleLevelInstr::Return),
                FrameLevelInstr::Br(idx) => {
                    let mut add_stack = self.stack.last().unwrap().stack.clone();
                    let idx = idx.to_idx();
                    for _ in 0..idx {
                        self.stack.pop().unwrap();
                    }

                    let k_label = self.stack.pop().unwrap().label;
                    let mut k = k_label
                        .instrs
                        .clone()
                        .into_iter()
                        .map(AdminInstr::Instr)
                        .rev()
                        .collect::<Vec<_>>();

                    if let Some(last_label) = self.stack.last_mut() {
                        last_label.instrs.append(&mut k);
                        last_label
                            .stack
                            .append(&mut pop_n(&mut add_stack, k_label.n));
                        None
                    } else {
                        self.stack.push(LabelStack {
                            label: Label {
                                instrs: vec![],
                                n: 0, /* dummy */
                            },
                            instrs: vec![],
                            stack: add_stack,
                        });
                        Some(ModuleLevelInstr::Return)
                    }
                }
                FrameLevelInstr::Label(label, instrs) => {
                    self.stack.push(LabelStack {
                        label,
                        instrs: instrs.into_iter().map(AdminInstr::Instr).rev().collect(),
                        stack: vec![],
                    });
                    None
                }
                FrameLevelInstr::LabelEnd => {
                    let mut cur_lavel = self.stack.pop().unwrap();
                    if let Some(last_label) = self.stack.last_mut() {
                        last_label.stack.append(&mut cur_lavel.stack);
                        None
                    } else {
                        self.stack.push(cur_lavel);
                        Some(ModuleLevelInstr::Return)
                    }
                }
            }
        } else {
            None
        })
    }
}

#[derive(Debug, Clone)]
pub struct LabelStack {
    pub label: Label,
    // 後ろから実行
    pub instrs: Vec<AdminInstr>,
    pub stack: Vec<Val>,
}

impl LabelStack {
    fn step(&mut self, frame: &mut Frame) -> Result<Option<FrameLevelInstr>, RuntimeError> {
        Ok(match self.instrs.pop() {
            Some(instr) => match instr {
                AdminInstr::Instr(instr) => {
                    match instr {
                        Instr::I32Const(x) => {
                            self.stack.push(Val::I32(x));
                        }
                        Instr::I64Const(x) => {
                            self.stack.push(Val::I64(x));
                        }
                        Instr::F32Const(x) => {
                            self.stack.push(Val::F32(x));
                        }
                        Instr::F64Const(x) => {
                            self.stack.push(Val::F64(x));
                        }
                        Instr::I32Clz => {
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x.leading_zeros() as i32));
                        }
                        Instr::I32Ctz => {
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x.trailing_zeros() as i32));
                        }
                        Instr::I32Popcnt => {
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x.count_ones() as i32));
                        }
                        Instr::I64Clz => {
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x.leading_zeros() as i64));
                        }
                        Instr::I64Ctz => {
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x.trailing_zeros() as i64));
                        }
                        Instr::I64Popcnt => {
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x.count_ones() as i64));
                        }
                        Instr::F32Abs => {
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(x.abs()));
                        }
                        Instr::F32Neg => {
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(-x));
                        }
                        Instr::F32Sqrt => {
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(x.sqrt()));
                        }
                        Instr::F32Ceil => {
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(x.ceil()));
                        }
                        Instr::F32Floor => {
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(x.floor()));
                        }
                        Instr::F32Trunc => {
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(x.trunc()));
                        }
                        Instr::F32Nearest => {
                            unimplemented!();
                        }
                        Instr::F64Abs => {
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(x.abs()));
                        }
                        Instr::F64Neg => {
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(-x));
                        }
                        Instr::F64Sqrt => {
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(x.sqrt()));
                        }
                        Instr::F64Ceil => {
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(x.ceil()));
                        }
                        Instr::F64Floor => {
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(x.floor()));
                        }
                        Instr::F64Trunc => {
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(x.trunc()));
                        }
                        Instr::F64Nearest => {
                            unimplemented!();
                        }
                        Instr::I32Add => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x.overflowing_add(y).0));
                        }
                        Instr::I32Sub => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x.overflowing_sub(y).0));
                        }
                        Instr::I32Mul => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x.overflowing_mul(y).0));
                        }
                        Instr::I32DivS => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            if y == 0 {
                                return Err(RuntimeError::Trap);
                            }
                            self.stack.push(Val::I32(
                                x.checked_div(y).ok_or_else(|| RuntimeError::Trap)?,
                            ));
                        }
                        Instr::I32DivU => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();

                            let x = i32_convert_u32(x);
                            let y = i32_convert_u32(y);

                            if y == 0 {
                                return Err(RuntimeError::Trap);
                            }
                            self.stack.push(Val::I32(u32_convert_i32(
                                x.checked_div(y).ok_or_else(|| RuntimeError::Trap)?,
                            )));
                        }
                        Instr::I32RemS => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();

                            if y == 0 {
                                return Err(RuntimeError::Trap);
                            }

                            self.stack.push(Val::I32(x % y));
                        }
                        Instr::I32RemU => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();

                            let x = i32_convert_u32(x);
                            let y = i32_convert_u32(y);

                            if y == 0 {
                                return Err(RuntimeError::Trap);
                            }

                            self.stack.push(Val::I32(u32_convert_i32(x % y)));
                        }
                        Instr::I32And => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x & y));
                        }
                        Instr::I32Or => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x | y));
                        }
                        Instr::I32Xor => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x ^ y));
                        }
                        Instr::I32ShL => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x.overflowing_shl(y as u32).0));
                        }
                        Instr::I32ShrS => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x.overflowing_shr(y as u32).0));
                        }
                        Instr::I32ShrU => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(u32_convert_i32(
                                i32_convert_u32(x).overflowing_shr(i32_convert_u32(y)).0,
                            )));
                        }
                        Instr::I32Rotl => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x.rotate_left(y as u32)));
                        }
                        Instr::I32Rotr => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(x.rotate_right(y as u32)));
                        }
                        Instr::I64Add => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x.overflowing_add(y).0));
                        }
                        Instr::I64Sub => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x.overflowing_sub(y).0));
                        }
                        Instr::I64Mul => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x.overflowing_mul(y).0));
                        }
                        Instr::I64DivS => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            if y == 0 {
                                return Err(RuntimeError::Trap);
                            }
                            self.stack.push(Val::I64(
                                x.checked_div(y).ok_or_else(|| RuntimeError::Trap)?,
                            ));
                        }
                        Instr::I64DivU => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();

                            let x = i64_convert_u64(x);
                            let y = i64_convert_u64(y);
                            if y == 0 {
                                return Err(RuntimeError::Trap);
                            }
                            self.stack.push(Val::I64(u64_convert_i64(
                                x.checked_div(y).ok_or_else(|| RuntimeError::Trap)?,
                            )));
                        }
                        Instr::I64RemS => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();

                            if y == 0 {
                                return Err(RuntimeError::Trap);
                            }

                            self.stack.push(Val::I64(x % y));
                        }
                        Instr::I64RemU => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();

                            let x = i64_convert_u64(x);
                            let y = i64_convert_u64(y);

                            if y == 0 {
                                return Err(RuntimeError::Trap);
                            }

                            self.stack.push(Val::I64(u64_convert_i64(x % y)));
                        }
                        Instr::I64And => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x & y));
                        }
                        Instr::I64Or => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x | y));
                        }
                        Instr::I64Xor => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x ^ y));
                        }
                        Instr::I64ShL => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x.overflowing_shl(y as u32).0));
                        }
                        Instr::I64ShrS => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x.overflowing_shr(y as u32).0));
                        }
                        Instr::I64ShrU => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(u64_convert_i64(
                                i64_convert_u64(x)
                                    .overflowing_shr(i64_convert_u64(y) as u32)
                                    .0,
                            )));
                        }
                        Instr::I64Rotl => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x.rotate_left(y as u32)));
                        }
                        Instr::I64Rotr => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I64(x.rotate_right(y as u32)));
                        }
                        Instr::F32Add => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(x + y));
                        }
                        Instr::F32Sub => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(x - y));
                        }
                        Instr::F32Mul => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(x * y));
                        }
                        Instr::F32Div => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(x / y));
                        }
                        Instr::F32Min => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            // 0.0, -0.0対策
                            self.stack.push(Val::F32(match (sign_f32(x), sign_f32(y)) {
                                (Some(Sign::Positive), Some(Sign::Negative)) => y,
                                (Some(Sign::Negative), Some(Sign::Positive)) => x,
                                _ => x.min(y),
                            }));
                        }
                        Instr::F32Max => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(match (sign_f32(x), sign_f32(y)) {
                                (Some(Sign::Positive), Some(Sign::Negative)) => x,
                                (Some(Sign::Negative), Some(Sign::Positive)) => y,
                                _ => x.max(y),
                            }));
                        }
                        Instr::F32CopySign => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::F32(x.copysign(y)));
                        }
                        Instr::F64Add => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(x + y));
                        }
                        Instr::F64Sub => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(x - y));
                        }
                        Instr::F64Mul => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(x * y));
                        }
                        Instr::F64Div => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(x / y));
                        }
                        Instr::F64Min => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(match (sign_f64(x), sign_f64(y)) {
                                (Some(Sign::Positive), Some(Sign::Negative)) => y,
                                (Some(Sign::Negative), Some(Sign::Positive)) => x,
                                _ => x.min(y),
                            }));
                        }
                        Instr::F64Max => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(match (sign_f64(x), sign_f64(y)) {
                                (Some(Sign::Positive), Some(Sign::Negative)) => x,
                                (Some(Sign::Negative), Some(Sign::Positive)) => y,
                                _ => x.max(y),
                            }));
                        }
                        Instr::F64CopySign => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::F64(x.copysign(y)));
                        }
                        Instr::I32Eqz => {
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(if x == 0 { 1 } else { 0 }));
                        }
                        Instr::I64Eqz => {
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I32(if x == 0 { 1 } else { 0 }));
                        }
                        Instr::I32Eq => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(if x == y { 1 } else { 0 }));
                        }
                        Instr::I32Ne => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(if x != y { 1 } else { 0 }));
                        }
                        Instr::I32LtS => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(if x < y { 1 } else { 0 }));
                        }
                        Instr::I32LtU => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack
                                .push(Val::I32(if i32_convert_u32(x) < i32_convert_u32(y) {
                                    1
                                } else {
                                    0
                                }));
                        }
                        Instr::I32GtS => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(if x > y { 1 } else { 0 }));
                        }
                        Instr::I32GtU => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack
                                .push(Val::I32(if i32_convert_u32(x) > i32_convert_u32(y) {
                                    1
                                } else {
                                    0
                                }));
                        }
                        Instr::I32LeS => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(if x <= y { 1 } else { 0 }));
                        }
                        Instr::I32LeU => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(
                                if i32_convert_u32(x) <= i32_convert_u32(y) {
                                    1
                                } else {
                                    0
                                },
                            ));
                        }
                        Instr::I32GeS => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(if x >= y { 1 } else { 0 }));
                        }
                        Instr::I32GeU => {
                            let y = self.stack.pop().unwrap().unwrap_i32();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(
                                if i32_convert_u32(x) >= i32_convert_u32(y) {
                                    1
                                } else {
                                    0
                                },
                            ));
                        }
                        Instr::I64Eq => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I32(if x == y { 1 } else { 0 }));
                        }
                        Instr::I64Ne => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I32(if x != y { 1 } else { 0 }));
                        }
                        Instr::I64LtS => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I32(if x < y { 1 } else { 0 }));
                        }
                        Instr::I64LtU => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack
                                .push(Val::I32(if i64_convert_u64(x) < i64_convert_u64(y) {
                                    1
                                } else {
                                    0
                                }));
                        }
                        Instr::I64GtS => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I32(if x > y { 1 } else { 0 }));
                        }
                        Instr::I64GtU => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack
                                .push(Val::I32(if i64_convert_u64(x) > i64_convert_u64(y) {
                                    1
                                } else {
                                    0
                                }));
                        }
                        Instr::I64LeS => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I32(if x <= y { 1 } else { 0 }));
                        }
                        Instr::I64LeU => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I32(
                                if i64_convert_u64(x) <= i64_convert_u64(y) {
                                    1
                                } else {
                                    0
                                },
                            ));
                        }
                        Instr::I64GeS => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I32(if x >= y { 1 } else { 0 }));
                        }
                        Instr::I64GeU => {
                            let y = self.stack.pop().unwrap().unwrap_i64();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I32(
                                if i64_convert_u64(x) >= i64_convert_u64(y) {
                                    1
                                } else {
                                    0
                                },
                            ));
                        }
                        Instr::F32Eq => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::I32(if x == y { 1 } else { 0 }));
                        }
                        Instr::F32Ne => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::I32(if x != y { 1 } else { 0 }));
                        }
                        Instr::F32Lt => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::I32(if x < y { 1 } else { 0 }));
                        }
                        Instr::F32Gt => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::I32(if x > y { 1 } else { 0 }));
                        }
                        Instr::F32Le => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::I32(if x <= y { 1 } else { 0 }));
                        }
                        Instr::F32Ge => {
                            let y = self.stack.pop().unwrap().unwrap_f32();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            self.stack.push(Val::I32(if x >= y { 1 } else { 0 }));
                        }
                        Instr::F64Eq => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::I32(if x == y { 1 } else { 0 }));
                        }
                        Instr::F64Ne => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::I32(if x != y { 1 } else { 0 }));
                        }
                        Instr::F64Lt => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::I32(if x < y { 1 } else { 0 }));
                        }
                        Instr::F64Gt => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::I32(if x > y { 1 } else { 0 }));
                        }
                        Instr::F64Le => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::I32(if x <= y { 1 } else { 0 }));
                        }
                        Instr::F64Ge => {
                            let y = self.stack.pop().unwrap().unwrap_f64();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            self.stack.push(Val::I32(if x >= y { 1 } else { 0 }));
                        }
                        Instr::I32WrapI64 => {
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            self.stack.push(Val::I32(x as i32));
                        }
                        Instr::I64ExtendI32S => {
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I64(x as i64));
                        }
                        Instr::I64ExtendI32U => {
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I64(i32_convert_u32(x) as i64));
                        }
                        Instr::Drop => {
                            self.stack.pop().unwrap();
                        }
                        Instr::Select => {
                            let c = self.stack.pop().unwrap().unwrap_i32();
                            let y = self.stack.pop().unwrap();
                            let x = self.stack.pop().unwrap();
                            self.stack.push(if c != 0 { x } else { y });
                        }
                        Instr::LocalGet(idx) => {
                            self.stack.push(frame.locals[idx.to_idx()]);
                        }
                        Instr::LocalSet(idx) => {
                            let x = self.stack.pop().unwrap();
                            frame.locals[idx.to_idx()] = x;
                        }
                        Instr::LocalTee(idx) => {
                            let x = *self.stack.last().unwrap();
                            frame.locals[idx.to_idx()] = x;
                        }
                        Instr::GlobalGet(idx) => {
                            let instance = frame.module.upgrade().unwrap();

                            self.stack
                                .push(instance.globals[idx.to_idx()].0.borrow().value);
                        }
                        Instr::GlobalSet(idx) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap();
                            let mut global = instance.globals[idx.to_idx()].0.borrow_mut();
                            global.value = x;
                        }
                        Instr::I32Load(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_i32::<LittleEndian>().unwrap();
                                self.stack.push(Val::I32(x));
                            });
                        }
                        Instr::I64Load(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_i64::<LittleEndian>().unwrap();
                                self.stack.push(Val::I64(x));
                            });
                        }
                        Instr::F32Load(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_f32::<LittleEndian>().unwrap();
                                self.stack.push(Val::F32(x));
                            });
                        }
                        Instr::F64Load(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_f64::<LittleEndian>().unwrap();
                                self.stack.push(Val::F64(x));
                            });
                        }
                        Instr::I32Store(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_mut_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                cur.write_i32::<LittleEndian>(x).unwrap();
                            });
                        }
                        Instr::I64Store(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_mut_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                cur.write_i64::<LittleEndian>(x).unwrap();
                            });
                        }
                        Instr::F32Store(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_mut_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                cur.write_f32::<LittleEndian>(x).unwrap();
                            });
                        }
                        Instr::F64Store(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_mut_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                cur.write_f64::<LittleEndian>(x).unwrap();
                            });
                        }
                        Instr::I32Load8S(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_i8().unwrap();
                                self.stack.push(Val::I32(x as i32));
                            });
                        }
                        Instr::I32Load8U(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_u8().unwrap();
                                self.stack.push(Val::I32(x as i32));
                            });
                        }
                        Instr::I64Load8S(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_i8().unwrap();
                                self.stack.push(Val::I64(x as i64));
                            });
                        }
                        Instr::I64Load8U(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_u8().unwrap();
                                self.stack.push(Val::I64(x as i64));
                            });
                        }
                        Instr::I32Load16S(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_i16::<LittleEndian>().unwrap();
                                self.stack.push(Val::I32(x as i32));
                            });
                        }
                        Instr::I32Load16U(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_u16::<LittleEndian>().unwrap();
                                self.stack.push(Val::I32(x as i32));
                            });
                        }
                        Instr::I64Load16S(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_i16::<LittleEndian>().unwrap();
                                self.stack.push(Val::I64(x as i64));
                            });
                        }
                        Instr::I64Load16U(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_u16::<LittleEndian>().unwrap();
                                self.stack.push(Val::I64(x as i64));
                            });
                        }
                        Instr::I64Load32S(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_i32::<LittleEndian>().unwrap();
                                self.stack.push(Val::I64(x as i64));
                            });
                        }
                        Instr::I64Load32U(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_u32::<LittleEndian>().unwrap();
                                self.stack.push(Val::I64(x as i64));
                            });
                        }
                        Instr::I32Store8(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_mut_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                cur.write_i8(x as i8).unwrap();
                            });
                        }
                        Instr::I64Store8(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_mut_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                cur.write_i8(x as i8).unwrap();
                            });
                        }
                        Instr::I32Store16(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_mut_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                cur.write_i16::<LittleEndian>(x as i16).unwrap();
                            });
                        }
                        Instr::I64Store16(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_mut_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                cur.write_i16::<LittleEndian>(x as i16).unwrap();
                            });
                        }
                        Instr::I64Store32(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_mut_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                cur.write_i32::<LittleEndian>(x as i32).unwrap();
                            });
                        }
                        Instr::MemorySize => {
                            let instance = frame.module.upgrade().unwrap();
                            self.stack.push(Val::I32(
                                instance.mem.as_ref().unwrap().0.borrow().page_size(),
                            ));
                        }
                        Instr::MemoryGrow => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.stack.push(Val::I32(
                                instance.mem.as_ref().unwrap().0.borrow_mut().grow(x),
                            ));
                        }
                        Instr::Nop => {}
                        Instr::Unreachable => return Err(RuntimeError::Trap),
                        Instr::Block(rt, is) => {
                            self.instrs.push(AdminInstr::Label(
                                Label {
                                    instrs: vec![],
                                    n: rt.0.iter().count(),
                                },
                                is,
                            ));
                        }
                        Instr::Loop(rt, is) => {
                            self.instrs.push(AdminInstr::Label(
                                Label {
                                    instrs: vec![Instr::Loop(rt, is.clone())],
                                    n: 0,
                                },
                                is,
                            ));
                        }
                        Instr::If(rt, is1, is2) => {
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            self.instrs.push(AdminInstr::Label(
                                Label {
                                    instrs: vec![],
                                    n: rt.0.iter().count(),
                                },
                                if x != 0 { is1 } else { is2 },
                            ));
                        }
                        Instr::Br(l) => self.instrs.push(AdminInstr::Br(l)),
                        Instr::BrIf(l) => {
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            if x != 0 {
                                self.instrs.push(AdminInstr::Br(l));
                            }
                        }
                        Instr::BrTable(ls, l) => {
                            let i = self.stack.pop().unwrap().unwrap_i32() as usize;
                            if i < ls.len() {
                                self.instrs.push(AdminInstr::Br(ls[i]));
                            } else {
                                self.instrs.push(AdminInstr::Br(l));
                            }
                        }
                        Instr::Return => {
                            self.instrs.push(AdminInstr::Return);
                        }
                        Instr::Call(idx) => {
                            let instance = frame.module.upgrade().unwrap();
                            self.instrs
                                .push(AdminInstr::Invoke(instance.funcs.get_idx(idx).clone()));
                        }
                        Instr::CallIndirect(t) => {
                            let instance = frame.module.upgrade().unwrap();
                            let i = self.stack.pop().unwrap().unwrap_i32() as usize;
                            let func_idx = {
                                let table = instance.table.as_ref().unwrap().0.borrow();
                                if let Some(Some(func_idx)) = table.elem.get(i) {
                                    func_idx.clone()
                                } else {
                                    return Err(RuntimeError::Trap);
                                }
                            };
                            let func = instance.funcs.get_idx(func_idx);
                            if func.0.borrow().type_() != instance.types.get_idx(t) {
                                return Err(RuntimeError::Trap);
                            }
                            self.instrs.push(AdminInstr::Invoke(func.clone()));
                        }
                        x => unimplemented!("{:?}", x),
                    }
                    None
                }
                AdminInstr::Invoke(x) => Some(FrameLevelInstr::Invoke(x)),
                AdminInstr::Label(l, is) => Some(FrameLevelInstr::Label(l, is)),
                AdminInstr::Br(l) => Some(FrameLevelInstr::Br(l)),
                AdminInstr::Return => Some(FrameLevelInstr::Return),
            },
            None => Some(FrameLevelInstr::LabelEnd),
        })
    }
}

#[derive(Debug)]
pub struct Stack {
    // not empty
    pub stack: Vec<FrameStack>,
}

impl Stack {
    pub fn step(&mut self) -> Result<(), RuntimeError> {
        let cur_frame = self.stack.last_mut().unwrap();
        if let Some(instr) = cur_frame.step()? {
            let cur_label = cur_frame.stack.last_mut().unwrap();
            match instr {
                ModuleLevelInstr::Invoke(func) => match &*func.0.borrow() {
                    FuncInst::RuntimeFunc {
                        type_,
                        code,
                        module,
                    } => {
                        let fs = FrameStack {
                            frame: Frame {
                                locals: {
                                    let mut locals = Vec::new();
                                    locals.append(&mut pop_n(
                                        &mut cur_label.stack,
                                        type_.params().len(),
                                    ));
                                    locals.append(
                                        &mut code
                                            .locals
                                            .iter()
                                            .map(|vt| match vt {
                                                ValType::I32 => Val::I32(0),
                                                ValType::I64 => Val::I64(0),
                                                ValType::F32 => Val::F32(0.0),
                                                ValType::F64 => Val::F64(0.0),
                                            })
                                            .collect(),
                                    );

                                    locals
                                },
                                module: module.clone(),
                                n: type_.ret().iter().count(),
                            },
                            stack: vec![LabelStack {
                                stack: vec![],
                                label: Label {
                                    instrs: vec![],
                                    n: type_.ret().iter().count(),
                                },
                                instrs: code
                                    .body
                                    .0
                                    .clone()
                                    .into_iter()
                                    .map(AdminInstr::Instr)
                                    .rev()
                                    .collect(),
                            }],
                        };
                        self.stack.push(fs);
                    }
                    FuncInst::HostFunc { type_, host_code } => {
                        let params = pop_n(&mut cur_label.stack, type_.params().len());
                        if let Some(result) = host_code(params)? {
                            cur_label.stack.push(result);
                        }
                    }
                },
                ModuleLevelInstr::Return => {
                    let ret = cur_label.stack.pop();
                    if self.stack.pop().unwrap().frame.n != 0 {
                        self.stack
                            .last_mut()
                            .unwrap()
                            .stack
                            .last_mut()
                            .unwrap()
                            .stack
                            .push(ret.unwrap());
                    }
                }
            }
        }
        Ok(())
    }
}

fn i32_convert_u32(x: i32) -> u32 {
    let mut wtr = vec![];
    wtr.write_i32::<LittleEndian>(x).unwrap();
    let mut rdr = Cursor::new(wtr);
    rdr.read_u32::<LittleEndian>().unwrap()
}

fn u32_convert_i32(x: u32) -> i32 {
    let mut wtr = vec![];
    wtr.write_u32::<LittleEndian>(x).unwrap();
    let mut rdr = Cursor::new(wtr);
    rdr.read_i32::<LittleEndian>().unwrap()
}

fn i64_convert_u64(x: i64) -> u64 {
    let mut wtr = vec![];
    wtr.write_i64::<LittleEndian>(x).unwrap();
    let mut rdr = Cursor::new(wtr);
    rdr.read_u64::<LittleEndian>().unwrap()
}

fn u64_convert_i64(x: u64) -> i64 {
    let mut wtr = vec![];
    wtr.write_u64::<LittleEndian>(x).unwrap();
    let mut rdr = Cursor::new(wtr);
    rdr.read_i64::<LittleEndian>().unwrap()
}

fn pop_n<T>(vec: &mut Vec<T>, n: usize) -> Vec<T> {
    vec.split_off(vec.len() - n)
}

#[test]
fn test_pop_n() {
    assert_eq!(pop_n::<i32>(&mut vec![], 0), vec![] as Vec<i32>);
    assert_eq!(pop_n::<i32>(&mut vec![1, 2, 3, 4, 5], 2), vec![4, 5]);
}

enum Sign {
    Positive,
    Negative,
}

fn sign_f32(x: f32) -> Option<Sign> {
    if x.is_sign_positive() {
        Some(Sign::Positive)
    } else if x.is_sign_negative() {
        Some(Sign::Negative)
    } else {
        None
    }
}

fn sign_f64(x: f64) -> Option<Sign> {
    if x.is_sign_positive() {
        Some(Sign::Positive)
    } else if x.is_sign_negative() {
        Some(Sign::Negative)
    } else {
        None
    }
}
