use super::instance::{FuncAddr, FuncInst, ModuleInst, TypedIdxAccess, Val};
use super::instance::{ValInterpret, ValPrimitive};
use crate::structure::instructions::Instr;
use crate::structure::modules::{LabelIdx, TypedIdx};
use crate::structure::types::ValType;
use crate::WasmError;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use frunk::{from_generic, into_generic, Generic, HCons, HNil};
use num::NumCast;
use std::convert::{From, Into, TryFrom};
use std::io::Cursor;
use std::rc::Weak;

pub trait StackValues: Sized {
    fn pop_stack(stack: &mut Vec<Val>) -> Option<Self>;
    fn push_stack(self, stack: &mut Vec<Val>);
}

impl<T: ValInterpret> StackValues for T {
    fn pop_stack(stack: &mut Vec<Val>) -> Option<Self> {
        let val = stack.pop()?;
        let primitive = <T::Primitive as TryFrom<Val>>::try_from(val).ok()?;
        Some(T::reinterpret(primitive))
    }
    fn push_stack(self, stack: &mut Vec<Val>) {
        stack.push(self.to_primitive().into());
    }
}

impl StackValues for HNil {
    fn pop_stack(_: &mut Vec<Val>) -> Option<Self> {
        Some(HNil)
    }
    fn push_stack(self, _: &mut Vec<Val>) {}
}

impl<H: ValInterpret, T: StackValues> StackValues for HCons<H, T> {
    fn pop_stack(stack: &mut Vec<Val>) -> Option<Self> {
        let tail = T::pop_stack(stack)?;
        let head = H::pop_stack(stack)?;
        Some(HCons { head, tail })
    }
    fn push_stack(self, stack: &mut Vec<Val>) {
        self.head.push_stack(stack);
        self.tail.push_stack(stack);
    }
}

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
    pub fn step(&mut self) -> Result<Option<ModuleLevelInstr>, WasmError> {
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
    fn run_ok<I, O>(&mut self, f: impl FnOnce(I) -> O) -> ()
    where
        I: Generic,
        I::Repr: StackValues,
        O: Generic,
        O::Repr: StackValues,
    {
        self.run(|i| Ok(f(i))).unwrap();
    }

    fn run<I, O>(&mut self, f: impl FnOnce(I) -> Result<O, WasmError>) -> Result<(), WasmError>
    where
        I: Generic,
        I::Repr: StackValues,
        O: Generic,
        O::Repr: StackValues,
    {
        let input = I::Repr::pop_stack(&mut self.stack).unwrap();
        let output = f(from_generic(input))?;
        into_generic(output).push_stack(&mut self.stack);
        Ok(())
    }

    fn run_const<T: ValInterpret>(&mut self, f: impl FnOnce() -> T) {
        self.run_ok(|(): ()| -> (T,) { (f(),) })
    }

    fn run_unop<T: ValInterpret>(
        &mut self,
        f: impl FnOnce(T) -> Result<T, WasmError>,
    ) -> Result<(), WasmError> {
        self.run(|(x,): (T,)| -> Result<(T,), _> { Ok((f(x)?,)) })
    }

    fn run_binop<T: ValInterpret>(
        &mut self,
        f: impl FnOnce(T, T) -> Result<T, WasmError>,
    ) -> Result<(), WasmError> {
        self.run(|(a, b): (T, T)| -> Result<(T,), _> { Ok((f(a, b)?,)) })
    }

    fn run_testop<T: ValInterpret>(&mut self, f: impl FnOnce(T) -> bool) {
        self.run_ok(|(x,): (T,)| -> (bool,) { (f(x),) })
    }

    fn run_reop<T: ValInterpret>(&mut self, f: impl FnOnce(T, T) -> bool) {
        self.run_ok(|(x, y): (T, T)| -> (bool,) { (f(x, y),) })
    }

    fn run_cvtop<T: ValInterpret, R: ValInterpret>(
        &mut self,
        f: impl FnOnce(T) -> Result<R, WasmError>,
    ) -> Result<(), WasmError> {
        self.run(|(x,): (T,)| -> Result<(R,), _> { Ok((f(x)?,)) })
    }

    fn step(&mut self, frame: &mut Frame) -> Result<Option<FrameLevelInstr>, WasmError> {
        Ok(match self.instrs.pop() {
            Some(instr) => match instr {
                AdminInstr::Instr(instr) => {
                    match instr {
                        Instr::I32Const(x) => {
                            self.run_const(|| -> i32 { x });
                        }
                        Instr::I64Const(x) => {
                            self.run_const(|| -> i64 { x });
                        }
                        Instr::F32Const(x) => {
                            self.run_const(|| -> f32 { x });
                        }
                        Instr::F64Const(x) => {
                            self.run_const(|| -> f64 { x });
                        }
                        Instr::I32Clz => {
                            self.run_unop(|x: i32| -> Result<i32, _> {
                                Ok(x.leading_zeros() as i32)
                            })?;
                        }
                        Instr::I32Ctz => {
                            self.run_unop(|x: i32| -> Result<i32, _> {
                                Ok(x.trailing_zeros() as i32)
                            })?;
                        }
                        Instr::I32Popcnt => {
                            self.run_unop(|x: i32| -> Result<i32, _> {
                                Ok(x.count_ones() as i32)
                            })?;
                        }
                        Instr::I64Clz => {
                            self.run_unop(|x: i64| -> Result<i64, _> {
                                Ok(x.leading_zeros() as i64)
                            })?;
                        }
                        Instr::I64Ctz => {
                            self.run_unop(|x: i64| -> Result<i64, _> {
                                Ok(x.trailing_zeros() as i64)
                            })?;
                        }
                        Instr::I64Popcnt => {
                            self.run_unop(|x: i64| -> Result<i64, _> {
                                Ok(x.count_ones() as i64)
                            })?;
                        }
                        Instr::F32Abs => {
                            self.run_unop(|x: f32| -> Result<f32, _> { Ok(x.abs()) })?;
                        }
                        Instr::F32Neg => {
                            self.run_unop(|x: f32| -> Result<f32, _> { Ok(-x) })?;
                        }
                        Instr::F32Sqrt => {
                            self.run_unop(|x: f32| -> Result<f32, _> { Ok(x.sqrt()) })?;
                        }
                        Instr::F32Ceil => {
                            self.run_unop(|x: f32| -> Result<f32, _> { Ok(x.ceil()) })?;
                        }
                        Instr::F32Floor => {
                            self.run_unop(|x: f32| -> Result<f32, _> { Ok(x.floor()) })?;
                        }
                        Instr::F32Trunc => {
                            self.run_unop(|x: f32| -> Result<f32, _> { Ok(x.trunc()) })?;
                        }
                        Instr::F32Nearest => {
                            self.run_unop(|x: f32| -> Result<f32, _> {
                                let x_mod2 = x % 2.0;
                                Ok(if x_mod2 == 0.5 {
                                    x.floor()
                                } else if x_mod2 == -0.5 {
                                    x.ceil()
                                } else {
                                    x.round()
                                })
                            })?;
                        }
                        Instr::F64Abs => {
                            self.run_unop(|x: f64| -> Result<f64, _> { Ok(x.abs()) })?;
                        }
                        Instr::F64Neg => {
                            self.run_unop(|x: f64| -> Result<f64, _> { Ok(-x) })?;
                        }
                        Instr::F64Sqrt => {
                            self.run_unop(|x: f64| -> Result<f64, _> { Ok(x.sqrt()) })?;
                        }
                        Instr::F64Ceil => {
                            self.run_unop(|x: f64| -> Result<f64, _> { Ok(x.ceil()) })?;
                        }
                        Instr::F64Floor => {
                            self.run_unop(|x: f64| -> Result<f64, _> { Ok(x.floor()) })?;
                        }
                        Instr::F64Trunc => {
                            self.run_unop(|x: f64| -> Result<f64, _> { Ok(x.trunc()) })?;
                        }
                        Instr::F64Nearest => {
                            self.run_unop(|x: f64| -> Result<f64, _> {
                                let x_mod2 = x % 2.0;
                                Ok(if x_mod2 == 0.5 {
                                    x.floor()
                                } else if x_mod2 == -0.5 {
                                    x.ceil()
                                } else {
                                    x.round()
                                })
                            })?;
                        }
                        Instr::I32Add => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> {
                                Ok(x.overflowing_add(y).0)
                            })?;
                        }
                        Instr::I32Sub => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> {
                                Ok(x.overflowing_sub(y).0)
                            })?;
                        }
                        Instr::I32Mul => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> {
                                Ok(x.overflowing_mul(y).0)
                            })?;
                        }
                        Instr::I32DivS => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> {
                                if y == 0 {
                                    return Err(WasmError::RuntimeError);
                                }
                                Ok(x.checked_div(y).ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I32DivU => {
                            self.run_binop(|x: u32, y: u32| -> Result<u32, _> {
                                if y == 0 {
                                    return Err(WasmError::RuntimeError);
                                }
                                Ok(x.checked_div(y).ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I32RemS => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> {
                                if y == 0 {
                                    return Err(WasmError::RuntimeError);
                                }
                                Ok(x.overflowing_rem(y).0)
                            })?;
                        }
                        Instr::I32RemU => {
                            self.run_binop(|x: u32, y: u32| -> Result<u32, _> {
                                if y == 0 {
                                    return Err(WasmError::RuntimeError);
                                }
                                Ok(x.overflowing_rem(y).0)
                            })?;
                        }
                        Instr::I32And => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> { Ok(x & y) })?;
                        }
                        Instr::I32Or => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> { Ok(x | y) })?;
                        }
                        Instr::I32Xor => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> { Ok(x ^ y) })?;
                        }
                        Instr::I32ShL => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> {
                                Ok(x.overflowing_shl(y as u32).0)
                            })?;
                        }
                        Instr::I32ShrS => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> {
                                Ok(x.overflowing_shr(y as u32).0)
                            })?;
                        }
                        Instr::I32ShrU => {
                            self.run_binop(|x: u32, y: u32| -> Result<u32, _> {
                                Ok(x.overflowing_shr(y).0)
                            })?;
                        }
                        Instr::I32Rotl => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> {
                                Ok(x.rotate_left(y as u32))
                            })?;
                        }
                        Instr::I32Rotr => {
                            self.run_binop(|x: i32, y: i32| -> Result<i32, _> {
                                Ok(x.rotate_right(y as u32))
                            })?;
                        }
                        Instr::I64Add => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> {
                                Ok(x.overflowing_add(y).0)
                            })?;
                        }
                        Instr::I64Sub => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> {
                                Ok(x.overflowing_sub(y).0)
                            })?;
                        }
                        Instr::I64Mul => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> {
                                Ok(x.overflowing_mul(y).0)
                            })?;
                        }
                        Instr::I64DivS => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> {
                                if y == 0 {
                                    return Err(WasmError::RuntimeError);
                                }
                                Ok(x.checked_div(y).ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I64DivU => {
                            self.run_binop(|x: u64, y: u64| -> Result<u64, _> {
                                if y == 0 {
                                    return Err(WasmError::RuntimeError);
                                }
                                Ok(x.checked_div(y).ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I64RemS => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> {
                                if y == 0 {
                                    return Err(WasmError::RuntimeError);
                                }
                                Ok(x.overflowing_rem(y).0)
                            })?;
                        }
                        Instr::I64RemU => {
                            self.run_binop(|x: u64, y: u64| -> Result<u64, _> {
                                if y == 0 {
                                    return Err(WasmError::RuntimeError);
                                }
                                Ok(x.overflowing_rem(y).0)
                            })?;
                        }
                        Instr::I64And => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> { Ok(x & y) })?;
                        }
                        Instr::I64Or => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> { Ok(x | y) })?;
                        }
                        Instr::I64Xor => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> { Ok(x ^ y) })?;
                        }
                        Instr::I64ShL => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> {
                                Ok(x.overflowing_shl(y as u32).0)
                            })?;
                        }
                        Instr::I64ShrS => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> {
                                Ok(x.overflowing_shr(y as u32).0)
                            })?;
                        }
                        Instr::I64ShrU => {
                            self.run_binop(|x: u64, y: u64| -> Result<u64, _> {
                                Ok(x.overflowing_shr(y as u32).0)
                            })?;
                        }
                        Instr::I64Rotl => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> {
                                Ok(x.rotate_left(y as u32))
                            })?;
                        }
                        Instr::I64Rotr => {
                            self.run_binop(|x: i64, y: i64| -> Result<i64, _> {
                                Ok(x.rotate_right(y as u32))
                            })?;
                        }
                        Instr::F32Add => {
                            self.run_binop(|x: f32, y: f32| -> Result<f32, _> { Ok(x + y) })?;
                        }
                        Instr::F32Sub => {
                            self.run_binop(|x: f32, y: f32| -> Result<f32, _> { Ok(x - y) })?;
                        }
                        Instr::F32Mul => {
                            self.run_binop(|x: f32, y: f32| -> Result<f32, _> { Ok(x * y) })?;
                        }
                        Instr::F32Div => {
                            self.run_binop(|x: f32, y: f32| -> Result<f32, _> { Ok(x / y) })?;
                        }
                        Instr::F32Min => {
                            self.run_binop(|x: f32, y: f32| -> Result<f32, _> {
                                // 0.0, -0.0対策
                                Ok(match (sign_f32(x), sign_f32(y)) {
                                    (Some(Sign::Positive), Some(Sign::Negative)) => y,
                                    (Some(Sign::Negative), Some(Sign::Positive)) => x,
                                    _ => x.min(y),
                                })
                            })?;
                        }
                        Instr::F32Max => {
                            self.run_binop(|x: f32, y: f32| -> Result<f32, _> {
                                Ok(match (sign_f32(x), sign_f32(y)) {
                                    (Some(Sign::Positive), Some(Sign::Negative)) => x,
                                    (Some(Sign::Negative), Some(Sign::Positive)) => y,
                                    _ => x.max(y),
                                })
                            })?;
                        }
                        Instr::F32CopySign => {
                            self.run_binop(|x: f32, y: f32| -> Result<f32, _> {
                                Ok(x.copysign(y))
                            })?;
                        }
                        Instr::F64Add => {
                            self.run_binop(|x: f64, y: f64| -> Result<f64, _> { Ok(x + y) })?;
                        }
                        Instr::F64Sub => {
                            self.run_binop(|x: f64, y: f64| -> Result<f64, _> { Ok(x - y) })?;
                        }
                        Instr::F64Mul => {
                            self.run_binop(|x: f64, y: f64| -> Result<f64, _> { Ok(x * y) })?;
                        }
                        Instr::F64Div => {
                            self.run_binop(|x: f64, y: f64| -> Result<f64, _> { Ok(x / y) })?;
                        }
                        Instr::F64Min => {
                            self.run_binop(|x: f64, y: f64| -> Result<f64, _> {
                                Ok(match (sign_f64(x), sign_f64(y)) {
                                    (Some(Sign::Positive), Some(Sign::Negative)) => y,
                                    (Some(Sign::Negative), Some(Sign::Positive)) => x,
                                    _ => x.min(y),
                                })
                            })?;
                        }
                        Instr::F64Max => {
                            self.run_binop(|x: f64, y: f64| -> Result<f64, _> {
                                Ok(match (sign_f64(x), sign_f64(y)) {
                                    (Some(Sign::Positive), Some(Sign::Negative)) => x,
                                    (Some(Sign::Negative), Some(Sign::Positive)) => y,
                                    _ => x.max(y),
                                })
                            })?;
                        }
                        Instr::F64CopySign => {
                            self.run_binop(|x: f64, y: f64| -> Result<f64, _> {
                                Ok(x.copysign(y))
                            })?;
                        }
                        Instr::I32Eqz => {
                            self.run_testop(|x: i32| -> bool { x == 0 });
                        }
                        Instr::I64Eqz => {
                            self.run_testop(|x: i64| -> bool { x == 0 });
                        }
                        Instr::I32Eq => {
                            self.run_reop(|x: i32, y: i32| -> bool { x == y });
                        }
                        Instr::I32Ne => {
                            self.run_reop(|x: i32, y: i32| -> bool { x != y });
                        }
                        Instr::I32LtS => {
                            self.run_reop(|x: i32, y: i32| -> bool { x < y });
                        }
                        Instr::I32LtU => {
                            self.run_reop(|x: u32, y: u32| -> bool { x < y });
                        }
                        Instr::I32GtS => {
                            self.run_reop(|x: i32, y: i32| -> bool { x > y });
                        }
                        Instr::I32GtU => {
                            self.run_reop(|x: u32, y: u32| -> bool { x > y });
                        }
                        Instr::I32LeS => {
                            self.run_reop(|x: i32, y: i32| -> bool { x <= y });
                        }
                        Instr::I32LeU => {
                            self.run_reop(|x: u32, y: u32| -> bool { x <= y });
                        }
                        Instr::I32GeS => {
                            self.run_reop(|x: i32, y: i32| -> bool { x >= y });
                        }
                        Instr::I32GeU => {
                            self.run_reop(|x: u32, y: u32| -> bool { x >= y });
                        }
                        Instr::I64Eq => {
                            self.run_reop(|x: i64, y: i64| -> bool { x == y });
                        }
                        Instr::I64Ne => {
                            self.run_reop(|x: i64, y: i64| -> bool { x != y });
                        }
                        Instr::I64LtS => {
                            self.run_reop(|x: i64, y: i64| -> bool { x < y });
                        }
                        Instr::I64LtU => {
                            self.run_reop(|x: u64, y: u64| -> bool { x < y });
                        }
                        Instr::I64GtS => {
                            self.run_reop(|x: i64, y: i64| -> bool { x > y });
                        }
                        Instr::I64GtU => {
                            self.run_reop(|x: u64, y: u64| -> bool { x > y });
                        }
                        Instr::I64LeS => {
                            self.run_reop(|x: i64, y: i64| -> bool { x <= y });
                        }
                        Instr::I64LeU => {
                            self.run_reop(|x: u64, y: u64| -> bool { x <= y });
                        }
                        Instr::I64GeS => {
                            self.run_reop(|x: i64, y: i64| -> bool { x >= y });
                        }
                        Instr::I64GeU => {
                            self.run_reop(|x: u64, y: u64| -> bool { x >= y });
                        }
                        Instr::F32Eq => {
                            self.run_reop(|x: f32, y: f32| -> bool { x == y });
                        }
                        Instr::F32Ne => {
                            self.run_reop(|x: f32, y: f32| -> bool { x != y });
                        }
                        Instr::F32Lt => {
                            self.run_reop(|x: f32, y: f32| -> bool { x < y });
                        }
                        Instr::F32Gt => {
                            self.run_reop(|x: f32, y: f32| -> bool { x > y });
                        }
                        Instr::F32Le => {
                            self.run_reop(|x: f32, y: f32| -> bool { x <= y });
                        }
                        Instr::F32Ge => {
                            self.run_reop(|x: f32, y: f32| -> bool { x >= y });
                        }
                        Instr::F64Eq => {
                            self.run_reop(|x: f64, y: f64| -> bool { x == y });
                        }
                        Instr::F64Ne => {
                            self.run_reop(|x: f64, y: f64| -> bool { x != y });
                        }
                        Instr::F64Lt => {
                            self.run_reop(|x: f64, y: f64| -> bool { x < y });
                        }
                        Instr::F64Gt => {
                            self.run_reop(|x: f64, y: f64| -> bool { x > y });
                        }
                        Instr::F64Le => {
                            self.run_reop(|x: f64, y: f64| -> bool { x <= y });
                        }
                        Instr::F64Ge => {
                            self.run_reop(|x: f64, y: f64| -> bool { x >= y });
                        }
                        Instr::I32WrapI64 => {
                            self.run_cvtop(|x: i64| -> Result<i32, _> { Ok(x as i32) })?;
                        }
                        Instr::I64ExtendI32S => {
                            self.run_cvtop(|x: i32| -> Result<i64, _> { Ok(x as i64) })?;
                        }
                        Instr::I64ExtendI32U => {
                            self.run_cvtop(|x: u32| -> Result<i64, _> { Ok(x as i64) })?;
                        }
                        Instr::I32TruncF32S => {
                            self.run_cvtop(|x: f32| -> Result<i32, _> {
                                Ok(<i32 as NumCast>::from(x)
                                    .ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I32TruncF32U => {
                            self.run_cvtop(|x: f32| -> Result<u32, _> {
                                Ok(<u32 as NumCast>::from(x)
                                    .ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I32TruncF64S => {
                            self.run_cvtop(|x: f64| -> Result<i32, _> {
                                Ok(<i32 as NumCast>::from(x)
                                    .ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I32TruncF64U => {
                            self.run_cvtop(|x: f64| -> Result<u32, _> {
                                Ok(<u32 as NumCast>::from(x)
                                    .ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I64TruncF32S => {
                            self.run_cvtop(|x: f32| -> Result<i64, _> {
                                Ok(<i64 as NumCast>::from(x)
                                    .ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I64TruncF32U => {
                            self.run_cvtop(|x: f32| -> Result<u64, _> {
                                Ok(<u64 as NumCast>::from(x)
                                    .ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I64TruncF64S => {
                            self.run_cvtop(|x: f64| -> Result<i64, _> {
                                Ok(<i64 as NumCast>::from(x)
                                    .ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::I64TruncF64U => {
                            self.run_cvtop(|x: f64| -> Result<u64, _> {
                                Ok(<u64 as NumCast>::from(x)
                                    .ok_or_else(|| WasmError::RuntimeError)?)
                            })?;
                        }
                        Instr::F32DemoteF64 => {
                            self.run_cvtop(|x: f64| -> Result<f32, _> { Ok(x as f32) })?;
                        }
                        Instr::F64PromoteF32 => {
                            self.run_cvtop(|x: f32| -> Result<f64, _> { Ok(x as f64) })?;
                        }
                        Instr::F32ConvertI32S => {
                            self.run_cvtop(|x: i32| -> Result<f32, _> { Ok(x as f32) })?;
                        }
                        Instr::F32ConvertI32U => {
                            self.run_cvtop(|x: u32| -> Result<f32, _> { Ok(x as f32) })?;
                        }
                        Instr::F32ConvertI64S => {
                            self.run_cvtop(|x: i64| -> Result<f32, _> { Ok(x as f32) })?;
                        }
                        Instr::F32ConvertI64U => {
                            self.run_cvtop(|x: u64| -> Result<f32, _> { Ok(x as f32) })?;
                        }
                        Instr::F64ConvertI32S => {
                            self.run_cvtop(|x: i32| -> Result<f64, _> { Ok(x as f64) })?;
                        }
                        Instr::F64ConvertI32U => {
                            self.run_cvtop(|x: u32| -> Result<f64, _> { Ok(x as f64) })?;
                        }
                        Instr::F64ConvertI64S => {
                            self.run_cvtop(|x: i64| -> Result<f64, _> { Ok(x as f64) })?;
                        }
                        Instr::F64ConvertI64U => {
                            self.run_cvtop(|x: u64| -> Result<f64, _> { Ok(x as f64) })?;
                        }
                        Instr::I32ReinterpretF32 => {
                            let x = self.stack.pop().unwrap().unwrap_f32();

                            let mut wtr = vec![];
                            wtr.write_f32::<LittleEndian>(x).unwrap();
                            let mut rdr = Cursor::new(wtr);

                            self.stack
                                .push(Val::I32(rdr.read_i32::<LittleEndian>().unwrap()));
                        }
                        Instr::I64ReinterpretF64 => {
                            let x = self.stack.pop().unwrap().unwrap_f64();

                            let mut wtr = vec![];
                            wtr.write_f64::<LittleEndian>(x).unwrap();
                            let mut rdr = Cursor::new(wtr);

                            self.stack
                                .push(Val::I64(rdr.read_i64::<LittleEndian>().unwrap()));
                        }
                        Instr::F32ReinterpretI32 => {
                            let x = self.stack.pop().unwrap().unwrap_i32();

                            let mut wtr = vec![];
                            wtr.write_i32::<LittleEndian>(x).unwrap();
                            let mut rdr = Cursor::new(wtr);

                            self.stack
                                .push(Val::F32(rdr.read_f32::<LittleEndian>().unwrap()));
                        }
                        Instr::F64ReinterpretI64 => {
                            let x = self.stack.pop().unwrap().unwrap_i64();

                            let mut wtr = vec![];
                            wtr.write_i64::<LittleEndian>(x).unwrap();
                            let mut rdr = Cursor::new(wtr);

                            self.stack
                                .push(Val::F64(rdr.read_f64::<LittleEndian>().unwrap()));
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
                                let x = cur
                                    .read_i32::<LittleEndian>()
                                    .map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I32(x));
                                Ok(())
                            })?;
                        }
                        Instr::I64Load(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur
                                    .read_i64::<LittleEndian>()
                                    .map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I64(x));
                                Ok(())
                            })?;
                        }
                        Instr::F32Load(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur
                                    .read_f32::<LittleEndian>()
                                    .map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::F32(x));
                                Ok(())
                            })?;
                        }
                        Instr::F64Load(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur
                                    .read_f64::<LittleEndian>()
                                    .map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::F64(x));
                                Ok(())
                            })?;
                        }
                        Instr::I32Store(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().write_with_cursor(
                                ptr + m.offset as usize,
                                |mut cur| {
                                    cur.write_i32::<LittleEndian>(x).unwrap();
                                },
                            )?;
                        }
                        Instr::I64Store(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().write_with_cursor(
                                ptr + m.offset as usize,
                                |mut cur| {
                                    cur.write_i64::<LittleEndian>(x).unwrap();
                                },
                            )?;
                        }
                        Instr::F32Store(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_f32();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().write_with_cursor(
                                ptr + m.offset as usize,
                                |mut cur| {
                                    cur.write_f32::<LittleEndian>(x).unwrap();
                                },
                            )?;
                        }
                        Instr::F64Store(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_f64();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().write_with_cursor(
                                ptr + m.offset as usize,
                                |mut cur| {
                                    cur.write_f64::<LittleEndian>(x).unwrap();
                                },
                            )?;
                        }
                        Instr::I32Load8S(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_i8().map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I32(x as i32));
                                Ok(())
                            })?;
                        }
                        Instr::I32Load8U(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_u8().map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I32(x as i32));
                                Ok(())
                            })?;
                        }
                        Instr::I64Load8S(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_i8().map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I64(x as i64));
                                Ok(())
                            })?;
                        }
                        Instr::I64Load8U(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur.read_u8().map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I64(x as i64));
                                Ok(())
                            })?;
                        }
                        Instr::I32Load16S(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur
                                    .read_i16::<LittleEndian>()
                                    .map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I32(x as i32));
                                Ok(())
                            })?;
                        }
                        Instr::I32Load16U(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur
                                    .read_u16::<LittleEndian>()
                                    .map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I32(x as i32));
                                Ok(())
                            })?;
                        }
                        Instr::I64Load16S(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur
                                    .read_i16::<LittleEndian>()
                                    .map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I64(x as i64));
                                Ok(())
                            })?;
                        }
                        Instr::I64Load16U(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur
                                    .read_u16::<LittleEndian>()
                                    .map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I64(x as i64));
                                Ok(())
                            })?;
                        }
                        Instr::I64Load32S(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur
                                    .read_i32::<LittleEndian>()
                                    .map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I64(x as i64));
                                Ok(())
                            })?;
                        }
                        Instr::I64Load32U(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().with_cursor(|mut cur| {
                                cur.set_position((ptr + m.offset as usize) as u64);
                                let x = cur
                                    .read_u32::<LittleEndian>()
                                    .map_err(|_| WasmError::RuntimeError)?;
                                self.stack.push(Val::I64(x as i64));
                                Ok(())
                            })?;
                        }
                        Instr::I32Store8(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().write_with_cursor(
                                ptr + m.offset as usize,
                                |mut cur| {
                                    cur.write_i8(x as i8).unwrap();
                                },
                            )?;
                        }
                        Instr::I64Store8(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().write_with_cursor(
                                ptr + m.offset as usize,
                                |mut cur| {
                                    cur.write_i8(x as i8).unwrap();
                                },
                            )?;
                        }
                        Instr::I32Store16(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i32();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().write_with_cursor(
                                ptr + m.offset as usize,
                                |mut cur| {
                                    cur.write_i16::<LittleEndian>(x as i16).unwrap();
                                },
                            )?;
                        }
                        Instr::I64Store16(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().write_with_cursor(
                                ptr + m.offset as usize,
                                |mut cur| {
                                    cur.write_i16::<LittleEndian>(x as i16).unwrap();
                                },
                            )?;
                        }
                        Instr::I64Store32(m) => {
                            let instance = frame.module.upgrade().unwrap();
                            let x = self.stack.pop().unwrap().unwrap_i64();
                            let ptr = self.stack.pop().unwrap().unwrap_i32() as usize;
                            instance.mem.as_ref().unwrap().write_with_cursor(
                                ptr + m.offset as usize,
                                |mut cur| {
                                    cur.write_i32::<LittleEndian>(x as i32).unwrap();
                                },
                            )?;
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
                        Instr::Unreachable => return Err(WasmError::RuntimeError),
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
                            let func = {
                                let table = instance.table.as_ref().unwrap().0.borrow();
                                if let Some(Some(func)) = table.elem.get(i) {
                                    func.clone()
                                } else {
                                    return Err(WasmError::RuntimeError);
                                }
                            };
                            if &func.0.borrow().type_() != instance.types.get_idx(t) {
                                return Err(WasmError::RuntimeError);
                            }
                            self.instrs.push(AdminInstr::Invoke(func.clone()));
                        }
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
    pub fn step(&mut self) -> Result<(), WasmError> {
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
