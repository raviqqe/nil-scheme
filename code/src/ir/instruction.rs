use crate::Operand;
use alloc::vec::Vec;

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Call(Operand),
    Closure(u64, Vec<Instruction>),
    Set(Operand),
    Get(Operand),
    Constant(Operand),
    If(Vec<Instruction>, Vec<Instruction>),
}

impl Instruction {
    pub const RETURN_CALL: u8 = 0;
    pub const CALL: u8 = 1;
    pub const CLOSURE: u8 = 2;
    pub const SET: u8 = 3;
    pub const GET: u8 = 4;
    pub const CONSTANT: u8 = 5;
    pub const IF: u8 = 6;
}
