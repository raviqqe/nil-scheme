use crate::{Instruction, Operand, Program};
use alloc::vec;
use alloc::vec::Vec;

pub(crate) const INTEGER_BASE: u64 = i8::MAX as u64 + 1;

pub fn encode(program: &Program) -> Vec<u8> {
    let mut codes = vec![];

    encode_instructions(&mut codes, program.instructions());

    codes
}

// TODO Use short encoding for instruction operands.
fn encode_instructions(codes: &mut Vec<u8>, instructions: &[Instruction]) {
    for instruction in instructions {
        match instruction {
            Instruction::Apply(operand, bool) => todo!(),
            Instruction::Set(operand) => {
                encode_operand(codes, *operand);
                codes.push(Instruction::CONSTANT)
            }
            Instruction::Get(operand) => {
                encode_operand(codes, *operand);
                codes.push(Instruction::CONSTANT)
            }
            Instruction::Constant(number) => {
                encode_integer(codes, *number);
                codes.push(Instruction::CONSTANT)
            }
            Instruction::If(r#then, r#else) => todo!(),
        }
    }
}

fn encode_operand(codes: &mut Vec<u8>, operand: Operand) {
    match operand {
        Operand::Global(number) => encode_integer(codes, number),
        Operand::Local(number) => encode_integer(codes, (number as i64 * -1) as u64),
    }
}

fn encode_integer(codes: &mut Vec<u8>, mut number: u64) {
    let mut rest = false;

    while {
        let part = number % INTEGER_BASE;

        codes.push((if rest { -1 } else { 1 } * part as i64) as u8);

        number /= INTEGER_BASE;
        rest = true;

        number != 0
    } {}
}
