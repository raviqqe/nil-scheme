use crate::cons::Cons;
use crate::number::Number;
use crate::{value::Value, Error};
use alloc::{vec, vec::Vec};
use core::fmt::{self, Display, Formatter};

const CONS_FIELD_COUNT: usize = 2;
const ZERO: Number = Number::new(0);

#[allow(dead_code)]
#[derive(Default)]
pub struct Vm<const N: usize> {
    heap: Vec<Value>,
    allocation_index: usize,
    allocation_limit: usize,
}

#[allow(dead_code)]
impl<const N: usize> Vm<N> {
    const SPACE_SIZE: usize = N * CONS_FIELD_COUNT;
    const HEAP_SIZE: usize = Self::SPACE_SIZE * 2;
    const HEAP_MIDDLE: usize = Self::SPACE_SIZE;
    const HEAP_TOP: usize = Self::HEAP_SIZE;

    pub fn new() -> Self {
        Self {
            heap: vec![ZERO.into(); Self::HEAP_SIZE],
            allocation_index: 0,
            allocation_limit: Self::HEAP_MIDDLE,
        }
    }

    pub fn car(&self, cons: Cons) -> Value {
        self.heap[cons.index()]
    }

    pub fn cdr(&self, cons: Cons) -> Value {
        self.heap[cons.index() + 1]
    }

    fn car_mut(&mut self, cons: Cons) -> &mut Value {
        &mut self.heap[cons.index() as usize]
    }

    fn cdr_mut(&mut self, cons: Cons) -> &mut Value {
        &mut self.heap[cons.index() as usize + 1]
    }

    pub fn allocate(&mut self) -> Cons {
        if self.allocation_index == self.allocation_limit {
            todo!("gc")
        }

        let cons = Cons::new(self.allocation_index as u64);
        self.allocation_index += CONS_FIELD_COUNT;
        cons
    }

    pub fn append(&mut self, value: Value, cons: Value) {
        let new = self.allocate();

        *self.car_mut(new) = value.into();
        *self.cdr_mut(new) = cons.into();
    }

    pub fn run(&mut self) -> Result<(), Error> {
        Ok(())
    }
}

impl<const N: usize> Display for Vm<N> {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        for index in 0..self.allocation_index / 2 {
            let cons = Cons::new(index as u64);

            write!(formatter, "{} {}", self.car(cons), self.cdr(cons))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::format;

    const HEAP_SIZE: usize = 1 << 8;

    #[test]
    fn create() {
        let vm = Vm::<HEAP_SIZE>::new();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn run_nothing() {
        let mut vm = Vm::<HEAP_SIZE>::new();

        vm.run().unwrap();

        insta::assert_display_snapshot!(vm);
    }

    #[test]
    fn create_list() {
        let mut vm = Vm::<HEAP_SIZE>::new();

        vm.append(Number::new(1).into(), ZERO.into());

        insta::assert_display_snapshot!(vm);
    }
}
