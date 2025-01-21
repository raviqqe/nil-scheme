//! Native functions dynamically defined.

mod error;

pub use self::error::DynamicError;
use alloc::boxed::Box;
use any_fn::AnyFn;
use core::{any::Any, cell::RefCell};
use heapless::Vec;
use stak_vm::{Error, Memory, Number, PrimitiveSet, Type};

const MAXIMUM_ARGUMENT_COUNT: usize = 16;

/// A dynamic primitive set equipped with native functions in Rust.
pub struct DynamicPrimitiveSet<'a, const N: usize> {
    functions: &'a mut [AnyFn<'a>],
    values: [Option<RefCell<Box<dyn Any>>>; N],
}

impl<'a, const N: usize> DynamicPrimitiveSet<'a, N> {
    /// Creates a primitive set.
    pub fn new(functions: &'a mut [AnyFn<'a>]) -> Self {
        Self {
            functions,
            // TODO Garbage-collect foreign values.
            values: [const { None }; N],
        }
    }
}

impl<const N: usize> PrimitiveSet for DynamicPrimitiveSet<'_, N> {
    type Error = DynamicError;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        let function = self
            .functions
            .get_mut(primitive)
            .ok_or(Error::IllegalPrimitive)?;

        let (value, index) = {
            let mut arguments = Vec::<_, MAXIMUM_ARGUMENT_COUNT>::new();

            for _ in 0..function.arity() {
                let value = memory.pop();
                // TODO Convert Scheme values into Rust values automatically?
                let value = self.values
                    [memory.car(value.assume_cons()).assume_number().to_i64() as usize]
                    .as_ref()
                    .ok_or(DynamicError::ObjectIndex)?;

                arguments.push(value).map_err(|_| Error::ArgumentCount)?;
            }

            let value = function.call(arguments.as_slice())?;

            (
                value,
                self.values
                    .iter()
                    .position(Option::is_none)
                    .ok_or(Error::OutOfMemory)?,
            )
        };

        self.values[index] = Some(RefCell::new(value));

        let cons = memory.cons(
            Number::from_i64(index as _).into(),
            memory.null().set_tag(Type::Foreign as _),
        )?;
        memory.push(cons.into())?;

        Ok(())
    }
}
