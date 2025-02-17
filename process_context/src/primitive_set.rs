mod primitive;

pub use self::primitive::Primitive;
use crate::ProcessContext;
use stak_vm::{Error, Memory, PrimitiveSet};

/// A primitive set for process context.
pub struct ProcessContextPrimitiveSet<T: ProcessContext> {
    process_context: T,
}

impl<T: ProcessContext> ProcessContextPrimitiveSet<T> {
    /// Creates a primitive set.
    pub const fn new(process_context: T) -> Self {
        Self { process_context }
    }
}

impl<T: ProcessContext> PrimitiveSet for ProcessContextPrimitiveSet<T> {
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::COMMAND_LINE => {
                memory.set_register(memory.null());

                for argument in self.process_context.command_line_rev() {
                    let string = memory.build_string(argument)?;
                    let list = memory.cons(string.into(), memory.register())?;
                    memory.set_register(list);
                }

                memory.push(memory.register().into())?;
            }
            Primitive::ENVIRONMENT_VARIABLES => {
                memory.set_register(memory.null());

                for (key, value) in self.process_context.environment_variables() {
                    let pair = memory.allocate(memory.null().into(), memory.null().into())?;
                    let list = memory.cons(pair.into(), memory.register())?;
                    memory.set_register(list);

                    let string = memory.build_string(key)?;
                    memory.set_car_value(memory.car(memory.register()), string.into());

                    let string = memory.build_string(value)?;
                    memory.set_cdr_value(memory.car(memory.register()), string.into());
                }

                memory.push(memory.register().into())?;
            }
            _ => return Err(Error::IllegalPrimitive),
        }

        Ok(())
    }
}
