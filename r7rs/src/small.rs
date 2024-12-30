mod error;
mod primitive;

pub use self::error::Error;
use self::primitive::Primitive;
use core::ops::{Add, Div, Mul, Rem, Sub};
use stak_device::{Device, DevicePrimitiveSet};
use stak_file::{FilePrimitiveSet, FileSystem};
use stak_native::TypeCheckPrimitiveSet;
use stak_process_context::{ProcessContext, ProcessContextPrimitiveSet};
use stak_time::{Clock, TimePrimitiveSet};
use stak_vm::{Memory, Number, NumberRepresentation, PrimitiveSet, Tag, Type, Value};

/// A primitive set that covers [the R7RS small](https://standards.scheme.org/corrected-r7rs/r7rs.html).
pub struct SmallPrimitiveSet<D: Device, F: FileSystem, P: ProcessContext, C: Clock> {
    device: DevicePrimitiveSet<D>,
    file: FilePrimitiveSet<F>,
    process_context: ProcessContextPrimitiveSet<P>,
    time: TimePrimitiveSet<C>,
    type_check: TypeCheckPrimitiveSet,
}

impl<D: Device, F: FileSystem, P: ProcessContext, C: Clock> SmallPrimitiveSet<D, F, P, C> {
    /// Creates a primitive set.
    pub fn new(device: D, file_system: F, process_context: P, clock: C) -> Self {
        Self {
            device: DevicePrimitiveSet::new(device),
            file: FilePrimitiveSet::new(file_system),
            process_context: ProcessContextPrimitiveSet::new(process_context),
            time: TimePrimitiveSet::new(clock),
            type_check: Default::default(),
        }
    }

    /// Returns a reference to a device.
    pub fn device(&self) -> &D {
        self.device.device()
    }

    /// Returns a mutable reference to a device.
    pub fn device_mut(&mut self) -> &mut D {
        self.device.device_mut()
    }

    fn operate_top<'a>(
        memory: &mut Memory<'a>,
        operate: impl Fn(&Memory<'a>, Value) -> Value,
    ) -> Result<(), Error> {
        let x = memory.pop();
        memory.push(operate(memory, x))?;
        Ok(())
    }

    fn operate_unary(memory: &mut Memory, operate: fn(Number) -> Number) -> Result<(), Error> {
        let [x] = memory.pop_numbers();

        memory.push(operate(x).into())?;

        Ok(())
    }

    fn operate_binary(
        memory: &mut Memory,
        operate: fn(Number, Number) -> Number,
    ) -> Result<(), Error> {
        let [x, y] = memory.pop_numbers();

        memory.push(operate(x, y).into())?;

        Ok(())
    }

    fn operate_comparison(
        memory: &mut Memory,
        operate: fn(NumberRepresentation, NumberRepresentation) -> bool,
    ) -> Result<(), Error> {
        let [x, y] = memory.pop_numbers();

        memory.push(
            memory
                .boolean(operate(x.to_representation(), y.to_representation()))
                .into(),
        )?;
        Ok(())
    }

    fn rib(memory: &mut Memory, car: Value, cdr: Value, tag: Tag) -> Result<(), Error> {
        let rib = memory.allocate(car, cdr.set_tag(tag))?;
        memory.push(rib.into())?;
        Ok(())
    }

    fn set_field<'a>(
        memory: &mut Memory<'a>,
        set_field: fn(&mut Memory<'a>, Value, Value),
    ) -> Result<(), Error> {
        let [x, y] = memory.pop_many();

        set_field(memory, x, y);
        memory.push(y)?;
        Ok(())
    }

    fn tag<'a>(
        memory: &mut Memory<'a>,
        field: impl Fn(&Memory<'a>, Value) -> Value,
    ) -> Result<(), Error> {
        Self::operate_top(memory, |vm, value| {
            field(vm, value)
                .to_cons()
                .map(|cons| Number::new(cons.tag() as _))
                .unwrap_or_default()
                .into()
        })
    }
}

impl<D: Device, F: FileSystem, P: ProcessContext, C: Clock> PrimitiveSet
    for SmallPrimitiveSet<D, F, P, C>
{
    type Error = Error;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        match primitive {
            Primitive::RIB => {
                let [car, cdr, tag] = memory.pop_many();

                Self::rib(memory, car, cdr, tag.assume_number().to_i64() as _)?;
            }
            // Optimize a cons.
            Primitive::CONS => {
                let [car, cdr] = memory.pop_many();

                Self::rib(memory, car, cdr, Type::Pair as _)?;
            }
            Primitive::CLOSE => {
                let closure = memory.pop();

                Self::rib(
                    memory,
                    memory.car_value(closure),
                    memory.stack().into(),
                    Type::Procedure as _,
                )?;
            }
            Primitive::IS_RIB => Self::operate_top(memory, |memory, value| {
                memory.boolean(value.is_cons()).into()
            })?,
            Primitive::CAR => Self::operate_top(memory, Memory::car_value)?,
            Primitive::CDR => Self::operate_top(memory, Memory::cdr_value)?,
            Primitive::TAG => Self::tag(memory, Memory::cdr_value)?,
            Primitive::SET_CAR => Self::set_field(memory, Memory::set_car_value)?,
            Primitive::SET_CDR => Self::set_field(memory, Memory::set_cdr_value)?,
            Primitive::EQUAL => {
                let [x, y] = memory.pop_many();
                memory.push(memory.boolean(x == y).into())?;
            }
            Primitive::LESS_THAN => Self::operate_comparison(memory, |x, y| x < y)?,
            Primitive::ADD => Self::operate_binary(memory, Add::add)?,
            Primitive::SUBTRACT => Self::operate_binary(memory, Sub::sub)?,
            Primitive::MULTIPLY => Self::operate_binary(memory, Mul::mul)?,
            Primitive::DIVIDE => Self::operate_binary(memory, Div::div)?,
            Primitive::REMAINDER => Self::operate_binary(memory, Rem::rem)?,
            Primitive::EXPONENTIATION => {
                Self::operate_unary(memory, |x| Number::from_f64(libm::exp(x.to_f64())))?
            }
            Primitive::LOGARITHM => {
                Self::operate_unary(memory, |x| Number::from_f64(libm::log(x.to_f64())))?
            }
            Primitive::HALT => return Err(Error::Halt),
            Primitive::NULL | Primitive::PAIR => self
                .type_check
                .operate(memory, primitive - Primitive::NULL)?,
            Primitive::MEMQ => {
                let [x, xs] = memory.pop_many();
                let mut xs = xs.assume_cons();
                let mut y = memory.boolean(false);

                while xs != memory.null() {
                    if x == memory.car(xs) {
                        y = xs;
                        break;
                    }

                    xs = memory.cdr(xs).assume_cons();
                }

                memory.push(y.into())?;
            }
            Primitive::ASSQ => {
                let [x, xs] = memory.pop_many();
                let mut xs = xs.assume_cons();
                let mut y = memory.boolean(false);

                while xs != memory.null() {
                    let cons = memory.car(xs).assume_cons();

                    if x == memory.car(cons) {
                        y = cons;
                        break;
                    }

                    xs = memory.cdr(xs).assume_cons();
                }

                memory.push(y.into())?;
            }
            Primitive::READ | Primitive::WRITE | Primitive::WRITE_ERROR => {
                self.device.operate(memory, primitive - Primitive::READ)?
            }
            Primitive::OPEN_FILE
            | Primitive::CLOSE_FILE
            | Primitive::READ_FILE
            | Primitive::WRITE_FILE
            | Primitive::DELETE_FILE
            | Primitive::EXISTS_FILE => self
                .file
                .operate(memory, primitive - Primitive::OPEN_FILE)?,
            Primitive::COMMAND_LINE | Primitive::ENVIRONMENT_VARIABLES => self
                .process_context
                .operate(memory, primitive - Primitive::COMMAND_LINE)?,
            Primitive::CURRENT_JIFFY => self
                .time
                .operate(memory, primitive - Primitive::CURRENT_JIFFY)?,
            _ => return Err(stak_vm::Error::IllegalPrimitive.into()),
        }

        Ok(())
    }
}
