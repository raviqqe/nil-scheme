//! Native functions dynamically defined.

mod error;

pub use self::error::DynamicError;
use any_fn::AnyFn;
use core::any::TypeId;
use heapless::Vec;
use stak_vm::{Error, Memory, Number, PrimitiveSet, Type, Value};

const MAXIMUM_ARGUMENT_COUNT: usize = 16;

type ArgumentVec<T> = Vec<T, MAXIMUM_ARGUMENT_COUNT>;

/// A dynamic primitive set equipped with native functions in Rust.
pub struct DynamicPrimitiveSet<'a, 'b, const N: usize> {
    functions: &'a mut [AnyFn<'b>],
    values: [Option<any_fn::Value>; N],
}

impl<'a, 'b, const N: usize> DynamicPrimitiveSet<'a, 'b, N> {
    /// Creates a primitive set.
    pub fn new(functions: &'a mut [AnyFn<'b>]) -> Self {
        Self {
            functions,
            // TODO Garbage-collect foreign values.
            values: [const { None }; N],
        }
    }

    fn from_scheme(value: Value, type_id: TypeId) -> Option<any_fn::Value> {
        if type_id == TypeId::of::<f32>() {
            Some(any_fn::value(value.assume_number().to_f64() as f32))
        } else if type_id == TypeId::of::<f64>() {
            Some(any_fn::value(value.assume_number().to_f64()))
        } else if type_id == TypeId::of::<i8>() {
            Some(any_fn::value(value.assume_number().to_i64() as i8))
        } else if type_id == TypeId::of::<u8>() {
            Some(any_fn::value(value.assume_number().to_i64() as u8))
        } else if type_id == TypeId::of::<isize>() {
            Some(any_fn::value(value.assume_number().to_i64() as isize))
        } else if type_id == TypeId::of::<usize>() {
            Some(any_fn::value(value.assume_number().to_i64() as usize))
        } else {
            None
        }
    }

    fn into_scheme(
        &mut self,
        memory: &mut Memory,
        value: any_fn::Value,
        _type_id: TypeId,
    ) -> Result<Value, DynamicError> {
        let index = self
            .values
            .iter()
            .position(Option::is_none)
            .ok_or(Error::OutOfMemory)?;

        self.values[index] = Some(value);

        let cons = memory.allocate(
            Number::from_i64(index as _).into(),
            memory.null().set_tag(Type::Foreign as _).into(),
        )?;

        Ok(cons.into())
    }
}

impl<const N: usize> PrimitiveSet for DynamicPrimitiveSet<'_, '_, N> {
    type Error = DynamicError;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        let function = self
            .functions
            .get_mut(primitive)
            .ok_or(Error::IllegalPrimitive)?;

        let value = {
            let arguments = (0..function.arity())
                .map(|_| memory.pop())
                .collect::<ArgumentVec<_>>();

            let cloned_values = arguments
                .iter()
                .enumerate()
                .map(|(index, &value)| Self::from_scheme(value, function.parameter_types()[index]))
                .collect::<ArgumentVec<_>>();

            let mut copied_values = ArgumentVec::new();

            for &value in &arguments {
                let value = if value.is_cons()
                    && memory.cdr_value(value).tag() == Type::Foreign as _
                {
                    Some(
                        self.values
                            .get(memory.car(value.assume_cons()).assume_number().to_i64() as usize)
                            .ok_or(DynamicError::ObjectIndex)?
                            .as_ref()
                            .ok_or(DynamicError::ObjectIndex)?,
                    )
                } else {
                    None
                };

                copied_values
                    .push(value)
                    .map_err(|_| Error::ArgumentCount)?;
            }

            let value = function.call(
                copied_values
                    .into_iter()
                    .enumerate()
                    .map(|(index, value)| {
                        if let Some(value) = &cloned_values[index] {
                            value
                        } else {
                            value.unwrap()
                        }
                    })
                    .collect::<ArgumentVec<_>>()
                    .as_slice(),
            )?;

            value
        };

        let value = self.into_scheme(memory, value, TypeId::of::<()>())?;
        memory.push(value)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use any_fn::{r#fn, Ref};

    struct Foo {
        bar: usize,
    }

    impl Foo {
        fn new(bar: usize) -> Self {
            Self { bar }
        }

        fn bar(&self) -> usize {
            self.bar
        }

        fn baz(&mut self, value: usize) {
            self.bar += value;
        }
    }

    #[test]
    fn create() {
        let mut functions = [
            r#fn(Foo::new),
            r#fn::<(Ref<_>,), _>(Foo::bar),
            r#fn(Foo::baz),
        ];

        DynamicPrimitiveSet::<0>::new(&mut functions);
    }
}
