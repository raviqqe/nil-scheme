//! Native functions dynamically defined.

mod error;

use core::any::TypeId;

pub use self::error::DynamicError;
use any_fn::AnyFn;
use heapless::Vec;
use stak_vm::{Error, Memory, Number, PrimitiveSet, Type, Value};

const MAXIMUM_ARGUMENT_COUNT: usize = 16;

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

    fn from_scheme<'c>(
        values: &'c [Option<any_fn::Value>],
        memory: &mut Memory,
        value: Value,
    ) -> Result<&'c any_fn::Value, DynamicError> {
        Ok(values
            .get(memory.car(value.assume_cons()).assume_number().to_i64() as usize)
            .ok_or(DynamicError::ObjectIndex)?
            .as_ref()
            .ok_or(DynamicError::ObjectIndex)?)
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

        let cons = memory.cons(
            Number::from_i64(index as _).into(),
            memory.null().set_tag(Type::Foreign as _),
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
            let mut arguments = Vec::<_, MAXIMUM_ARGUMENT_COUNT>::new();

            for _ in 0..function.arity() {
                let value = memory.pop();
                let value = Self::from_scheme(&self.values, memory, value)?;

                arguments.push(value).map_err(|_| Error::ArgumentCount)?;
            }

            function.call(arguments.as_slice())?
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
