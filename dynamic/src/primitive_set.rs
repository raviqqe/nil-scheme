use crate::{error::DynamicError, scheme_value::SchemeValue};
use alloc::{boxed::Box, string::String, vec, vec::Vec};
use any_fn::AnyFn;
use bitvec::bitvec;
use core::any::TypeId;
use stak_vm::{Cons, Error, Memory, Number, PrimitiveSet, Type, Value};

const MAXIMUM_ARGUMENT_COUNT: usize = 16;

type ArgumentVec<T> = heapless::Vec<T, MAXIMUM_ARGUMENT_COUNT>;
type SchemeType = (
    TypeId,
    Box<dyn Fn(&Memory, Value) -> Option<any_fn::Value>>,
    Box<dyn Fn(&mut Memory, any_fn::Value) -> Result<Value, DynamicError>>,
);

/// A dynamic primitive set equipped with native functions in Rust.
pub struct DynamicPrimitiveSet<'a, 'b> {
    functions: &'a mut [(&'a str, AnyFn<'b>)],
    types: Vec<SchemeType>,
    values: Vec<Option<any_fn::Value>>,
}

impl<'a, 'b> DynamicPrimitiveSet<'a, 'b> {
    /// Creates a primitive set.
    pub fn new(functions: &'a mut [(&'a str, AnyFn<'b>)]) -> Self {
        let mut set = Self {
            functions,
            types: vec![],
            values: vec![],
        };

        // TODO Support more types including `()` and `Vec<u8>`.
        set.register_type::<bool>();
        set.register_type::<i8>();
        set.register_type::<u8>();
        set.register_type::<i16>();
        set.register_type::<u16>();
        set.register_type::<i32>();
        set.register_type::<u32>();
        set.register_type::<i64>();
        set.register_type::<u64>();
        set.register_type::<f32>();
        set.register_type::<f64>();
        set.register_type::<isize>();
        set.register_type::<usize>();
        set.register_type::<String>();

        set
    }

    /// Registers a type compatible between Scheme and Rust.
    ///
    /// Values of such types are automatically marshalled when we pass them from
    /// Scheme to Rust, and vice versa. Marshalling values can lead to the loss
    /// of information (e.g. floating-point numbers in Scheme marshalled
    /// into integers in Rust.)
    pub fn register_type<T: SchemeValue + 'static>(&mut self) {
        self.types.push((
            TypeId::of::<T>(),
            Box::new(|memory, value| T::from_scheme(memory, value).map(any_fn::value)),
            Box::new(|memory, value| T::into_scheme(value.downcast()?, memory)),
        ));
    }

    fn collect_garbages(&mut self, memory: &Memory) {
        let mut marks = bitvec![0; self.values.len()];

        for index in 0..(memory.allocation_index() / 2) {
            let cons = Cons::new((memory.allocation_start() + 2 * index) as _);

            if memory.cdr(cons).tag() != Type::Foreign as _ {
                continue;
            }

            let index = memory.car(cons).assume_number().to_i64() as _;

            // Be conservative as foreign type tags can be used for something else.
            if index >= self.values.len() {
                continue;
            }

            marks.set(index, true);
        }

        for (index, mark) in marks.into_iter().enumerate() {
            if !mark {
                self.values[index] = None;
            }
        }
    }

    // TODO Optimize this with `BitSlice::first_zero()`.
    fn find_free(&self) -> Option<usize> {
        self.values.iter().position(Option::is_none)
    }

    fn allocate(&mut self, memory: &Memory) -> usize {
        if let Some(index) = self.find_free() {
            index
        } else if let Some(index) = {
            self.collect_garbages(memory);
            self.find_free()
        } {
            index
        } else {
            self.values.push(None);
            self.values.len() - 1
        }
    }

    fn convert_from_scheme(
        &self,
        memory: &Memory,
        value: Value,
        type_id: TypeId,
    ) -> Option<any_fn::Value> {
        for (id, from, _) in &self.types {
            if type_id == *id {
                return from(memory, value);
            }
        }

        None
    }

    fn convert_into_scheme(
        &mut self,
        memory: &mut Memory,
        value: any_fn::Value,
    ) -> Result<Value, DynamicError> {
        for (id, _, into) in &self.types {
            if value.type_id()? == *id {
                return into(memory, value);
            }
        }

        let index = self.allocate(memory);

        self.values[index] = Some(value);

        Ok(memory
            .allocate(
                Number::from_i64(index as _).into(),
                memory.null().set_tag(Type::Foreign as _).into(),
            )?
            .into())
    }
}

impl PrimitiveSet for DynamicPrimitiveSet<'_, '_> {
    type Error = DynamicError;

    fn operate(&mut self, memory: &mut Memory, primitive: usize) -> Result<(), Self::Error> {
        if primitive == 0 {
            memory.set_register(memory.null());

            for (name, _) in self.functions.iter() {
                let list = memory.cons(memory.null().into(), memory.register())?;
                memory.set_register(list);
                let string = memory.build_string(name)?;
                memory.set_car(memory.register(), string.into());
            }

            memory.push(memory.register().into())?;

            Ok(())
        } else {
            let primitive = primitive - 1;
            let (_, function) = self
                .functions
                .get(primitive)
                .ok_or(Error::IllegalPrimitive)?;

            let mut arguments = (0..function.arity())
                .map(|_| memory.pop())
                .collect::<ArgumentVec<_>>();
            arguments.reverse();

            let cloned_arguments = {
                arguments
                    .iter()
                    .enumerate()
                    .map(|(index, &value)| {
                        self.convert_from_scheme(memory, value, function.parameter_types()[index])
                    })
                    .collect::<ArgumentVec<_>>()
            };

            let mut copied_arguments = ArgumentVec::new();

            for &value in &arguments {
                let value =
                    if value.is_cons() && memory.cdr_value(value).tag() == Type::Foreign as _ {
                        Some(
                            self.values
                                .get(memory.car_value(value).assume_number().to_i64() as usize)
                                .ok_or(DynamicError::ValueIndex)?
                                .as_ref()
                                .ok_or(DynamicError::ValueIndex)?,
                        )
                    } else {
                        None
                    };

                copied_arguments
                    .push(value)
                    .map_err(|_| Error::ArgumentCount)?;
            }

            let value = self
                .functions
                .get_mut(primitive)
                .ok_or(Error::IllegalPrimitive)?
                .1
                .call(
                    copied_arguments
                        .into_iter()
                        .enumerate()
                        .map(|(index, value)| {
                            cloned_arguments[index]
                                .as_ref()
                                .map_or_else(|| value.ok_or(DynamicError::ForeignValueExpected), Ok)
                        })
                        .collect::<Result<ArgumentVec<_>, DynamicError>>()?
                        .as_slice(),
                )?;

            let value = self.convert_into_scheme(memory, value)?;
            memory.push(value)?;

            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use any_fn::{r#fn, value, Ref};

    const HEAP_SIZE: usize = 1 << 8;

    struct Foo {
        bar: usize,
    }

    impl Foo {
        const fn new(bar: usize) -> Self {
            Self { bar }
        }

        const fn bar(&self) -> usize {
            self.bar
        }

        fn baz(&mut self, value: usize) {
            self.bar += value;
        }
    }

    fn invalidate_foreign_values(memory: &mut Memory) {
        for index in 0..(memory.size() / 2) {
            let cons = Cons::new((2 * index) as _);

            if memory.cdr(cons).tag() == Type::Foreign as _ {
                memory.set_car(cons, Number::from_i64(1 << 16).into());
            }
        }
    }

    #[test]
    fn create() {
        let mut functions = [
            ("make-foo", r#fn(Foo::new)),
            ("foo-bar", r#fn::<(Ref<_>,), _>(Foo::bar)),
            ("foo-baz", r#fn(Foo::baz)),
        ];

        DynamicPrimitiveSet::new(&mut functions);
    }

    #[test]
    fn allocate_two() {
        let mut heap = [Default::default(); HEAP_SIZE];
        let mut primitive_set = DynamicPrimitiveSet::new(&mut []);
        let mut memory = Memory::new(&mut heap).unwrap();

        let index = primitive_set.allocate(&memory);
        primitive_set.values[index] = Some(value(42usize));
        assert_eq!(index, 0);
        assert_eq!(primitive_set.find_free(), None);

        let cons = memory
            .allocate(
                Number::from_i64(index as _).into(),
                memory.null().set_tag(Type::Foreign as _).into(),
            )
            .unwrap();
        memory.push(cons.into()).unwrap();

        let index = primitive_set.allocate(&memory);
        primitive_set.values[index] = Some(value(42usize));
        assert_eq!(index, 1);
        assert_eq!(primitive_set.find_free(), None);
    }

    mod garbage_collection {
        use super::*;

        #[test]
        fn collect_none() {
            let mut heap = [Default::default(); HEAP_SIZE];
            let mut primitive_set = DynamicPrimitiveSet::new(&mut []);

            primitive_set.collect_garbages(&Memory::new(&mut heap).unwrap());
        }

        #[test]
        fn collect_one() {
            let mut heap = [Default::default(); HEAP_SIZE];
            let mut functions = [("make-foo", r#fn(|| Foo { bar: 42 }))];
            let mut primitive_set = DynamicPrimitiveSet::new(&mut functions);
            let mut memory = Memory::new(&mut heap).unwrap();

            primitive_set.operate(&mut memory, 1).unwrap();

            assert_eq!(primitive_set.find_free(), None);

            invalidate_foreign_values(&mut memory);

            primitive_set.collect_garbages(&memory);

            assert_eq!(primitive_set.find_free(), Some(0));
        }

        #[test]
        fn keep_one() {
            let mut heap = [Default::default(); HEAP_SIZE];
            let mut functions = [("make-foo", r#fn(|| Foo { bar: 42 }))];
            let mut primitive_set = DynamicPrimitiveSet::new(&mut functions);
            let mut memory = Memory::new(&mut heap).unwrap();

            primitive_set.operate(&mut memory, 1).unwrap();

            assert_eq!(primitive_set.find_free(), None);

            primitive_set.collect_garbages(&memory);

            assert_eq!(primitive_set.find_free(), None);
        }
    }
}
