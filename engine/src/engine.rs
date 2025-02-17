use crate::{primitive_set::EnginePrimitiveSet, EngineError};
use any_fn::AnyFn;
use stak_dynamic::SchemeValue;
use stak_module::Module;
use stak_vm::{Error, Value, Vm};

/// A scripting engine.
pub struct Engine<'a, 'b> {
    vm: Vm<'a, EnginePrimitiveSet<'a, 'b>>,
}

impl<'a, 'b> Engine<'a, 'b> {
    /// Creates a scripting engine.
    pub fn new(
        heap: &'a mut [Value],
        functions: &'a mut [(&'a str, AnyFn<'b>)],
    ) -> Result<Self, Error> {
        Ok(Self {
            vm: Vm::new(heap, EnginePrimitiveSet::new(functions))?,
        })
    }

    /// Runs a module.
    pub fn run<'c>(&mut self, module: &'c impl Module<'c>) -> Result<(), EngineError> {
        self.vm.initialize(module.bytecode().iter().copied())?;
        self.vm.run()
    }

    /// Registers a type compatible between Scheme and Rust.
    ///
    /// We register all types that this crate implements [`SchemeValue`] for to
    /// the engines by default.
    ///
    /// For more information, see
    /// [`DynamicPrimitiveSet`][stak_dynamic::DynamicPrimitiveSet].
    pub fn register_type<T: SchemeValue + 'static>(&mut self) {
        self.vm
            .primitive_set_mut()
            .dynamic_mut()
            .register_type::<T>()
    }
}
