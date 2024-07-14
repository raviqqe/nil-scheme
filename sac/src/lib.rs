//! Utilities to build executable binaries from bytecode files.

#[doc(hidden)]
pub mod __private {
    pub use clap;
    pub use main_error;
    pub use stak_configuration;
    pub use stak_device;
    pub use stak_file;
    pub use stak_macro;
    pub use stak_primitive;
    pub use stak_process_context;
    pub use stak_vm;
    pub use std;
}

/// Defines a `main` function that executes a source file at a given path.
///
/// The given source file is compiled into bytecodes and bundled into a
/// resulting binary.
#[macro_export]
macro_rules! main {
    ($path:expr) => {
        $crate::main!(
            $path,
            $crate::__private::stak_configuration::DEFAULT_HEAP_SIZE
        );
    };
    ($path:expr, $heap_size:expr) => {
        use $crate::__private::{
            clap::{self, Parser},
            main_error::MainError,
            stak_device::StdioDevice,
            stak_file::OsFileSystem,
            stak_macro::include_r7rs,
            stak_primitive::SmallPrimitiveSet,
            stak_process_context::OsProcessContext,
            stak_vm::Vm,
            std::{env, error::Error},
        };

        #[derive(clap::Parser)]
        #[command(about, version)]
        struct Arguments {
            #[arg()]
            arguments: Vec<String>,
            #[arg(short = 's', long, default_value_t = $heap_size)]
            heap_size: usize,
        }

        fn main() -> Result<(), MainError> {
            let arguments = Arguments::parse();

            let mut heap = vec![Default::default(); arguments.heap_size];
            let mut vm = Vm::new(
                &mut heap,
                SmallPrimitiveSet::new(
                    StdioDevice::new(),
                    OsFileSystem::new(),
                    OsProcessContext::new(),
                ),
            )?;

            vm.initialize(include_r7rs!($path).iter().copied())?;

            Ok(vm.run()?)
        }
    };
}

/// Defines a `main` function that executes a source file at a given path.
///
/// The given source file is compiled into bytecodes and bundled into a
/// resulting binary.
#[macro_export]
macro_rules! libc_main {
    ($path:expr) => {
        $crate::main!(
            $path,
            $crate::__private::stak_configuration::DEFAULT_HEAP_SIZE
        );
    };
    ($path:expr, $heap_size:expr) => {
        #![no_std]
        #![cfg_attr(not(test), no_main)]

        use mstak_util::Mmap;
        use $crate::__private::{
            clap::{self, Parser},
            core::{ffi::CStr, mem::size_of, slice},
            main_error::MainError,
            stak_device::libc::{ReadWriteDevice, Stderr, Stdin, Stdout},
            stak_file::LibcFileSystem,
            stak_macro::include_r7rs,
            stak_primitive::SmallPrimitiveSet,
            stak_process_context::VoidProcessContext,
            stak_vm::Vm,
            std::{env, error::Error},
        };

        #[cfg_attr(not(test), no_mangle)]
        unsafe extern "C" fn main(argc: isize, argv: *const *const i8) -> isize {
            let Some(&file) = &slice::from_raw_parts(argv, argc as _).get(1) else {
                return 1;
            };

            let heap = slice::from_raw_parts_mut(
                libc::malloc(size_of::<Value>() * HEAP_SIZE) as *mut Value,
                HEAP_SIZE,
            );

            let mut vm = Vm::new(
                heap,
                SmallPrimitiveSet::new(
                    ReadWriteDevice::new(Stdin::new(), Stdout::new(), Stderr::new()),
                    LibcFileSystem::new(),
                    VoidProcessContext::new(),
                ),
            )
            .unwrap();

            let mmap = Mmap::new(CStr::from_ptr(file));

            vm.initialize(mmap.as_slice().iter().copied()).unwrap();
            vm.run().unwrap();

            0
        }
    };
}
