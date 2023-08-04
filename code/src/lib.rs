#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc;
#[cfg(any(feature = "std", test))]
extern crate std;

#[cfg(feature = "alloc")]
mod decode;
#[cfg(feature = "alloc")]
mod encode;
#[cfg(feature = "alloc")]
mod error;
mod ir;

#[cfg(feature = "alloc")]
pub use decode::decode;
#[cfg(feature = "alloc")]
pub use encode::encode;
#[cfg(feature = "alloc")]
pub use error::Error;
pub use ir::*;

pub const INTEGER_BASE: u64 = i8::MAX as u64 + 1;
pub const INSTRUCTION_MASK: u8 = 0b1111;
pub const INSTRUCTION_BITS: u64 = INSTRUCTION_MASK.count_ones() as u64;
pub const SHORT_INTEGER_BASE: u64 = 1 << (8 - INSTRUCTION_BITS - 1);
