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

/// A number of bits required to encode an instruction in bytecodes.
pub const INSTRUCTION_BITS: u64 = 4;
/// A mask for instruction bits in bytecodes.
pub const INSTRUCTION_MASK: u8 = (1 << INSTRUCTION_BITS) - 1;
/// A base for integer encoding in bytecodes.
pub const INTEGER_BASE: u64 = i8::MAX as u64 + 1;
/// A base for short integer encoding in bytecodes.
pub const SHORT_INTEGER_BASE: u64 = 1 << (8 - INSTRUCTION_BITS - 1);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mask() {
        assert_eq!(INSTRUCTION_MASK, 0b1111);
    }
}
