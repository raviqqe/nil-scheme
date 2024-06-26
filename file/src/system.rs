use crate::FileDescriptor;
use core::fmt::Debug;

/// A file system.
pub trait FileSystem {
    type Error: Debug;

    /// Opens a file.
    fn open(&self, path: &[u8], output: bool) -> Result<FileDescriptor, Self::Error>;

    /// Closes a file.
    fn close(&self, descriptor: FileDescriptor) -> Result<(), Self::Error>;

    /// Reads a file.
    fn read(&self, descriptor: FileDescriptor) -> Result<u8, Self::Error>;

    /// Writes a file.
    fn write(&self, descriptor: FileDescriptor, byte: u8) -> Result<(), Self::Error>;

    /// Deletes a file.
    fn delete(&self, path: &[u8]) -> Result<(), Self::Error>;

    /// Checks if a file exists.
    fn exists(&self, path: &[u8]) -> Result<bool, Self::Error>;
}
