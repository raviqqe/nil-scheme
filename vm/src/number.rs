use core::fmt::{self, Display, Formatter};

#[repr(transparent)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Number(u64);

impl Number {
    pub const fn new(number: u64) -> Self {
        Self(number)
    }

    #[allow(dead_code)]
    pub const fn to_u64(self) -> u64 {
        self.0
    }
}

impl Display for Number {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "n{}", self.0)
    }
}
