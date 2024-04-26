use crate::error::Error;
use core::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

/// A record type.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RecordType {
    /// A call.
    Call,
    /// A return.
    Return,
    /// A return (tail) call.
    ReturnCall,
}

impl Display for RecordType {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self {
            Self::Call => write!(formatter, "call"),
            Self::Return => write!(formatter, "return"),
            Self::ReturnCall => write!(formatter, "return_call"),
        }
    }
}

impl FromStr for RecordType {
    type Err = Error;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        match string {
            "call" => Ok(Self::Call),
            "return" => Ok(Self::Return),
            "return_call" => Ok(Self::ReturnCall),
            _ => Err(Error::UnknownRecordType),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(RecordType::Call.to_string(), "call");
        assert_eq!(RecordType::Return.to_string(), "return");
        assert_eq!(RecordType::ReturnCall.to_string(), "return_call");
    }

    #[test]
    fn test_from_str() {
        assert_eq!("call".parse(), Ok(RecordType::Call));
        assert_eq!("return".parse(), Ok(RecordType::Return));
        assert_eq!("return_call".parse(), Ok(RecordType::ReturnCall));
        assert_eq!(
            "unknown".parse::<RecordType>(),
            Err(Error::UnknownRecordType)
        );
    }
}
