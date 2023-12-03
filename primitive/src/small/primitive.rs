#[repr(u8)]
#[derive(Clone, Copy)]
pub(super) enum Primitive {
    Rib,
    Cons,
    Close,
    IsCons,
    Car,
    Cdr,
    Tag,
    SetCar,
    SetCdr,
    SetTag,
    Equal,
    LessThan,
    Add,
    Subtract,
    Multiply,
    Divide,
    Read,
    Write,
    WriteError,
    Halt,
    Type,
    SetType,
}

impl Primitive {
    pub const RIB: u8 = Self::Rib as _;
    pub const CONS: u8 = Self::Cons as _;
    pub const CLOSE: u8 = Self::Close as _;
    pub const IS_CONS: u8 = Self::IsCons as _;
    pub const CAR: u8 = Self::Car as _;
    pub const CDR: u8 = Self::Cdr as _;
    pub const TAG: u8 = Self::Tag as _;
    pub const SET_CAR: u8 = Self::SetCar as _;
    pub const SET_CDR: u8 = Self::SetCdr as _;
    pub const SET_TAG: u8 = Self::SetTag as _;
    pub const EQUAL: u8 = Self::Equal as _;
    pub const LESS_THAN: u8 = Self::LessThan as _;
    pub const ADD: u8 = Self::Add as _;
    pub const SUBTRACT: u8 = Self::Subtract as _;
    pub const MULTIPLY: u8 = Self::Multiply as _;
    pub const DIVIDE: u8 = Self::Divide as _;
    pub const READ: u8 = Self::Read as _;
    pub const WRITE: u8 = Self::Write as _;
    pub const WRITE_ERROR: u8 = Self::WriteError as _;
    pub const HALT: u8 = Self::Halt as _;
    pub const TYPE: u8 = Self::Type as _;
    pub const SET_TYPE: u8 = Self::SetType as _;
}
