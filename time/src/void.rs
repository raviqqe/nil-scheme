use crate::Clock;

/// A void time that provides no context information.
#[derive(Debug, Default)]
pub struct VoidTime {}

impl VoidTime {
    /// Creates a time.
    pub const fn new() -> Self {
        Self {}
    }
}

impl Clock for VoidTime {
    fn command_line_rev(&self) -> impl IntoIterator<Item = &str> {
        []
    }

    fn environment_variables(&self) -> impl IntoIterator<Item = (&str, &str)> {
        []
    }
}
