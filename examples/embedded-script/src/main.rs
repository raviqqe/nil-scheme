//! The pie-throwing duel.

use any_fn::{r#fn, Ref};
use core::error::Error;
use rand::random;
use stak::{
    engine::{Engine, EngineError},
    include_module,
    module::UniversalModule,
};

const HEAP_SIZE: usize = 1 << 16;

/// A person who holds pies to throw.
struct Person {
    pies: usize,
    dodge: f64,
    wasted: bool,
}

impl Person {
    /// Creates a person.
    pub const fn new(pies: usize, dodge: f64) -> Self {
        Self {
            pies,
            dodge,
            wasted: false,
        }
    }

    /// Returns a number of pies the person has.
    pub const fn pies(&self) -> usize {
        self.pies
    }

    /// Returns `true` if a person is wasted.
    pub const fn wasted(&self) -> bool {
        self.wasted
    }

    /// Throws a pie to another person.
    pub fn throw_pie(&mut self, other: &mut Self) {
        if self.pies == 0 || self.wasted {
            return;
        }

        self.pies -= 1;

        if random::<f64>() > other.dodge {
            other.wasted = true;
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    // Import a Scheme module of the script.
    static MODULE: UniversalModule = include_module!("fight.scm");

    // Run the Scheme module.
    run_scheme(&MODULE)?;

    Ok(())
}

fn run_scheme(module: &'static UniversalModule) -> Result<(), EngineError> {
    // Initialize a heap memory for a Scheme scripting engine.
    let mut heap = [Default::default(); HEAP_SIZE];
    // Define Rust functions to pass to the engine.
    let mut functions = [
        ("make-person", r#fn(Person::new)),
        ("person-pies", r#fn::<(Ref<_>,), _>(Person::pies)),
        ("person-wasted", r#fn::<(Ref<_>,), _>(Person::wasted)),
        ("person-throw-pie", r#fn(Person::throw_pie)),
    ];
    // Initialize the engine.
    let mut engine = Engine::new(&mut heap, &mut functions)?;

    // Finally, run the module!
    engine.run(module)
}
