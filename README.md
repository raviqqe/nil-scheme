# Stak Scheme

[![GitHub Action](https://img.shields.io/github/actions/workflow/status/raviqqe/stak/test.yaml?branch=main&style=flat-square)](https://github.com/raviqqe/stak/actions)
[![Crate](https://img.shields.io/crates/v/stak.svg?style=flat-square)](https://crates.io/crates/stak)
[![Codecov](https://img.shields.io/codecov/c/github/raviqqe/stak.svg?style=flat-square)](https://codecov.io/gh/raviqqe/stak)
[![CodSpeed](https://img.shields.io/endpoint?url=https://codspeed.io/badge.json&style=flat-square)](https://codspeed.io/raviqqe/stak)
[![License](https://img.shields.io/github/license/raviqqe/stak.svg?style=flat-square)](https://github.com/raviqqe/stak/blob/main/LICENSE)

The miniature, embeddable R7RS Scheme implementation in Rust

Stak Scheme aims to be:

- An embeddable Scheme interpreter for Rust with very small memory footprint and reasonable performance
- The minimal implementation of [the R7RS-small standard][r7rs-small]
  - A subset of [Chibi Scheme](https://github.com/ashinn/chibi-scheme), [Gauche](https://github.com/shirok/Gauche), and [Guile](https://www.gnu.org/software/guile/)
- A portable scripting environment that supports even no-`std` and no-`alloc` platforms

For more information and usage, visit [the full documentation](https://raviqqe.github.io/stak/install).

## Install

### Interpreter

To install the Scheme interpreter as a command, run:

```sh
cargo install stak
```

### Libraries

To install Stak Scheme as a library in your Rust project, run:

```sh
cargo add stak
cargo add --build stak-build
cargo install stak-compile
```

For full examples, see [the `examples` directory](https://github.com/raviqqe/stak/tree/main/examples).

## Examples

### Dynamic scripting in Rust

First, prepare a Scheme script named `src/fight.scm`:

```scheme
; Import a base library and the library named `(stak rust)` for Rust integration.
(import (scheme base) (stak rust))

; Make two people with a number of pies they have and their dodge rates.
(define me (make-person 4 0.2))
(define friend (make-person 2 0.6))

; The fight begins. Let's throw pies to each other!
(do ()
  ((or
      (person-wasted me)
      (person-wasted friend)
      (and
        (zero? (person-pies me))
        (zero? (person-pies friend)))))
  (person-throw-pie me friend)
  (person-throw-pie friend me))

; Output the winner.
(write-string
  (cond
    ((person-wasted friend)
      "You won!")
    ((person-wasted me)
      "You lost...")
    (else
      "Draw...")))
```

Then, add a build script at `build.rs` to build the Scheme source file
into bytecodes.

```rust no_run
use stak_build::{build_r7rs, BuildError};

fn main() -> Result<(), BuildError> {
    build_r7rs()
}
```

Finally, you can embed and run the Scheme script in a Rust program.

```rust
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
    pub fn new(pies: usize, dodge: f64) -> Self {
        Self {
            pies,
            dodge,
            wasted: false,
        }
    }

    /// Returns a number of pies the person has.
    pub fn pies(&self) -> usize {
        self.pies
    }

    /// Returns `true` if a person is wasted.
    pub fn wasted(&self) -> bool {
        self.wasted
    }

    /// Throws a pie to another person.
    pub fn throw_pie(&mut self, other: &mut Person) {
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
```

## References

- This project is based on [Ribbit Scheme][ribbit], the small and portable R4RS implementation.
- [Scheme programming language][scheme]
- [The R7RS-small standard][r7rs-small]

## License

[MIT](https://github.com/raviqqe/stak/blob/main/LICENSE)

[scheme]: https://www.scheme.org/
[r7rs-small]: https://small.r7rs.org/
[ribbit]: https://github.com/udem-dlteam/ribbit
