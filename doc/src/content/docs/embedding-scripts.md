---
title: Embedding Scheme scripts in Rust
description: How to embed Scheme scripts in Rust
---

This page explains how to embed scripts written in Stak Scheme into Rust programs. By reading this page, you will learn:

- How to compile Scheme scripts into bytecodes in Rust crates to embed the scripts.
- How to embed and run scripts written in Stak Scheme in Rust programs.

The full source codes used in this guide is available at the [`examples/embedded-script` directory on GitHub][source]

## Installing dependencies

First, follow [the install instruction](install#libraries) to add Stak Scheme as dependencies in your Rust crate.

In addition, to use native functions written in Rust in Scheme scripts, install [the `any-fn` crate](https://crates.io/crates/any-fn) which converts statically-typed functions in Rust into dynamically-typed functions by running the following command.

```sh
cargo add any-fn
```

## Preparing a Scheme script

First, prepare a Scheme script named `src/fight.scm` as follows.

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

The main part for Rust integration is importing the `(stak rust)` library which imports and defines native functions in Rust at the top level.

## Adding a build script

Then, add a build script at `build.rs` to build the Scheme source file into bytecodes.

The bytecodes are compact codes that describe programs written in Scheme. It is just like machine codes but for a Stak Scheme virtual machine. The bytecodes compiled from source files are stored in [the `target` directory](https://doc.rust-lang.org/cargo/reference/build-cache.html) in your crate.

```rust no_run
use stak_build::{build_r7rs, BuildError};

fn main() -> Result<(), BuildError> {
    build_r7rs()
}
```

## Embedding a Scheme script in a Rust program

Now, you can embed and run the Scheme script in a Rust program.

First, define a `Person` data structure and its associated functions.

```rust
use rand::random;

// Define a person data structure and its associated functions which you include
// into the Scheme script.

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

        // The other person dodges a pie at a certain rate.
        if random::<f64>() > other.dodge {
            other.wasted = true;
        }
    }
}
```

Secondly, define a `run_scheme` function which runs a Scheme program.

```rust
use any_fn::{r#fn, Ref};
use stak::{
    engine::{Engine, EngineError},
    module::UniversalModule,
};

const HEAP_SIZE: usize = 1 << 16;

fn run_scheme(module: &'static UniversalModule) -> Result<(), EngineError> {
    // Initialize a heap memory for a Scheme scripting engine.
    let mut heap = [Default::default(); HEAP_SIZE];
    // Define Rust functions to pass to the engine.
    let mut functions = [
        r#fn(Person::new),
        r#fn::<(Ref<_>,), _>(Person::pies),
        r#fn::<(Ref<_>,), _>(Person::wasted),
        r#fn(Person::throw_pie),
    ];
    // Initialize the engine.
    let mut engine = Engine::new(&mut heap, &mut functions)?;

    // Finally, run the module!
    engine.run(module)
}
```

Finally, call the `run_scheme` function in the `main` function.

```rust
use core::error::Error;
use stak::{
    include_module,
    module::UniversalModule,
};

fn main() -> Result<(), Box<dyn Error>> {
    // Import a Scheme module of the script.
    static MODULE: UniversalModule = include_module!("fight.scm");

    // Run the Scheme module.
    run_scheme(&MODULE)?;

    Ok(())
}
```

When you run the crate with the following command, you will see a result of the fight changing every time you run the command.

```sh
cargo run # -> You won!
```

## References

- [`examples/embedded-script` directory on GitHub][source]

[source]: https://github.com/raviqqe/stak/tree/main/examples/embedded-script
