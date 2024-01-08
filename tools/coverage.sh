#!/bin/sh

set -e

rustup component add llvm-tools-preview

cargo install cargo-llvm-cov
cargo llvm-cov --workspace --profile release_test --lcov --output-path lcov.info
