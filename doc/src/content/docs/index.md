---
title: Miniature, embeddable Scheme
description: No-std and no-alloc Scheme implementation in Rust.
template: splash
hero:
  image:
    alt: Stak icon
    file: ../../../public/icon.svg
  tagline: No-std and no-alloc Scheme implementation in Rust.
  actions:
    - text: Examples
      link: examples/continuation
      icon: right-arrow
      variant: primary
---

# Stak

Stak is a no-`std` and no-`alloc` [Scheme][scheme] implementation in Rust.

This project is a fork of [Ribbit Scheme][ribbit], the tiny R4RS implementation.

## Goals

- Minimal implementation of [R7RS small][r7rs-small]
- Subset of [Chibi Scheme](https://github.com/ashinn/chibi-scheme), [Gauche](https://github.com/shirok/Gauche), and [Guile](https://www.gnu.org/software/guile/)
- Small memory footprint

## Limitations

- Numbers are 63-bit integers.
- Some runtime errors are not raised.
  - e.g. argument count and out-of-memory errors

## References

- [Ribbit Scheme][ribbit]
- [R7RS small][r7rs-small]
- [Scheme][scheme]

## License

[MIT](https://github.com/raviqqe/stak/blob/main/LICENSE)

[scheme]: https://www.scheme.org/
[r7rs-small]: https://small.r7rs.org/
[ribbit]: https://github.com/udem-dlteam/ribbit
