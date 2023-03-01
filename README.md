# Lowarn

> Practical dynamic software updating for Haskell

Lowarn is a **work-in-progress** dynamic software updating system for Haskell.

## Packages

- [lowarn](core) – types and functions for interacting with Lowarn
- [lowarn-runtime](runtime) - Lowarn's runtime system
- [lowarn-transformer](transformer) – utilities for defining type-driven state transformers
- [lowarn-inject](inject) - runtime data injection for versions of programs
- [lowarn-arbitrary](arbitrary) – orphaned instances of QuickCheck's `Arbitrary` typeclass
- [lowarn-test](test) – testing utilities and non-doctest parts of Lowarn's test suite
- [lowarn-test-types](test-types) – types and orphaned instances used by [lowarn-test](test) for testing [lowarn-transformer](transformer)
