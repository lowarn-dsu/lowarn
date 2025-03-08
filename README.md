# Lowarn

> Practical dynamic software updating for Haskell

Lowarn is a dynamic software updating system for Haskell.

## Packages

- [lowarn](core) – types and functions for interacting with Lowarn
- [lowarn-runtime](runtime) – Lowarn's runtime system
- [lowarn-cli](cli) – CLI for using Lowarn
- [lowarn-transformer](transformer) – utilities for defining type-driven state transformers
- [lowarn-inject](inject) – runtime data injection for versions of programs
- [lowarn-arbitrary](arbitrary) – orphaned instances of QuickCheck's `Arbitrary` type class
- [lowarn-aeson](aeson) – orphaned instances of Aeson's `ToJson` and `FromJson` type classes
- [lowarn-test](test) – testing utilities and non-doctest parts of Lowarn's test suite
- [lowarn-test-types](test-types) – types and orphaned instances used by [lowarn-test](test) for testing [lowarn-transformer](transformer)
