# Building STELF

This document is intended to be a guide for building STELF from source.

[!WARNING] Make sure that you clone this using either `--recurse-submodules` or `--recursive`, as to capture the basis submodule

## Prerequisites

For the actual project, dependency management is handled by Dune, so you *should* only need [opam](https://opam.ocaml.org/) 

## Building

To build the project, simply run `dune build` in the root directory. This will build all of the libraries and executables in the project.
To build the documentation, run `dune build @doc`. This will build the documentation for all of the libraries and executables in the project.
To run the tests, run `dune runtest`. This will run all of the tests in the project.