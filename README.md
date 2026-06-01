# STELF (System for Totality in the Edinburgh Logical Framework)

This is the STELF project, a port of the Twelf system to OCaml, and subsequents developments thereof.

## Twelf Port 

The Twelf port, which is completed, involved translating between SML to OCaml, and also the creation of a temporary basis library.
If you are interested in looking at the process, see [shibboleth](https://github.com/standardocaml/shibboleth) 

## From Twelf to STELF

The STELF project, which is still in devolopment, involves a number of changes to the original Twelf codebase, designed to do the following:

- Imporove the syntax
  - Remove unnecassary special syntax (Parsing done, printing not started)
  - Simplify the language
  - Make the codebase trivial to parse for editor integration
  - Overhaul the parser (Nearly done)
- Increase performance (WIP)
- Make the codebase more flexible, particular in regards to custom frontends 
  - Changed the design of the concrete syntax tree to not depend on the actual concrete syntax, but instead be view based (CST is done, but integration is not)
  - Make the codebase more modular, and break up larger modules / libraries into smaller ones (WIP)
  - Make documentation consistent (WIP)
  - Testing
- Create a cleaner frontend 
  - Internally, the frontend is exposed at one point (Mostly done)
  - Output should be given and then dealt with, not done internally (Not started)
  - Create a nicer REPL (Done)
  - Create a nicer CLI (Done)

Heavy inspiration was taken from the following sources:

- Rocq (interlopability) 
- Z3 theorem prover (language design)
- Metamath (minimalism)

## Documentation

Documentation is very incomplete, but the following resources are available:
- The dev docs are not yet available, but running `source hacking.sh BROWSER` where `BROWSER` is your browser should (propably (on Linux (maybe ))) open the dev docs (hopefully). 
  Dev docs are not done
- [The wiki](https://github.com/standardocaml/stelf/wiki) contains some documentation, but is also incomplete.
- The STELF book, which is intended to be both a manual and reference, is located in [the book directory](./guide/). It is also incomplete, and as a little experiment is written in Typst.
- Some other useful links include:
  - Original Twelf [repo](https://github.com/standardml/twelf) and [website](https://twelf.org/)
  - The [Shibboleth transpiler](https://github.com/standardocaml/shibboleth)
  - A certain [paper](https://www.cs.cmu.edu/~rwh/papers/mech/jfp07.pdf) describing canonical LF
  - [Building](./hacking/BUILD.md), which is hopefully up to date 

If you couldn't tell, this project is a tad large.
If I miss something, please reach out. 

## Original README

Copyright (C) 1997-2011, Frank Pfenning and Carsten Schuermann

Authors:

    Frank Pfenning
    Carsten Schuermann

With contributions by:

    Brigitte Pientka
    Roberto Virga
    Kevin Watkins
    Jason Reed

## STELF

Copyright (C) 2026, Asher Frost
