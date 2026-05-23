open! Basis

(** Unit tests targeting root causes of failing integration tests.

    The integration test failures fall into three categories:

    1. ctxLookup crash (Test 010 - cut_elim): Pattern match failure in
    IntSyn.ctxLookup when de Bruijn index exceeds context depth. Triggered
    during coverage checker's abstract + doubleCheck path.

    2. Approximate type matching (Test 008 - debruijn1): Apx.match_ raises
    "Type/kind expression clash" when approximate types have incompatible shapes
    during higher-order variable application reconstruction.

    3. Coverage missing cases (Tests 016, 021-024, 029-031): Coverage checker
    reports spurious "missing cases" involving parameter variables (#ovar). The
    splitting logic in cover_.ml (paramCases/constCases) may not enumerate all
    parameter variable instantiations correctly.

    These unit tests isolate each subsystem to pinpoint the exact failure
    conditions without needing full .elf file loading. *)

module I = Intsyn.Lambda_.IntSyn
module Whnf = Intsyn.Lambda_.Whnf
module Approx = Intsyn.Lambda_.Approx
module Abstract = Intsyn.Lambda_.Abstract

