open! Basis

let test_case =
  Regression_case.test ~title:"examples/alloc_sem/sources.cfg"
    "examples/alloc_sem/sources.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/arith/test.cfg"
    "examples/arith/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Cartesian closed categories with environments"
    "examples/ccc/sources.cfg"
open! Basis

let test_case =
  Regression_case.test
    ~title:"Church-Rosser theorem for untyped lambda calculus"
    "examples/church_rosser/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Compilation: CLS machine (SECD variant)"
    "examples/compile/cls/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Compilation: Continuation passing machine"
    "examples/compile/cpm/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Compilation: CPS transformation"
    "examples/compile/cps/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Compilation: Expression-value machine"
    "examples/compile/cxm/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Compilation: De Bruijn indices (variant)"
    "examples/compile/debruijn1/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Compilation: De Bruijn index translation"
    "examples/compile/debruijn/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"CPS transformation correctness"
    "examples/cpsocc/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Explicit contexts in LF"
    "examples/crary/explicit/excon.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Explicit contexts in LF (reversed)"
    "examples/crary/explicit/excon-rev.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"ML definitional equivalence (alpha)"
    "examples/crary/mldef-alpha/sources.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"ML definitional equivalence (singleton)"
    "examples/crary/mldef-alpha/sing/sources.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Call-by-value lambda calculus semantics"
    "examples/crary/standard/standard.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Substructural logic: Linear with dependencies"
    "examples/crary/substruct/lineard.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Substructural logic: Linear types"
    "examples/crary/substruct/linear.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Substructural logic: Modal types"
    "examples/crary/substruct/modal.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Syntactic singleton kinds"
    "examples/crary/synsing/sources.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Typed singleton LF"
    "examples/crary/tslf/sources.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Cut elimination for classical sequent calculus"
    "examples/cut_elim/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Expected failure: unsatisfiable query"
    ~success:false "examples/failure/sources.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/fj/sources.cfg"
    "examples/fj/sources.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Intuitionistic propositional logic (fol)"
    "examples/fol/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~skip:true ~title:"Stelf User's Guide examples"
    "examples/guide/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"First-order logic (Handbook of AR)"
    "examples/handbook/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Intuitionistic linear logic"
    "examples/incll/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/js4/sources.cfg"
    "examples/js4/sources.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Kolmogorov translation (double-negation)"
    "examples/kolm/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Logic programming: Horn clause fragment"
    "examples/lp_horn/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~unsafe:true
    ~title:"Logic programming: canonical forms (unsafe)" "examples/lp/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Mini-ML type system and evaluation"
    "examples/mini_ml/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Polymorphic lambda calculus (System F)"
    "examples/polylam/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"Propositional calculus (Hilbert vs ND)"
    "examples/prop_calc/test.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tabled/ccc/tab.cfg"
    "examples/tabled/ccc/tab.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tabled/cr/tab.cfg"
    "examples/tabled/cr/tab.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tabled/mini_ml/tab.cfg"
    "examples/tabled/mini_ml/tab.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tabled/parsing/tab.cfg"
    "examples/tabled/parsing/tab.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tabled/poly/tab.cfg"
    "examples/tabled/poly/tab.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tabled/refine/tab.cfg"
    "examples/tabled/refine/tab.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tabled/seqCalc/sources.cfg"
    "examples/tabled/seqCalc/sources.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tabled/subtype1/tab.cfg"
    "examples/tabled/subtype1/tab.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tabled/subtype/tab.cfg"
    "examples/tabled/subtype/tab.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tabled/tests/tab.cfg"
    "examples/tabled/tests/tab.cfg"
open! Basis

let test_case =
  Regression_case.test ~title:"examples/tapl_ch13/sources.cfg"
    "examples/tapl_ch13/sources.cfg"
