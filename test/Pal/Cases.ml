open Common

let nat_test =
  [ {| %sort nat |}; {| %term zero nat |}; {| %term succ {_ nat} nat |} ]

let add_test =
  [
    {| %sort add {_ nat} {_ nat} {_ nat} |};
    {| %term add/zero {y nat} add zero y y |};
    {| %term add/succ {x nat} {y nat} {z nat} {_ add x y z} add (succ x) y (succ z) |};
  ]

let mul_test =
  [
    {| %sort mul {_ nat} {_ nat} {_ nat} |};
    {| %term mul/zero {x nat} mul x zero zero |};
    {| %term mul/succ {x nat} {y nat} {z nat} {z' nat} {_ mul x y z} {_ add y z z'} (mul (succ x) y z') |};
  ]

let total_add_mul_test =
  [
    {| %mode {%in x nat} {%in y nat} {%out z nat} add x y z |};
    {| %worlds () (add _ _ _) |};
    {| %total N (add N _ _) |};
  ]

let cases () = Alcotest.run "PAL" begin [
  test "%term and %sort" Source.[
      String.concat "\n" nat_test;
      String.concat "\n" add_test;
      String.concat "\n" mul_test;
      String.concat "\n" total_add_mul_test;
    ];
    test "ZF" Source.[
      zf_1;
      zf_2;
      zf_3;
      zf_4;
      zf_5;
      zf_6;
    ];
    test "FOL" Source.[
      fol1;
      fol2;
      fol3_1;
      fol3_2_1;
      fol3_2_2;
      fol3_2_3;
      fol3_3;
      fol3_4;
      fol4_1;
      fol4_2;
      fol5_1;
      fol5_2;
      fol6_1;
      fol6_2;
    ];
    
    test "Nats" Source.[
      nats1;
      nats2;
      nats3;
      nats4;
    ];
    test "S4" Source.[
      jsf_1;
      jsf_2_1;
      jsf_2_2;
      jsf_3;
      jsf_4;
    ];
    test "LAM" Source.[
      lam_1;
      lam_2;
      lam_3;
      lam_4; 
      lam_5;
    ];
    test "POLYLAM" Source.[
      polylam;
    ];
    test "PROP-CALC" Source.[
      prop_calc_types;
      prop_calc_types ^ prop_calc_hilbert;
      prop_calc_types ^ prop_calc_hilbert ^ prop_calc_nd;
    ];
    test "MINI-ML" Source.[
      mini_ml_exp;
      mini_ml_value;
      mini_ml_tp;
    ];
    test "ARITH" Source.[
      arith_nat;
      arith_nt;
      arith_plus;
      arith_acker;
    ];
    test "GUIDE-LISTS" Source.[
      guide_lists_types;
      guide_lists_append;
      guide_lists_mode;
    ];
    test "TAPL-NAT" Source.[
      tapl_nat_base;
      tapl_nat_eq;
    ];
    test "LP-HORN-ND" Source.[
      lp_horn_nd;
    ];
    test "CHURCH-ROSSER-LAM" Source.[
      church_rosser_lam;
    ];
    test "CUT-ELIM-FORMULAS" Source.[
      cut_elim_formulas;
    ];
    test "GUIDE-ND" Source.[
      guide_nd;
    ];
    test "CPSOCC-DSBNF" Source.[
      cpsocc_dsbnf;
    ];
    test "SMALL-STEP-LAM" Source.[
      small_step_lam_types;
      small_step_lam_terms;
      small_step_lam_typing;
      small_step_lam_value;
      small_step_lam_step;
    ];
    test "CRARY-EXCON" Source.[
      crary_excon;
    ];
    test "CRARY-EXCON-REV" Source.[
      crary_excon_rev_syntax;
    ];
    test "TAPL-DEFS" Source.[
      tapl_defs_types;
      tapl_defs_labels;
      tapl_defs_exp;
      tapl_defs_value;
      tapl_defs_store;
      tapl_defs_heap;
    ];
    test "SMALL-STEP-SYSF" Source.[ 
      small_step_sysf_types;
      small_step_sysf_terms;
      small_step_sysf_typing;
      small_step_sysf_value;
      small_step_sysf_step;
    ];
    test "SMALL-STEP-SYSF-ISO" Source.[
      small_step_sysf_iso_types;
      small_step_sysf_iso_terms;
      small_step_sysf_iso_typing;
      small_step_sysf_iso_value;
      small_step_sysf_iso_step;
    ];
    test "POPLMARK-1A" Source.[
      poplmark_1a_syntax;
      poplmark_1b_syntax;
      poplmark_2b_syntax;
    ];
    (* POPLMARK-2A: `of`/`term`/`value` conflict with earlier suite declarations
       in the shared global state of the Pal frontend. Kept as failing test consistent
       with FOL-14, S4-1, LAM-1, POLYLAM-1, GUIDE-ND-1 pattern — runtime failure only.
    *)
    test "POPLMARK-2A" Source.[
      poplmark_2a_syntax;
    ];
    test "CCC" Source.[
      ccc_syntax;
    ];
    test "INCLL" Source.[
      incll_syntax;
    ];
    test "CRARY-LINEAR" Source.[
      crary_linear_syntax;
      crary_linear_linear;
    ];
    test "CRARY-LINEARD" Source.[
      crary_lineard_syntax;
    ];
    test "CRARY-MODAL" Source.[
      crary_modal_syntax;
    ];

] end