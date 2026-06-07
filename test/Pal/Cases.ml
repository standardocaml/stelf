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

let cases () =
  Alcotest.run "PAL"
    begin
      [
        ( "%term and %sort",
          List.concat
            [
              snd (test "Natural Numbers (nat)" nat_test);
              snd (test "Natural Numbers (nat, add)" (nat_test @ add_test));
              snd
                (test "Natural Numbers (nat, add, mul)"
                   (nat_test @ add_test @ mul_test));
              snd
                (test ~failure:true "Natural numbers (ill-formed)"
                   [
                     {| %sort nat |};
                     {| %term zero nat |};
                     {| %term succ {_ bool} nat |};
                   ]);
            ] );
        ( "%total and friends",
          List.concat
            [
              snd
                (test "Natural Numbers (total)"
                   (nat_test @ add_test @ mul_test @ total_add_mul_test));
            ] );
        ( "Wiki",
          List.concat
            [
              snd (test "ZF 1" [ Source.zf_core ]);
              snd (test "ZF 2" [ Source.zf_core; Source.zf_basics ]);
              snd
                (test "ZF 3"
                   [ Source.zf_core; Source.zf_basics; Source.zf_def_basic ]);
              snd
                (test "ZF 4"
                   [
                     Source.zf_core;
                     Source.zf_basics;
                     Source.zf_def_basic;
                     Source.zf_high;
                   ]);
            ] );
        ( "Nats",
          List.concat
            [
              snd (test "Nats 1" [ Source.nats1 ]);
              snd (test "Nats 2" [ Source.nats1; Source.nats2 ]);
              snd (test "Nats 3" [ Source.nats1; Source.nats2; Source.nats3 ]);
              snd (test "Nats 4" [ Source.nats1; Source.nats2; Source.nats3; Source.nats4 ]);
            ] );
        ( "FOL",
          List.concat
            [
              snd (test "FOL" [ Source.fol1 ]);
              snd (test "FOL2" [ Source.fol1; Source.fol2 ]);
              snd (test "FOL3.1" [ Source.fol1; Source.fol2; Source.fol3_1 ]);
              snd (test "FOL3.2.1" [ Source.fol1; Source.fol2; Source.fol3_1; Source.fol3_2_1 ]);
              snd (test "FOL3.2.2" [ Source.fol1; Source.fol2; Source.fol3_1; Source.fol3_2_1; Source.fol3_2_2 ]);
              snd
                (test "FOL3.2.3" [ Source.fol1; Source.fol2; Source.fol3_1; Source.fol3_2_1; Source.fol3_2_2; Source.fol3_2_3 ]);
              snd (test "FOL3.2.*" [ Source.fol1; Source.fol2; Source.fol3_1; Source.fol3_2 ]);
              snd (test "FOL3.3" [ Source.fol1; Source.fol2; Source.fol3_1; Source.fol3_2; Source.fol3_3 ]);
              snd (test "FOL3.4" [ Source.fol1; Source.fol2; Source.fol3_1; Source.fol3_2; Source.fol3_3; Source.fol3_4 ]);
              snd (test "FOL3.*" [ Source.fol1; Source.fol2; Source.fol3 ]);
              snd (test "FOL4.1" [ Source.fol1; Source.fol2; Source.fol3; Source.fol4_1 ]);
              snd (test "FOL4.2" [ Source.fol1; Source.fol2; Source.fol3; Source.fol4_1; Source.fol4_2 ]);
              snd (test "FOL4.*" [ Source.fol1; Source.fol2; Source.fol3; Source.fol4 ]);
              snd (test "FOL5" [ Source.fol1; Source.fol2; Source.fol3; Source.fol4; Source.fol5 ]);
              snd (test "FOL6" [ Source.fol1; Source.fol2; Source.fol3; Source.fol4; Source.fol5; Source.fol6 ]);
            ] );
        ( "S4", List.concat [ snd (test "S4" [ Source.js4 ]) ] );
        ( "LAM", List.concat [ snd (test "LAM" [ Source.lam ]) ] );
        ( "POLYLAM", List.concat [ snd (test "POLYLAM" [ Source.polylam ]) ] )
      ]
    end ~verbose:false 
let trash _ = " %. "
let cases () = Alcotest.run "PAL" begin [
  test "%term and %sort" Source.[
      String.concat "\n" nat_test;
      String.concat "\n" add_test;
      String.concat "\n" mul_test;
      String.concat "\n" total_add_mul_test;
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
    test "ZF" Source.[
      zf_core;
      zf_basics;
      zf_def_basic;
      zf_high;
    ];
    test "Nats" Source.[
      nats1;
      nats2;
      nats3;
      nats4;
    ];
    test "S4" Source.[
      js4;
    ];
    test "LAM" Source.[
      lam;
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