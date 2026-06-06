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
          [
            test "Natural Numbers (nat)" nat_test;
            test "Natural Numbers (nat, add)" (nat_test @ add_test);
            test "Natural Numbers (nat, add, mul)"
              (nat_test @ add_test @ mul_test);
            test ~failure:true "Natural numbers (ill-formed)"
              [
                {| %sort nat |};
                {| %term zero nat |};
                {| %term succ {_ bool} nat |};
              ];
          ] );
        ( "%total and friends",
          [
            test "Natural Numbers (total)"
              (nat_test @ add_test @ mul_test @ total_add_mul_test);
          ] );
        ( "Wiki",
          [
            test "ZF 1" [ Source.zf_core ];
            test "ZF 2" [ Source.zf_core; Source.zf_basics ];
            test "ZF 3"
              [ Source.zf_core; Source.zf_basics; Source.zf_def_basic ];
            test "ZF 4"
              [
                Source.zf_core;
                Source.zf_basics;
                Source.zf_def_basic;
                Source.zf_high;
              ];
          ] );
        ( "FOL", Source.[ 
          test "FOL" [ fol1 ]; 
          test "FOL2" [ fol1; fol2 ] ;
          test "FOL3.1" [ fol1; fol2; fol3_1 ];
          test "FOL3.2.1" [ fol1; fol2; fol3_1; fol3_2_1 ];
          test "FOL3.2.2" [ fol1; fol2; fol3_1; fol3_2_1; fol3_2_2 ];
          test "FOL3.2.3" [ fol1; fol2; fol3_1; fol3_2_1; fol3_2_2; fol3_2_3 ];
          test "FOL3.2.*" [ fol1; fol2; fol3_1; fol3_2 ];
          test "FOL3.3" [ fol1; fol2; fol3_1; fol3_2; fol3_3 ];
          test "FOL3.4" [ fol1; fol2; fol3_1; fol3_2; fol3_3; fol3_4 ]; 
          test "FOL3.*" [ fol1; fol2; fol3 ];
          test "FOL4.1" [ fol1; fol2; fol3; fol4_1 ]; 
          test "FOL4.2" [ fol1; fol2; fol3; fol4_1; fol4_2 ]; 
          test "FOL4.*" [ fol1; fol2; fol3; fol4 ]; 
          test "FOL5" [ fol1; fol2; fol3; fol4; fol5 ];
          test "FOL6" [ fol1; fol2; fol3; fol4; fol5; fol6 ]
        ] );
        ( "S4", [ test "S4" [ Source.js4 ] ] );
        ( "LAM", [ test "LAM" [ Source.lam ] ] );
        ( "POLYLAM", [ test "POLYLAM" [ Source.polylam ] ] )
      ]
    end ~verbose:false
