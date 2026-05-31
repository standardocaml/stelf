open Common 


let nat_test =
  [
      {| %sort nat |} ;
      {| %term zero nat |} ;
      {| %term succ {_ nat} nat |} 
  ]
let add_test = [
      {| %sort add {_ nat} {_ nat} {_ nat} |} ;
      {| %term add/zero {y nat} add zero y y |} ;
      {| %term add/succ {x nat} {y nat} {z nat} {_ add x y z} add (succ x) y (succ z) |} ;
  ]
let mul_test = [
      {| %sort mul {_ nat} {_ nat} {_ nat} |} ;
      {| %term mul/zero {x nat} mul x zero zero |} ;
      {| %term mul/succ {x nat} {y nat} {z nat} {z' nat} {_ mul x y z} {_ add y z z'} (mul (succ x) y z') |} ;
  ]
let total_add_mul_test = [
      {| %mode {%in x nat} {%in y nat} {%out z nat} add x y z |} ;
      {| %worlds () (add _ _ _) |} ;
      {| %total N (add N _ _) |}
    ]
 
let cases () = 
  Alcotest.run "PAL" begin [
  "%term and %sort", [
    test "Natural Numbers (nat)" nat_test;  
    test "Natural Numbers (nat, add)"  (nat_test @ add_test);
    test "Natural Numbers (nat, add, mul)"  (nat_test @ add_test @ mul_test);
    
    test ~failure:true "Natural numbers (ill-formed)" [
      {| %sort nat |} ;
      {| %term zero nat |} ;
      {| %term succ {_ bool} nat |}
    ];
  ]; 
  "%total and friends", [
    test "Natural Numbers (total)" (nat_test @ add_test @ mul_test @ total_add_mul_test);
  ];
  "Wiki", [
      test "ZF 1" [
        Source.zf_core;
        ] ;
      test "ZF 2" [
        Source.zf_core;
        Source.zf_basics;
       
      ] ;
      test "ZF 3" [
        Source.zf_core;
        Source.zf_basics;
        Source.zf_def_basic;
      ] ;
      test "ZF 4" [
        Source.zf_core;
        Source.zf_basics;
        Source.zf_def_basic;
        Source.zf_high;   
      ]
  ]

 
] end 

 