let zf_core = {|

%sort prop
%sort pf {_ prop}
%sort set
|}

let zf_basics =
  {|
%term false prop
%term imp {_ prop} {_ prop} prop
%term all {_ {_ set} prop} prop 
%term eq {_ set} {_ set} prop
%term in {_ set} {_ set} prop
|}

let zf_def_basic =
  {|
%def not ({_ prop} prop) ([a] imp a false) %.
%def and ({_ prop} {_ prop} prop) ([a][b] not (imp a (not b))) %.
%def or ({_ prop} {_ prop} prop) ([a][b] imp (not a) b) %.
%def iff ({_ prop} {_ prop} prop) ([a][b] and (imp a b) (imp b a)) %.
%def ex ({_ {_ set} prop} prop) ([p] not(all([z]not (p z)))) %.
%def unique ({_ {_ set} prop} prop) ([p] all([z] imp (p z) (all ([z'] imp (p z') (eq z z'))))) %.
|}

let zf_high =
  {|
%def ex_unique ({_ {_ set} prop} prop) ([p] and (ex p) (unique p)) %.
%term imp_i {_ {_ pf _A} pf _B} pf (imp _A _B) %.
%term imp_e {_ pf (imp _A _B)} {_ pf _A} pf _B %.
%term all_i {_ {z} pf (_P z)} pf (all _P) %.
%term all_e {_ pf (all _P)} {z set} pf (_P z) %.
%term classical {_ pf (not(not _A))} pf _A %.
%term eq_i pf (eq _A _A) %.
%term eq_e {_ pf (eq _A _B)} {s {_ set} prop} {_ pf (s _A)} pf (s _B) %.
%term if {_ prop} {_ set} {_ set} set
%term if_then {_ pf _P} pf (eq (if _P _X _Y) _X) %.
%term if_else {_ pf (not _P)} pf (eq (if _P _X _Y) _Y) %.
%term empty    set %.
%term double   {_ set} {_ set} set %.
%term unions   {_ set} set %.
%term powerset {_ set} set %.
%term replace  {_ set} {_ {_ set} set} set %.
%term omega    set %.
%def single ({_ set} set) [x] double x x %.
%def restrict ({_ set} {_ {_ set} prop} set) [x][q] unions (replace x ([z] if (q z) (single z) empty)) %.
%def inter ({_ set} {_ set} set) [x][y] restrict x ([z] in z y) %.
%def union ({_ set} {_ set} set) [x][y] unions (double x y) %.
%def zero set empty %.
%def succ ({_ set} set) [x] union x (single x) %.
%def subset ({_ set} {_ set} prop) [x][y]all[z] imp (in z x) (in z y) %.
%def disjoint ({_ set} {_ set} prop) [x][y] eq (inter x y) empty %.
%def omega_closed ({_ set} prop) [x] and (in empty x) (all [n] imp (in n x) (in (succ n) x)) %.
%term extensionality pf (iff (eq _X _Y) (all[z] iff (in z _X) (in z _Y))) %.
%term foundation     pf (ex([z] and (in z _X) (disjoint z _X))) %.
%term emtpy_ax       pf (not (in _X empty)) %.
%term double_ax      pf (iff (in _Z (double _X _Y)) (or (in _Z _X) (in _Z _Y))) %.
%term union_ax       pf (iff (in _Z (unions _X)) (ex[y] and (in _Z y) (in y _X))) %.
%term powerset_ax    pf (iff (in _Z (powerset _X)) (subset _Z _X)) %.
%term replace_ax     pf (iff (in _Z (replace _X F)) (ex[y] and (in y _X) (eq _Z (F y)))) %.
%term omega_ax       pf (and (omega_closed omega)
		          (all[o] imp (omega_closed o) (subset omega o))) %.
%term choice_ax      pf
(imp (all[y1] imp (in y1 _X)
      (all[y2] imp (in y2 _X) (disjoint y1 y2)))
   (ex [x'](all[y] imp (in y _X)
		 (ex_unique ([y'] (and (in y' x') (in y' y))))))) %.
|}

let nats1 = {|
%sort nat 
%term z nat 
%term s {_ nat} nat
|}
let nats2 = {|

%sort even {_ nat}
%term even-z even z
%term even-s {{N}} %<- (even (s (s N))) (even N)

|}

let nats3 = {| 
%sort plus {_ nat} {_ nat} {_ nat}
%term plus-z {{N2}} plus z N2 N2
%term plus-s {{N1 N2 N3}} 
  %<- (plus (s N1) N2 (s N3))
  (plus N1 N2 N3)
|} (* TODO Fix prec of arrows fix sort w/o names *)

let nats4 = {|
%mode plus %in %in %out
%worlds () (plus _ _ _)

%total N1 (plus N1 _ _)
|}
let fol1 =
  {|
%. First-Order Logic
Fragment with implication, negation, universal quantification.

Author: Frank Pfenning

This code is from the article

  Logical Frameworks
  Handbook of Automated Reasoning
  Alan Robinson and Andrei Voronkov, editors
  Chapter 16
  Elsevier Science and MIT Press
  In preparation

%sort i
%sort o

Formulas

%term imp {_ o} {_ o} o
%prec %right 10 imp
%term not {_ o} o
%prec %prefix 12 not
%term forall %-> (%-> i o) o %.

Natural deductions

%sort nd {_ o}

%term impi {{A B}} {_ {_ nd A} nd B} nd (A imp B)
%term impe {{A B}} {_ nd (A imp B)} {_ nd A} nd B

%term noti {{A}} {_ {p o} {_ nd A} nd p} nd (not A)
%term note {{A}} {_ nd (not A)} {C o} {_ nd A} nd C

%term foralli {{A}} {_ {a i} nd (A a)} nd (forall A)
%term foralle {{A}} {_ nd (forall A)} {T i} nd (A T)

%block lnd [A o] {u nd A}
%block lo {_ o}
%block li {_ i}

%worlds (lnd lo li) (nd A)
|}

let fol2 =
  {|

%. Example


%. Hilbert deductions

%sort hil {_ o}

%term k {{A B}} hil (A imp (B imp A))
%term s {{A B C}} hil ((A imp (B imp C)) imp ((A imp B) imp (A imp C)))

%term n1 {{A B}} hil ((A imp (not B)) imp ((A imp B) imp (not A)))
%term n2 {{A B}} hil ((not A) imp (A imp B))

%term f1 {T i} hil ((forall [x i] A x) imp (A T))
%term f2 {{A B}} hil ((forall [x i] (B imp A x)) imp (B imp forall [x i] A x))

%term mp {{A B}} {_ hil (A imp B)} {_ hil A} hil B
%term ug {{A}} {_ {a i} hil (A a)} hil (forall [x i] A x)

%worlds (li) (hil A)

|}

let fol3_1 =
  {|

%. Local reductions

%sort ==>R {_ nd _A} {_ nd _A}
%prec %none 14 ==>R

|}

let fol3_2_1 = {|
%term redl_imp {{A D E}} (impe (impi [u nd A] D u) E) ==>R (D E)

|}

let fol3_2_2 =
  {|
%term redl_not {{A C D E}} (note (noti [p o] [u nd A] D p u) C E) ==>R (D C E)

|}

let fol3_2_3 =
  {| 
%term redl_forall {{D T}} (foralle (foralli [a i] D a) T) ==>R (D T)

|}

let fol3_2 = fol3_2_1 ^ fol3_2_2 ^ fol3_2_3

let fol3_3 =
  {|

%. Local expansions

%sort ==>E {_ nd A} {_ nd A}
%prec %none 14 ==>E
|}

let fol3_4 =
  {|

%term expl_imp {{A B}} {D nd (A imp B)} D ==>E (impi [u nd A] impe D u)
%term expl_not {{A}} {D nd (not A)} D ==>E (noti [p] [u nd A] note D p u)
%term expl_forall {{A}} {D nd (forall A)} D ==>E (foralli [a] foralle D a)

|}

let fol3 = fol3_1 ^ fol3_2 ^ fol3_3 ^ fol3_4

let fol4_1 =
  {|
%. Sequent calculus search result

%def dn (nd (A imp not not A)) (impi [u nd A] noti [p] [w] note w p u)

%. Translating Hilbert derivations to natural deductions

%sort hilnd {_ hil A} {_ nd A}

%term hnd_k {{A B}} hilnd k (impi [u nd A] impi [v nd B] u)
%term hnd_s {{A B C}} hilnd s (impi [u nd (A imp B imp C)] impi [v] impi [w nd A] impe (impe u w) (impe v w))

%term hnd_n1 hilnd n1 (impi [u] impi [v] noti [p] [w] note (impe u w) p (impe v w))

%term hnd_n2 {{A B}} hilnd n2 (impi [u] impi [v] note u B v)
%term hnd_f1 {{A T}} hilnd (f1 T) (impi [u nd (forall [x i] A x)] foralle u T)

%term hnd_f2 {{A B}} hilnd f2 (impi [u] impi [v] foralli [a] impe (foralle u a) v)

%term hnd_mp {{A B}} {H1 hil (A imp B)} {H2 hil A} {D1 nd (A imp B)} {D2 nd A} {_ hilnd H1 D1} {_ hilnd H2 D2} hilnd (mp H1 H2) (impe D1 D2)

%term hnd_ug {{A}} {H1 {a i} hil (A a)} {D1 {a i} nd (A a)} {_ {a i} hilnd (H1 a) (D1 a)} hilnd (ug H1) (foralli D1)
|}
(* %mode {%in x hil _A} {%out y nd _A} hilnd x y *)
let fol4_2 = {|
%mode {%in X _} {%out Y _} hilnd X Y
%worlds (li) (hilnd H D)
%terminates H (hilnd H _)
%covers hilnd %in %out
%total H (hilnd H _)

%? hilnd (mp (mp s k) k) D

%def _ (nd (A imp A))
    (impe
      (impe
        (impi [u]
          impi [v]
          impi [w] impe (impe u w) (impe v w))
        (impi [u] impi [v] u))
      (impi [u] impi [v] u))

      |}

let fol4 = fol4_1 ^ fol4_2
let fol5_1 =
  {|

The deduction theorem for Hilbert derivations

%sort ded {_ {_ hil A} hil B} {_ hil (A imp B)}

%term ded_id {{A}} ded ([u hil A] u) (mp (mp s k) (%the (hil (A imp (A imp A))) k))

%term ded_k {{A B}} ded ([u] k) (mp k k)
%term ded_s {{A B C}} ded ([u] s) (mp k s)

%term ded_n1 {{A B}} ded ([u] n1) (mp k n1)
%term ded_n2 {{A B}} ded ([u] n2) (mp k n2)

%term ded_f1 {{A B T}} ded ([u] f1 T) (mp k (f1 T))
%term ded_f2 {{A B}} ded ([u] f2) (mp k f2)

%term ded_mp {{A B C}} {H1 {_ hil A} hil (B imp C)} {H2 {_ hil A} hil B} {H1' hil (A imp (B imp C))} {H2' hil (A imp B)} {_ ded H1 H1'} {_ ded H2 H2'} ded ([u] mp (H1 u) (H2 u)) (mp (mp s H1') H2')

%term ded_ug {{A B H1 H1'}} {_ {a i} ded ([u] H1 u a) (H1' a)} ded ([u] ug (H1 u)) (mp f2 (ug H1'))
|}
let fol5_2 = {|
%block lded [A o] {u nd A} {v hil A} {h {C o} ded ([w hil C] v) (mp k v)}

%mode ded %in %out
%worlds (li lo lded) (ded H H')
%terminates H (ded H _)
%covers ded %in %out
%total H (ded H _)

|}
let fol5 = fol5_1 ^ fol5_2

let fol6_1 =
  {|

Mapping natural deductions to Hilbert derivations.

%sort ndhil {_ nd A} {_ hil A}

%term ndh_impi {{A1 B C D1 H1 H1' H1''}} {_ ded H1 H1'} {_ {u nd A1} {v hil A1} {_ {C o} ded ([w hil C] v) (mp k v)} {_ ndhil u v} ndhil (D1 u) (H1 v)} ndhil (impi D1) H1'

%term ndh_impe {{A B D1 D2 H1 H2}} {_ ndhil D1 H1} {_ ndhil D2 H2} ndhil (impe D1 D2) (mp H1 H2)

%term ndh_noti {{A1 H1 H1' H1'' D1}} {_ ded (H1 (not A1)) H1'} {_ {p o} {u nd A1} {v hil A1} {_ {C o} ded ([w hil C] v) (mp k v)} {_ ndhil u v} ndhil (D1 p u) (H1 p v)} ndhil (noti D1) (mp (mp n1 H1') H1'')

%term ndh_note {{A C D1 D2 H1 H2}} {_ ndhil D1 H1} {_ ndhil D2 H2} ndhil (note D1 C D2) (mp (mp n2 H1) H2)

%term ndh_foralli {{A}} {D1 {_ i} nd _} {H1 {_ i} hil _} {_ {a i} ndhil (D1 a) (H1 a)} ndhil (foralli D1) (ug H1)

%term ndh_foralle {{A}} {T i} {D1 nd (forall A)} {H1 hil (forall A)} {_ ndhil D1 H1} ndhil (foralle D1 T) (mp (f1 T) H1)
|}
let fol6_2 = {|
%mode {%in X _} {%out Y _} ndhil X Y
%block lndhil [A o] {u nd A} {v hil A} {h {C o} ded ([w] v) (mp k v)} {nh ndhil u v}
%worlds (li lo lndhil) (ndhil D H)
%terminates D (ndhil D _)
%covers ndhil %in %out
%total D (ndhil D _)


|}
let fol6 = fol6_1 ^ fol6_2
let js4 =
  {|
Judgmental S4
[A judgmental reconstruction of modal logic, F.Pfenning and R.Davies,
 MSCS 11:511-540, 2001]

Representation with intrinsic types, worlds,
but not a Kripke semantics

Idea: Translate the judgment
  u1::B1,...,un::Bn ; x1:A1,...,xm:Am |- J
as
  u1:{W'}tm B1 W',...,un:{W'}tm Bn W', ---,
  x1:tm A1 W,...,xm:tm Am W |- J*

where "---" are assumptions y:tm B W' for W' <> W
and if J = (M : A) then J* = M* : tm A W
    if J = (E - A) then J* = E* : exp A W

%sort tp
%term => {_ tp} {_ tp} tp
%term box {_ tp} tp
%term dia {_ tp} tp
%prec %right 10 =>

%sort world

%sort tm {_ tp} {_ world}
%sort exp {_ tp} {_ world}

%term lam {A tp} {B tp} {W world} {_ {_ tm A W} tm B W} tm (A => B) W
%term app {A tp} {B tp} {W world} {_ tm (A => B) W} {_ tm A W} tm B W
%term boxi {A tp} {W world} {_ {w world} tm A w} tm (box A) W
%term boxe {A tp} {C tp} {W world} {_ tm (box A) W} {_ {_ {W' world} tm A W'} tm C W} tm C W
%term t2e {A tp} {W world} {_ tm A W} exp A W
%term diai {A tp} {W world} {_ exp A W} tm (dia A) W
%term diae {A tp} {C tp} {W world} {_ tm (dia A) W} {_ {w world} {_ tm A w} exp C w} exp C W
%term boxep {A tp} {C tp} {W world} {_ tm (box A) W} {_ {_ {W' world} tm A W'} exp C W} exp C W

%sort subdia {_ exp A W} {_ {w world} {_ tm A w} exp C w} {_ exp C W}
%mode subdia %in %in %out

%term sdt2e {A tp} {C tp} {W world} {M tm A W} {F {w world} {_ tm A w} exp C w} subdia (t2e M) ([w] [x] F w x) (F W M)

%term sddiae {A tp} {B tp} {C tp} {W world} {M tm (dia A) W} {E {_ world} {_ tm A _} exp B _} {F {_ world} {_ tm B _} exp C _} {F' {_ world} {_ tm A _} exp C _} {_ {v world} {y tm A v} subdia (E v y) ([w] [x] F w x) (F' v y)} subdia (diae M [v] [y] E v y) ([w] [x] F w x) (diae M [v] [y] F' v y)

%term sdboxep {A tp} {B tp} {C tp} {W world} {M tm (box A) W} {E {_ {_ world} tm B _} exp C _} {F {_ world} {_ tm A _} exp C _} {F' {_ {_ world} tm B _} exp C _} {_ {u {V world} tm B V} subdia (E u) ([w] [x] F w x) (F' u)} subdia (boxep M [u] E u) ([w] [x] F w x) (boxep M [u] F' u)

%block by {B tp} {v world} {y tm B v}
%block bu {B tp} {u {V world} tm B V}
%worlds (by bu) (subdia E F F')
%total E (subdia E F _) %.

This does not work, unfortunately:
The "str" strengthening lemma would require handling cases like strlam where a bound
variable y2 of type tm C2 w appears free in the conclusion, but w is also quantified
by the str family. The main issue is that the case for str ([x][w] y2) ([w] y2) cannot
be typed because y2 has type tm C2 w but w is the variable we are quantifying over.

Examples

%def _ ({A tp} {W world} tm (box A => A) W) (lam [x] boxe x [u] u W)
%def _ ({A tp} {W world} tm (box A => box (box A)) W) (lam [x] boxe x [u] boxi [w] boxi [w'] u w')
%def _ ({A tp} {B tp} {W world} tm (box (A => B) => box A => box B) W) (lam [x] lam [y] boxe x [u] boxe y [v] boxi [w] app (u w) (v w))
%def _ ({A tp} {W world} tm (A => dia A) W) (lam [x] diai (t2e x))
%def _ ({A tp} {W world} tm (dia (dia A) => dia A) W) (lam [x] diai (diae x [w] [y] diae y [v] [z] t2e z))
%def _ ({A tp} {B tp} {W world} tm (box (A => B) => dia A => dia B) W) (lam [x] lam [y] diai (boxep x [u] diae y [w] [z] t2e (app (u w) z)))
%.
Counterexamples, all must fail:
The following would require box to be a comonad (A => box A), but this is not valid in S4.
The term (lam [x] boxi [w] x) fails because x has type tm A W but we need tm A w for
arbitrary w, which would require the structural rule for modal contexts.
Similarly (lam [x] diae x [w][y] t2e y) fails because y has type tm A w but we need
tm A W for the current world W.
The term for (dia (A => B) => dia A => dia B) fails because dia is not "normal" in S4.
The term (dia A => box B) => box (A => B) is true in Kripke semantics a la Simpson but
not in the judgmental formulation.
The two S5 theorems (dia A => box (dia A)) and (dia (box A) => box A) both fail because
S4 does not have the symmetry or Euclidean properties needed for S5.
|}

let lam =
  {|
Lambda-Calculus Fragment of Mini-ML.
Author: Frank Pfenning

Simple types
%sort tp

%term arrow {_ tp} {_ tp} tp

%. Expressions
%sort exp

%term lam {_ {_ exp} exp} exp
%term app {_ exp} {_ exp} exp

%. Type inference
|- E : T  (expression E has type T)

%sort of {_ exp} {_ tp}
%mode of %in %star

%term tp_lam {E {_ exp} exp} {T1 tp} {T2 tp} {_ {x exp} {_ of x T1} of (E x) T2} of (lam E) (arrow T1 T2)

%term tp_app {E1 exp} {E2 exp} {T1 tp} {T2 tp} {_ of E1 (arrow T2 T1)} {_ of E2 T2} of (app E1 E2) T1

%. Evaluation (call-by-value)
E ==> V  (expression E evaluates to value V)

%sort eval {_ exp} {_ exp}
%mode eval %in %out

%term ev_lam {E {_ exp} exp} eval (lam E) (lam E)

%term ev_app {E1 exp} {E2 exp} {V exp} {V2 exp} {E1' {_ exp} exp} {_ eval E1 (lam E1')} {_ eval E2 V2} {_ eval (E1' V2) V} eval (app E1 E2) V

%. Regular world for type-checking
%block tp_var [T tp] {x exp} {u of x T}
%worlds (tp_var) (of E T)

%. Type inference terminates
%terminates E (of E T)

%. There is at least one typing rule for every expression
%covers of %in %star

%. Closed worlds for evaluation
%worlds () (eval E V)

%. There is at least one evaluation rule for every closed expression
%covers eval %in %out

%. Type preservation as higher-level family
%sort tps {_ eval E V} {_ of E T} {_ of V T}

%term tps_lam {E {_ exp} exp} {T1 tp} {T2 tp} {P {_ exp} {_ of _ T1} of _ T2} tps ev_lam (tp_lam P) (tp_lam P)

%term tps_app {E1 exp} {E2 exp} {V exp} {V2 exp} {E1' {_ exp} exp} {T tp} {T2 tp} {D1 eval E1 (lam E1')} {D2 eval E2 V2} {D3 eval (E1' V2) V} {P1 of E1 (arrow T2 T)} {P2 of E2 T2} {Q1' {_ exp} {_ of _ T2} of _ T2} {Q2 of V2 T2} {Q of V T} {_ tps D1 P1 (tp_lam Q1')} {_ tps D2 P2 Q2} {_ tps D3 (Q1' V2 Q2) Q} tps (ev_app D3 D2 D1) (tp_app P2 P1) Q

%mode tps %in %in %out
%worlds () (tps D P _)
%total D (tps D P _)

%. Applying type preservation
%def e0 _ (app (lam [x] x) (lam [y] y))
%? of e0 T
%? eval e0 V
%? tps d0 p0 Q

%. Example of regular worlds
cp copies input to output.

%sort cp {_ exp} {_ exp}

%term cp_app {E1 exp} {E2 exp} {F1 exp} {F2 exp} {_ cp E1 F1} {_ cp E2 F2} cp (app E1 E2) (app F1 F2)

%term cp_lam {E {_ exp} exp} {F {_ exp} exp} {_ {x exp} {_ cp x x} cp (E x) (F x)} cp (lam [x] E x) (lam [x] F x)

%mode cp %in %out
%block cp_var {x exp} {u cp x x}
%worlds (cp_var) (cp E _)
%total E (cp E _)
%.
Following version cannot be checked: input coverage on parameter y is violated.
It would declare cp with a block containing {x:exp} {y:exp} {u:cp x y}, but cp_lam's
higher-order premise would allow y to differ from x, which violates input coverage.

Following version also cannot be checked: output coverage on (F y) is violated.
It would add a premise cp y y -> cp (E x) (F y), meaning F y may not be covered
for the output position.

|}

let polylam =
  {|
%sort tp
%term => {_ tp} {_ tp} tp
%prec %right 10 =>
%term all {_ {_ tp} tp} tp

%sort tm {_ tp}
%term lam {A tp} {B tp} {_ {_ tm A} tm B} tm (A => B)
%term app {A tp} {B tp} {_ tm (A => B)} {_ tm A} tm B
%term tlam {A {_ tp} tp} {_ {a tp} tm (A a)} tm (all A)
%term tapp {A {_ tp} tp} {_ tm (all A)} {B tp} tm (A B)

%def nat _ (all [a] a => (a => a) => a)
%def zero _ (tlam [a] lam [z] lam [s] z)
%def succ _ (lam [x] tlam [a] lam [z] lam [s] app s (app (app (tapp x a) z) s))
%def succ' (tm (nat => nat)) (lam [x] tlam [a] lam [z] lam [s] app s (app (app (tapp x a) z) s))
%def plus _ (lam [x] lam [y] app (app (tapp y nat) x) succ)
%def times _ (lam [x] lam [y] app (app (tapp y nat) zero) (app plus x))
%def exp _ (lam [x] lam [y] app (app (tapp y nat) (app succ zero)) (app times x))
|}

(* Propositional calculus: intuitionistic natural deduction
   Ported from twelf/examples/prop-calc/prop-calc.elf
   Note: %infix declarations omitted (Twelf `%infix` → STELF `%prec`)
   Combined into a single string to avoid cross-chunk scope issues.
*)
let prop_calc_types =
  {|
%sort o
%term imp {_ o} {_ o} o
%prec %right 10 imp
%term and {_ o} {_ o} o
%prec %right 11 and
%term true o
|}

(* Hilbert axioms - all require o, imp, and, true in scope.
   Uses infix notation for imp since it is declared %prec %right 10 *)
let prop_calc_hilbert =
  {|
%. Provability (Hilbert-style)
%sort pf {_ o}
%term K {{A B}} pf (A imp (B imp A))
%term S {{A B C}} pf ((A imp (B imp C)) imp ((A imp B) imp (A imp C)))
%term ONE pf true
%term PAIR {{A B}} pf (A imp (B imp (A and B)))
%term LEFT {{A B}} pf ((A and B) imp A)
%term RIGHT {{A B}} pf ((A and B) imp B)
%term MP {{A B}} {_ pf (A imp B)} {_ pf A} pf B
|}

(* Natural deduction: require o, imp, and, true to be in scope.
   Uses infix notation for imp and and. *)
let prop_calc_nd =
  {|
%. Natural deduction
%sort nd {_ o}
%term trueI nd true
%term andI {{A B}} {_ nd A} {_ nd B} nd (A and B)
%term andEL {{A B}} {_ nd (A and B)} nd A
%term andER {{A B}} {_ nd (A and B)} nd B
%term impliesI {{A B}} {_ {_ nd A} nd B} nd (A imp B)
%term impliesE {{A B}} {_ nd (A imp B)} {_ nd A} nd B
|}

(* Mini-ML expression language
   Ported from twelf/examples/mini-ml/mini-ml.elf
*)
let mini_ml_exp =
  {|
%sort exp
%term z exp
%term s {_ exp} exp
%term case {_ exp} {_ exp} {_ {_ exp} exp} exp
%term pair {_ exp} {_ exp} exp
%term fst {_ exp} exp
%term snd {_ exp} exp
%term lam {_ {_ exp} exp} exp
%term app {_ exp} {_ exp} exp
%term letv {_ exp} {_ {_ exp} exp} exp
%term letn {_ exp} {_ {_ exp} exp} exp
%term fix {_ {_ exp} exp} exp
|}

(* Mini-ML values
   Ported from twelf/examples/mini-ml/value.elf
*)
let mini_ml_value =
  {|
%sort value {_ exp}
%term val-z value z
%term val-lam {{E}} value (lam E)
%term val-s {{V}} %<- (value (s V)) (value V)
%term val-pair {{V1 V2}} %<- (value (pair V1 V2)) (value V1) (value V2)
%mode value %in
%worlds () (value _)
|}

(* Mini-ML types
   Ported from twelf/examples/mini-ml/tp.elf
*)
let mini_ml_tp =
  {|
%sort tp
%term nat tp
%term cross {_ tp} {_ tp} tp
%term arrow {_ tp} {_ tp} tp
|}

(* Ackermann function in unary arithmetic
   Ported from twelf/examples/arith/arith.elf
   Note: %compile and %query directives omitted (not supported)
*)
let arith_nat =
  {|
%sort nat
%term z nat
%term s {_ nat} nat
|}

let arith_nt =
  {|
%sort nt {_ nat}
%term nt-z nt z
%term nt-s {{X}} %<- (nt (s X)) (nt X)
|}

let arith_plus =
  {|
%sort plus {_ nat} {_ nat} {_ nat}
%term p-z {{Y}} plus z Y Y
%term p-s {{X Y Z}} %<- (plus (s X) Y (s Z)) (plus X Y Z)
|}

let arith_acker =
  {|
%sort acker {_ nat} {_ nat} {_ nat}
%mode acker %in %in %out
%term a-1 {{Y}} acker z Y (s Y)
%term a-2 {{X Z}} %<- (acker (s X) z Z) (acker X (s z) Z)
%term a-3 {{X Y Z Z'}} %<- (acker (s X) (s Y) Z) (acker (s X) Y Z') (acker X Z' Z)
%worlds () (acker _ _ _)
|}

(* List append, element type, and list sort.
   Ported from twelf/examples/guide/lists.elf.
   Note: %query directives omitted (not supported).
   Element type 'o' declared here to keep this chunk self-contained.
*)
let guide_lists_types =
  {|
%sort o
%sort list
%term nil list
%term cons {_ o} {_ list} list
|}

let guide_lists_append =
  {|
%sort append {_ list} {_ list} {_ list}
%term appNil {K list} append nil K K
%term appCons {A o} {L list} {K list} {M list} %<- (append (cons A L) K (cons A M)) (append L K M)
|}

let guide_lists_mode =
  {|
%mode append %in %in %out
%worlds () (append _ _ _)
%total L (append L _ _)
|}

(* Natural numbers with equality and ordering.
   Ported from twelf/examples/tapl_ch13/nat.elf.
   Note: %mode, %terminates, %unique omitted (not supported or not needed).
   nat is re-declared here to keep this chunk self-contained.
*)
let tapl_nat_base =
  {|
%sort nat
%term z nat
%term s {_ nat} nat
|}

let tapl_nat_eq =
  {|
%sort nat_eq {_ nat} {_ nat}
%sort nat_neq {_ nat} {_ nat}
%sort nat_lt {_ nat} {_ nat}

%term neq_eq_refl {{N}} nat_eq N N

%term nat_neq_zs {{N}} nat_neq z (s N)
%term nat_neq_sz {{N}} nat_neq (s N) z
%term nat_neq_ss {{N1 N2}} %<- (nat_neq (s N1) (s N2)) (nat_neq N1 N2)

%term nat_lt_zs {{N}} nat_lt z (s N)
%term nat_lt_ss {{N1 N2}} %<- (nat_lt (s N1) (s N2)) (nat_lt N1 N2)
|}

(* Positive fragment of first-order logic with natural deduction.
   Ported from twelf/examples/lp_horn/natded.elf.
   Operator declarations: imp with %prec %right 10, and with %prec %right 11.
   Note: %name hints and %block/%worlds omitted.
*)
let lp_horn_nd =
  {|
%sort i
%sort o
%sort p

%term atom {_ p} o
%term and {_ o} {_ o} o
%prec %right 11 and
%term imp {_ o} {_ o} o
%prec %right 10 imp
%term true o
%term forall {_ {_ i} o} o

%sort pf {_ o}
%term andi {{A B}} {_ pf A} {_ pf B} pf (A and B)
%term andel {{A B}} {_ pf (A and B)} pf A
%term ander {{A B}} {_ pf (A and B)} pf B
%term impi {{A B}} {_ {_ pf A} pf B} pf (A imp B)
%term impe {{A B}} {_ pf (A imp B)} {_ pf A} pf B
%term truei pf true
%term foralli {{A}} {_ {a i} pf (A a)} pf (forall A)
%term foralle {{A}} {_ pf (forall A)} {T i} pf (A T)
|}

(* Untyped lambda-calculus terms.
   Ported from twelf/examples/church_rosser/lam.elf.
   This is just the term syntax — no reduction or typing.
*)
let church_rosser_lam =
  {|
%sort term
%term lam {_ {_ term} term} term
%term app {_ term} {_ term} term
|}

(* Formula syntax for predicate calculus (intuitionistic and classical).
   Ported from twelf/examples/cut_elim/formulas.elf.
   Note: %name hints omitted.
*)
let cut_elim_formulas =
  {|
%sort i
%sort o
%term and {_ o} {_ o} o
%prec %right 11 and
%term imp {_ o} {_ o} o
%prec %right 10 imp
%term or {_ o} {_ o} o
%prec %right 11 or
%term not {_ o} o
%prec %prefix 12 not
%term true o
%term false o
%term forall {_ {_ i} o} o
%term exists {_ {_ i} o} o
|}

(* Natural deduction for intuitionistic logic (positive+negative fragment).
   Ported from twelf/examples/guide/nd.elf.
   Note: %theorem/%prove stripped; abbreviation definitions for not/noti/note
   handled as regular terms since STELF does not support Twelf abbreviation syntax.
   The %block/%worlds declarations are kept as-is.
*)
let guide_nd =
  {|
%sort i
%sort o
%term imp {_ o} {_ o} o
%prec %right 10 imp
%term and {_ o} {_ o} o
%prec %right 11 and
%term true o
%term or {_ o} {_ o} o
%prec %right 11 or
%term false o
%term forall {_ {_ i} o} o
%term exists {_ {_ i} o} o

%sort nd {_ o}

%term impi {{A B}} {_ {_ nd A} nd B} nd (A imp B)
%term impe {{A B}} {_ nd (A imp B)} {_ nd A} nd B
%term andi {{A B}} {_ nd A} {_ nd B} nd (A and B)
%term ande1 {{A B}} {_ nd (A and B)} nd A
%term ande2 {{A B}} {_ nd (A and B)} nd B
%term truei nd true
%term ori1 {{A B}} {_ nd A} nd (A or B)
%term ori2 {{A B}} {_ nd B} nd (A or B)
%term ore {{A B C}} {_ nd (A or B)} {_ {_ nd A} nd C} {_ {_ nd B} nd C} nd C
%term falsee {{C}} {_ nd false} nd C
%term foralli {{A}} {_ {x i} nd (A x)} nd (forall A)
%term foralle {{A}} {_ nd (forall A)} {T i} nd (A T)
%term existsi {{A}} {T i} {_ nd (A T)} nd (exists A)
%term existse {{A C}} {_ nd (exists A)} {_ {x i} {_ nd (A x)} nd C} nd C

%block nd_hyp {A o} {u nd A}
%block nd_parm {x i}
%worlds (nd_hyp nd_parm) (nd A)

%sort red {_ nd A} {_ nd A}
%term impred {{A B D E}} red (impe (impi D) E) (D E)
%term andred1 {{A B D E}} red (ande1 (andi D E)) D
%term andred2 {{A B D E}} red (ande2 (andi D E)) E
%term orred1 {{A B C D E1 E2}} red (ore (ori1 D) E1 E2) (E1 D)
%term orred2 {{A B C D E1 E2}} red (ore (ori2 D) E1 E2) (E2 D)
%term forallred {{A D T}} red (foralle (foralli D) T) (D T)
%term existsred {{A C T D E}} red (existse (existsi T D) E) (E T D)
|}

(* Direct-style CPS BNF term syntax.
   Ported from twelf/examples/cpsocc/dsBNF.elf.
   Simple type declarations only — no infix operators.
*)
let cpsocc_dsbnf =
  {|
%sort droot
%sort dexp
%sort dtriv

%term dexp->droot {_ dexp} droot
%term dapp {_ dexp} {_ dexp} dexp
%term dtriv->dexp {_ dtriv} dexp
%term dlam {_ {_ dtriv} droot} dtriv
|}

(* Simply-typed lambda-calculus: types, terms, typing, values, and small-step semantics.
   Ported from twelf/examples/small_step/lam.elf (syntax + typing + values + step only;
   preservation and progress proofs omitted — they use Twelf automation not in STELF).
   Infix operators: => (type arrow, left 5), @ (application, left 5),
                    is (typing, left 5), ~> (step, left 5).
*)
let small_step_lam_types =
  {|
%sort tp
%sort tm

%term => {_ tp} {_ tp} tp
%prec %left 5 =>
|}

let small_step_lam_terms =
  {|
%term @ {_ tm} {_ tm} tm
%prec %left 5 @
%term lam {_ tp} {_ {_ tm} tm} tm
|}

let small_step_lam_typing =
  {|
%sort is {_ tm} {_ tp}
%prec %left 5 is

%term is_@ {{E1 E2 T1 T2}} {_ E1 is (T1 => T2)} {_ E2 is T1} (E1 @ E2) is T2
%term is_lam {{E T1 T2}} {_ {x tm} {_ x is T1} (E x) is T2} (lam T1 E) is (T1 => T2)
|}

let small_step_lam_value =
  {|
%sort value {_ tm}
%term value_lam {{T E}} value (lam T E)
|}

let small_step_lam_step =
  {|
%sort ~> {_ tm} {_ tm}
%prec %left 5 ~>

%term ~>_@1 {{E1 E1' E2}} {_ E1 ~> E1'} (E1 @ E2) ~> (E1' @ E2)
%term ~>_@2 {{E1 E2 E2'}} {_ value E1} {_ E2 ~> E2'} (E1 @ E2) ~> (E1 @ E2')
%term ~>_@3 {{T E1 E2}} {_ value E2} ((lam T E1) @ E2) ~> (E1 E2)
|}

(* Explicit contexts in LF: types, expressions, and natural numbers.
   Ported from twelf/examples/crary/explicit/excon.elf (first part only —
   typing rules and nat up through leq).
   Note: %name hints omitted.
*)
let crary_excon =
  {|
%sort tp
%sort exp

%term o tp
%term p {_ exp} tp
%term pi {_ tp} {_ {_ exp} tp} tp

%term b exp
%term lam {_ tp} {_ {_ exp} exp} exp
%term app {_ exp} {_ exp} exp

%sort of {_ exp} {_ tp}
%term of/b of b o
%term of/lam {{A B M}} {_ {x exp} {_ of x A} of (M x) (B x)} of (lam A M) (pi A B)
%term of/app {{A B M N}} {_ of M (pi A B)} {_ of N A} of (app M N) (B N)

%sort nat
%term zero nat
%term s {_ nat} nat

%sort nat-eq {_ nat} {_ nat}
%term nat-eq/i {{N}} nat-eq N N

%sort leq {_ nat} {_ nat}
%term leq/z {{N}} leq zero N
%term leq/s {{N1 N2}} %<- (leq (s N1) (s N2)) (leq N1 N2)
|}

(* CRARY-EXCON-REV: excon-rev.elf (explicit contexts, reversed variant)
   Ported from twelf/examples/crary/explicit/excon-rev.elf (syntax-only chunk).
   Note: Twelf uses `0 : nat` (numeric literal); renamed to `zero` for STELF.
   The `-` family (isvar dependency decl) is a Twelf quirk; omitted.
*)
let crary_excon_rev_syntax =
  {|
%sort tp
%sort exp

%term o tp
%term p {_ exp} tp
%term pi {_ tp} {_ {_ exp} tp} tp

%term b exp
%term lam {_ tp} {_ {_ exp} exp} exp
%term app {_ exp} {_ exp} exp

%sort of {_ exp} {_ tp}
%term of/b of b o
%term of/lam {{A B M}} {_ {x exp} {_ of x A} of (M x) (B x)} of (lam A M) (pi A B)
%term of/app {{A B M N}} {_ of M (pi A B)} {_ of N A} of (app M N) (B N)

%sort nat
%term zero nat
%term s {_ nat} nat

%sort nat-eq {_ nat} {_ nat}
%term nat-eq/i {{N}} nat-eq N N

%sort lt {_ nat} {_ nat}
%term lt/z {{N}} lt zero (s N)
%term lt/s {{N1 N2}} %<- (lt (s N1) (s N2)) (lt N1 N2)

%sort ctx
%term nil ctx
%term cons {_ ctx} {_ exp} {_ tp} ctx
|}

(* TAPL-DEFS: tapl_ch13/defs.elf (STLC with references — syntax-only chunk)
   Ported from twelf/examples/tapl_ch13/defs.elf.
   Note: Uses unique prefixes to avoid conflicts with earlier suite declarations.
   `tp` → `ref_tp`, `exp` → `ref_exp` to avoid clashes with SMALL-STEP-LAM/CRARY-EXCON.
   `nat`, `z`, `s` re-used from earlier suites (already in scope).
   `=>` already infix-left 5 from SMALL-STEP-LAM; used that way here.
   Dropped: %name hints, %freeze, %terminates {}, %unique, step/typing rules.
*)
let tapl_defs_types =
  {|
%sort ref_tp
%term ref_arrow {_ ref_tp} {_ ref_tp} ref_tp
%term unit_tp ref_tp
%term ref {_ ref_tp} ref_tp
|}

let tapl_defs_labels =
  {|
%sort nat
%term z nat
%term s {_ nat} nat

%sort label
%term lbl {_ nat} label
|}

let tapl_defs_exp =
  {|
%sort ref_exp
%term ref_app {_ ref_exp} {_ ref_exp} ref_exp
%term ref_lam {_ ref_tp} {_ {_ ref_exp} ref_exp} ref_exp
%term dot ref_exp
%term alloc {_ ref_exp} ref_exp
%term deref {_ ref_exp} ref_exp
%term gets {_ ref_exp} {_ ref_exp} ref_exp
%term loc {_ label} ref_exp
|}

let tapl_defs_value =
  {|
%sort ref_value {_ ref_exp}
%mode ref_value %in
%term v_lam {{T E}} ref_value (ref_lam T E)
%term v_dot ref_value dot
%term v_loc {{L}} ref_value (loc L)
|}

let tapl_defs_store =
  {|
%sort ref_store
%term store_nil ref_store
%term store_cons {_ ref_tp} {_ ref_store} ref_store

%sort length_store {_ ref_store} {_ nat}
%mode length_store %in %out
%term length_store_nil length_store store_nil z
%term length_store_cons {S ref_store} {N nat} %<- (length_store (store_cons _ S) (s N)) (length_store S N)
%worlds () (length_store _ _)
%total S (length_store S _)

%sort find_in_store {_ label} {_ ref_store} {_ ref_tp}
%mode find_in_store %in %in %out
%term find_in_store_yes {T ref_tp} find_in_store (lbl z) (store_cons T _) T
%term find_in_store_no {N nat} {S ref_store} {T ref_tp} %<- (find_in_store (lbl (s N)) (store_cons _ S) T) (find_in_store (lbl N) S T)
%worlds () (find_in_store _ _ _)
|}

let tapl_defs_heap =
  {|
%sort ref_heap
%term heap_nil ref_heap
%term heap_cons {_ ref_exp} {_ ref_heap} ref_heap

%sort length_heap {_ ref_heap} {_ nat}
%mode length_heap %in %out
%term length_heap_nil length_heap heap_nil z
%term length_heap_cons {H ref_heap} {N nat} %<- (length_heap (heap_cons _ H) (s N)) (length_heap H N)
%worlds () (length_heap _ _)
%total H (length_heap H _)

%sort find_in_heap {_ label} {_ ref_heap} {_ ref_exp}
%mode find_in_heap %in %in %out
%term find_in_heap_yes {E ref_exp} find_in_heap (lbl z) (heap_cons E _) E
%term find_in_heap_no {N nat} {H ref_heap} {E ref_exp} %<- (find_in_heap (lbl (s N)) (heap_cons _ H) E) (find_in_heap (lbl N) H E)
%worlds () (find_in_heap _ _ _)

%sort replace_in_heap {_ ref_heap} {_ label} {_ ref_exp} {_ ref_heap}
%mode replace_in_heap %in %in %in %out
%term replace_in_heap_yes {E1 ref_exp} {H ref_heap} {E2 ref_exp} replace_in_heap (heap_cons E1 H) (lbl z) E2 (heap_cons E2 H)
%term replace_in_heap_no {E1 ref_exp} {H ref_heap} {N nat} {E2 ref_exp} {H' ref_heap} %<- (replace_in_heap (heap_cons E1 H) (lbl (s N)) E2 (heap_cons E1 H')) (replace_in_heap H (lbl N) E2 H')
%worlds () (replace_in_heap _ _ _ _)

%sort append_heap {_ ref_heap} {_ ref_exp} {_ ref_heap}
%mode append_heap %in %in %out
%term append_heap_nil {E ref_exp} append_heap heap_nil E (heap_cons E heap_nil)
%term append_heap_cons {E1 ref_exp} {H ref_heap} {E2 ref_exp} {H' ref_heap} %<- (append_heap (heap_cons E1 H) E2 (heap_cons E1 H')) (append_heap H E2 H')
%worlds () (append_heap _ _ _)
%total H (append_heap H _ _)
|}

(* SMALL-STEP-SYSTEM-F: system_f.elf (System F with nat)
   Ported from twelf/examples/small_step/system_f.elf.
   Operators: `=>` (left 5), `@` (left 5), `is` (left 5), `~>` (left 5), `#` (left 5).
   Dropped: preservation/progress proofs (use %block/%worlds unsupported forms).
   Note: `Lam` (capital L) = type abstraction; `#` = type application.
*)
let small_step_sysf_types =
  {|
%sort tp
%sort tm

%term nat tp
%term => {_ tp} {_ tp} tp
%prec %left 5 =>
%term forall {_ {_ tp} tp} tp
|}

let small_step_sysf_terms =
  {|
%term z tm
%term s {_ tm} tm
%term @ {_ tm} {_ tm} tm
%prec %left 5 @
%term lam {_ tp} {_ {_ tm} tm} tm
%term Lam {_ {_ tp} tm} tm
%term # {_ tm} {_ tp} tm
%prec %left 5 #
|}

let small_step_sysf_typing =
  {|
%sort is {_ tm} {_ tp}
%prec %left 5 is

%term is_z z is nat
%term is_s {{E}} {_ E is nat} (s E) is nat
%term is_@ {{E1 E2 T1 T2}} {_ E1 is (T1 => T2)} {_ E2 is T1} (E1 @ E2) is T2
%term is_lam {{E T1 T2}} {_ {x tm} {_ x is T1} (E x) is T2} (lam T1 E) is (T1 => T2)
%term is_Lam {{E T}} {_ {a tp} (E a) is (T a)} (Lam E) is (forall T)
%term is_# {{E T1 T2}} {_ E is (forall T1)} ((E # T2) is (T1 T2))
|}

let small_step_sysf_value =
  {|
%sort value {_ tm}
%term value_z value z
%term value_s {{V}} {_ value V} value (s V)
%term value_lam {{T E}} value (lam T E)
%term value_Lam {{E}} value (Lam E)
|}

let small_step_sysf_step =
  {|
%sort ~> {_ tm} {_ tm}
%prec %left 5 ~>

%term ~>_s {{E E'}} {_ E ~> E'} (s E) ~> (s E')
%term ~>_@1 {{E1 E1' E2}} {_ E1 ~> E1'} (E1 @ E2) ~> (E1' @ E2)
%term ~>_@2 {{E1 E2 E2'}} {_ value E1} {_ E2 ~> E2'} (E1 @ E2) ~> (E1 @ E2')
%term ~>_@3 {{T E1 E2}} {_ value E2} ((lam T E1) @ E2) ~> (E1 E2)
%term ~>_#1 {{E E' T}} {_ E ~> E'} (E # T) ~> (E' # T)
%term ~>_#2 {{E T}} (Lam E) # T ~> (E T)
|}

(* SMALL-STEP-SYSTEM-F-ISO: system_f_iso.elf (System F + iso-recursive types)
   Ported from twelf/examples/small_step/system_f_iso.elf.
   Adds: mu, roll, unroll, value_roll, ~>_roll, ~>_unroll1, ~>_unroll2.
   Dropped: preservation/progress proofs.
*)
let small_step_sysf_iso_types =
  {|
%sort tp
%sort tm

%term nat tp
%term => {_ tp} {_ tp} tp
%prec %left 5 =>
%term forall {_ {_ tp} tp} tp
%term mu {_ {_ tp} tp} tp
|}

let small_step_sysf_iso_terms =
  {|
%term z tm
%term s {_ tm} tm
%term @ {_ tm} {_ tm} tm
%prec %left 5 @
%term roll {_ tm} {_ tp} tm
%term unroll {_ tm} {_ tp} tm
%term lam {_ tp} {_ {_ tm} tm} tm
%term Lam {_ {_ tp} tm} tm
%term # {_ tm} {_ tp} tm
%prec %left 5 #
|}

let small_step_sysf_iso_typing =
  {|
%sort is {_ tm} {_ tp}
%prec %left 5 is

%term is_z z is nat
%term is_s {{E}} {_ E is nat} (s E) is nat
%term is_roll {{E T}} {_ E is (T (mu T))} (roll E (mu T)) is (mu T)
%term is_unroll {{E T}} {_ E is (mu T)} (unroll E (mu T)) is (T (mu T))
%term is_@ {{E1 E2 T1 T2}} {_ E1 is (T1 => T2)} {_ E2 is T1} (E1 @ E2) is T2
%term is_lam {{E T1 T2}} {_ {x tm} {_ x is T1} (E x) is T2} (lam T1 E) is (T1 => T2)
%term is_Lam {{E T}} {_ {a tp} (E a) is (T a)} (Lam E) is (forall T)
%term is_# {{E T1 T2}} {_ E is (forall T1)} ((E # T2) is (T1 T2))
|}

let small_step_sysf_iso_value =
  {|
%sort value {_ tm}
%term value_z value z
%term value_s {{V}} {_ value V} value (s V)
%term value_roll {{E T}} {_ value E} value (roll E T)
%term value_lam {{T E}} value (lam T E)
%term value_Lam {{E}} value (Lam E)
|}

let small_step_sysf_iso_step =
  {|
%sort ~> {_ tm} {_ tm}
%prec %left 5 ~>

%term ~>_s {{E E'}} {_ E ~> E'} (s E) ~> (s E')
%term ~>_@1 {{E1 E1' E2}} {_ E1 ~> E1'} (E1 @ E2) ~> (E1' @ E2)
%term ~>_@2 {{E1 E2 E2'}} {_ value E1} {_ E2 ~> E2'} (E1 @ E2) ~> (E1 @ E2')
%term ~>_@3 {{T E1 E2}} {_ value E2} ((lam T E1) @ E2) ~> (E1 E2)
%term ~>_#1 {{E E' T}} {_ E ~> E'} (E # T) ~> (E' # T)
%term ~>_#2 {{E T}} (Lam E) # T ~> (E T)
%term ~>_unroll1 {{E E' T}} {_ E ~> E'} (unroll E T) ~> (unroll E' T)
%term ~>_unroll2 {{E T1 T2}} {_ value E} (unroll (roll E T1) T2) ~> E
%term ~>_roll {{E E' T}} {_ E ~> E'} (roll E T) ~> (roll E' T)
|}

(* POPLMARK-1A: F-sub subtyping syntax-only chunk.
   Ported from twelf/examples/poplmark/1a.elf (syntax declarations only).
   Dropped: all proof families (trans*, narrow*, reflx, soundness, completeness)
   because they use %block some {...} block {...} which is not supported.
   Nat is needed for the mutual induction measure.
*)
let poplmark_1a_syntax =
  {|
%sort tp
%term top tp
%term arrow {_ tp} {_ tp} tp
%term forall {_ tp} {_ {_ tp} tp} tp

%sort assm {_ tp} {_ tp}
%sort sub {_ tp} {_ tp}

%term sub_top {{T}} sub T top
%term sub_refl {{X}} %<- (sub X X) (assm X _)
%term sub_trans {{X U T}} %<- (sub X T) (assm X U) (sub U T)
%term sub_arrow {{S1 S2 T1 T2}} %<- (sub (arrow S1 S2) (arrow T1 T2)) (sub T1 S1) (sub S2 T2)
%term sub_forall {{S1 S2 T1 T2}} %<- (sub (forall S1 S2) (forall T1 T2)) (sub T1 S1) ({x tp} {_ assm x T1} sub (S2 x) (T2 x))

%sort var {_ tp}
%sort false

%sort nat
%term z nat
%term s {_ nat} nat

%sort nat_eq {_ nat} {_ nat}
%term nat_eq_ {{N}} nat_eq N N
|}

(* POPLMARK-2A: System Fw subtyping syntax-only chunk.
   Ported from twelf/examples/poplmark/2a.elf (syntax + typing declarations only).
   Relies on POPLMARK-1A's tp/top/arrow/forall being in global scope from the prior test.
   Adds: sub_tp declarative subtyping, term/value/of typing for System Fw terms.
   Dropped: sub_tp_forall (higher-order premise causes reconstruction failure),
   of_tabs, of_tapp (involve forall which needs higher-order sub_tp_forall).
*)
let poplmark_2a_syntax =
  {|
%sort sub_tp {_ tp} {_ tp}
%term sub_tp_top {{T}} sub_tp T top
%term sub_tp_refl {{T}} sub_tp T T
%term sub_tp_trans {{T1 T2 T3}} %<- (sub_tp T1 T3) (sub_tp T1 T2) (sub_tp T2 T3)
%term sub_tp_arrow {{S1 S2 T1 T2}} %<- (sub_tp (arrow S1 S2) (arrow T1 T2)) (sub_tp T1 S1) (sub_tp S2 T2)

%sort term
%term abs {_ tp} {_ {_ term} term} term
%term app {_ term} {_ term} term
%term tabs {_ tp} {_ {_ tp} term} term
%term tapp {_ term} {_ tp} term

%sort value {_ term}
%term value_abs {{T E}} value (abs T E)
%term value_tabs {{T E}} value (tabs T E)

%sort of {_ term} {_ tp}
%term of_abs {{T1 T2 E}} {_ {x term} {_ of x T1} of (E x) T2} of (abs T1 E) (arrow T1 T2)
%term of_app {{E1 E2 T11 T12}} {_ of E1 (arrow T11 T12)} {_ of E2 T11} of (app E1 E2) T12
%term of_sub {{E S T}} {_ of E S} {_ sub_tp S T} of E T
|}

(* POPLMARK-1B: Record row sorts and extended subtyping.
   Ported from twelf/examples/poplmark/1b.elf (syntax block only).
   Depends on POPLMARK-1A's tp/top/arrow/forall/assm/sub/var/false/nat/z/s/nat_eq
   being in global scope (from 1a running first in the suite).
   Adds: nat_neq/less/more, plus, label, trow (row sort + constructors),
   record tp constructor, sub_trow, sub_tp, sub_tp_trow.
   Dropped: all proof families (sum_inc, commute', assoc, assoc', add, etc.)
   and %block/%reduces declarations.
*)
let poplmark_1b_syntax =
  {|
%sort nat_neq {_ nat} {_ nat}
%term nat_neq_zs {{N}} nat_neq z (s N)
%term nat_neq_sz {{N}} nat_neq (s N) z
%term nat_neq_ss {{N M}} %<- (nat_neq (s N) (s M)) (nat_neq N M)

%sort nat_less {_ nat} {_ nat}
%term nat_less_z {{N}} nat_less z (s N)
%term nat_less_s {{N M}} %<- (nat_less (s N) (s M)) (nat_less N M)

%sort nat_more {_ nat} {_ nat}
%term nat_more_z {{N}} nat_more (s N) z
%term nat_more_s {{N M}} %<- (nat_more (s N) (s M)) (nat_more N M)

%sort plus {_ nat} {_ nat} {_ nat}
%term plus_z {{N}} plus z N N
%term plus_s {{M N N'}} %<- (plus (s M) N (s N')) (plus M N N')

%sort label
%term label_nat {_ nat} label

%sort label_eq {_ label} {_ label}
%term label_eq_ {{L}} label_eq L L

%sort label_neq {_ label} {_ label}
%term label_neq_ {{N M}} %<- (label_neq (label_nat N) (label_nat M)) (nat_neq N M)

%sort label_less {_ label} {_ label}
%term label_less_ {{N M}} %<- (label_less (label_nat N) (label_nat M)) (nat_less N M)

%sort label_more {_ label} {_ label}
%term label_more_ {{N M}} %<- (label_more (label_nat N) (label_nat M)) (nat_more N M)

%sort trow
%term trow_nil trow
%term trow_cons {_ label} {_ tp} {_ trow} trow

%sort trow_lookup {_ label} {_ trow} {_ tp}
%term trow_lookup_yes {L label} {T tp} {TR trow} trow_lookup L (trow_cons L T TR) T
%term trow_lookup_no {L label} {L' label} {T tp} {T' tp} {TR trow} %<- (trow_lookup L (trow_cons L' T' TR) T) (trow_lookup L TR T)

%sort trow_labelfree {_ trow} {_ label}
%term trow_labelfree_nil {L label} trow_labelfree trow_nil L
%term trow_labelfree_cons {L label} {L' label} {TR trow} {T tp} %<- (trow_labelfree (trow_cons L' T TR) L) (label_neq L L') (trow_labelfree TR L)

%sort trow_eq {_ trow} {_ trow}
%term trow_eq_ {{TR}} trow_eq TR TR

%sort trow_order {_ trow} {_ trow}
%sort trow_insert {_ label} {_ tp} {_ trow} {_ trow}

%term trow_order_nil trow_order trow_nil trow_nil
%term trow_order_cons {L label} {S tp} {SR trow} {TR trow} {TR' trow} %<- (trow_order (trow_cons L S SR) TR') (trow_order SR TR) (trow_insert L S TR TR')

%term trow_insert_nil {L label} {S tp} trow_insert L S trow_nil (trow_cons L S trow_nil)
%term trow_insert_less {L label} {L' label} {S tp} {T tp} {TR trow} %<- (trow_insert L S (trow_cons L' T TR) (trow_cons L S (trow_cons L' T TR))) (label_less L L')
%term trow_insert_more {L label} {L' label} {S tp} {T tp} {TR trow} {TR' trow} %<- (trow_insert L S (trow_cons L' T TR) (trow_cons L' T TR')) (label_more L L') (trow_insert L S TR TR')

%sort trow_uniqueness {_ trow}
%term trow_uniqueness_nil trow_uniqueness trow_nil
%term trow_uniqueness_cons {L label} {TR trow} {T tp} %<- (trow_uniqueness (trow_cons L T TR)) (trow_labelfree TR L) (trow_uniqueness TR)

%term record {_ trow} {_ trow_uniqueness _} tp

%sort sub_trow {_ trow} {_ trow}
%term sub_trow_nil sub_trow trow_nil trow_nil
%term sub_trow_cons {L label} {S tp} {T tp} {SR trow} {TR trow} %<- (sub_trow (trow_cons L S SR) (trow_cons L T TR)) (sub_trow SR TR) (sub S T)
%term sub_trow_cons' {L label} {S tp} {SR trow} {TR trow} %<- (sub_trow (trow_cons L S SR) TR) (sub_trow SR TR)

%sort sub_tp {_ tp} {_ tp}
%sort sub_tp_trow {_ trow} {_ trow}

%term sub_tp_top {{T}} sub_tp T top
%term sub_tp_refl {{T}} sub_tp T T
%term sub_tp_trans {{T1 T2 T3}} %<- (sub_tp T1 T3) (sub_tp T1 T2) (sub_tp T2 T3)
%term sub_tp_arrow {{S1 S2 T1 T2}} %<- (sub_tp (arrow S1 S2) (arrow T1 T2)) (sub_tp T1 S1) (sub_tp S2 T2)
%term sub_tp_forall {{S1 S2 T1 T2}} %<- (sub_tp (forall S1 S2) (forall T1 T2)) (sub_tp T1 S1) ({x tp} {_ sub_tp x T1} sub_tp (S2 x) (T2 x))
%term sub_tp_record {SR trow} {TR trow} {SR' trow} {TR' trow} {SRuniq trow_uniqueness SR} {TRuniq trow_uniqueness TR} %<- (sub_tp (record SR SRuniq) (record TR TRuniq)) (trow_order SR SR') (trow_order TR TR') (sub_tp_trow SR' TR')

%term sub_tp_trow_nil sub_tp_trow trow_nil trow_nil
%term sub_tp_trow_cons {L label} {S tp} {T tp} {SR trow} {TR trow} %<- (sub_tp_trow (trow_cons L S SR) (trow_cons L T TR)) (sub_tp_trow SR TR) (sub_tp S T)
%term sub_tp_trow_cons' {L label} {S tp} {SR trow} {TR trow} %<- (sub_tp_trow (trow_cons L S SR) TR) (sub_tp_trow SR TR)
|}

(* POPLMARK-2B: System Fw with records — syntax-only extension of 1b.
   Ported from twelf/examples/poplmark/2b.elf (syntax block only).
   Depends on POPLMARK-1B's nat/label/trow/sub_tp/sub_tp_trow in global scope.
   Adds: term sorts (term/bterm/erow/pattern/prow), basic constructors.
   Dropped: all proof families (typing, step, progress, preservation).
*)
let poplmark_2b_syntax =
  {|
%sort term
%sort bterm
%sort erow
%sort pattern
%sort prow

%term base {_ term} bterm
%term bnd {_ tp} {_ {_ term} bterm} bterm

%term abs {_ tp} {_ {_ term} term} term
%term app {_ term} {_ term} term
%term tabs {_ tp} {_ {_ tp} term} term
%term tapp {_ term} {_ tp} term

%term rec {_ erow} term
%term proj {_ term} {_ label} term
%term plet {_ pattern} {_ term} {_ bterm} term

%term erow_nil erow
%term erow_cons {_ label} {_ term} {_ erow} erow

%sort erow_lookup {_ label} {_ erow} {_ term}
%term erow_lookup_yes {L label} {E term} {ER erow} erow_lookup L (erow_cons L E ER) E
%term erow_lookup_no {L label} {L' label} {E term} {E' term} {ER erow} %<- (erow_lookup L (erow_cons L' E' ER) E) (erow_lookup L ER E)

%sort erow_order {_ erow} {_ erow}
%sort erow_insert {_ label} {_ term} {_ erow} {_ erow}

%term erow_order_nil erow_order erow_nil erow_nil
%term erow_order_cons {L label} {E term} {ER erow} {ER' erow} {ER'' erow} %<- (erow_order (erow_cons L E ER) ER') (erow_order ER ER'') (erow_insert L E ER'' ER')

%term erow_insert_nil {L label} {E term} erow_insert L E erow_nil (erow_cons L E erow_nil)
%term erow_insert_less {L label} {L' label} {E term} {E' term} {ER erow} %<- (erow_insert L E (erow_cons L' E' ER) (erow_cons L E (erow_cons L' E' ER))) (label_less L L')
%term erow_insert_more {L label} {L' label} {E term} {E' term} {ER erow} {ER' erow} %<- (erow_insert L E (erow_cons L' E' ER) (erow_cons L' E' ER')) (label_more L L') (erow_insert L E ER ER')

%term pat_var {_ tp} pattern
%term pat_rec {_ prow} pattern

%term prow_nil prow
%term prow_cons {_ label} {_ pattern} {_ prow} prow
|}

(* CCC: Cartesian closed category syntax.
   Ported from twelf/examples/ccc/ccc.elf.
   Renamed: `1` (terminal obj) → `unit_obj`, `*` (product) → `prod`, `@` → `comp`,
   `==` (morphism equality) → `meq`, `=>` (exponential) → `exp_obj`.
   Note: no %prec declarations — all operators used in prefix form to avoid
   "Leading infix operator" parse errors that occur when infix is mixed with prefix.
*)
let ccc_syntax =
  {|
%sort obj
%sort mor {_ obj} {_ obj}
%sort meq {_ mor _A _B} {_ mor _A _B}

%term id {{A}} mor A A
%term comp {{A B C}} {_ mor B C} {_ mor A B} mor A C

%term meq_refl {{A B F}} meq F F
%term meq_then {{A B F F' F''}} {_ meq F F'} {_ meq F' F''} meq F F''
%term meq_sym {{A B F F'}} {_ meq F F'} meq F' F

%term eq_comp {{A B C F F' G G'}} {_ meq F F'} {_ meq G G'} meq (comp F G) (comp F' G')

%term id_l {{A B F}} meq (comp id F) F
%term id_r {{A B F}} meq (comp F id) F
%term assoc {{A B C D F G H}} meq (comp H (comp G F)) (comp (comp H G) F)

%term unit_obj obj
%term prod {_ obj} {_ obj} obj

%term drop {{A}} mor A unit_obj
%term fst {{A B}} mor (prod A B) A
%term snd {{A B}} mor (prod A B) B
%term pair_mor {{A B C}} {_ mor A B} {_ mor A C} mor A (prod B C)

%term eq_pair {{A B C F F' G G'}} {_ meq F F'} {_ meq G G'} meq (pair_mor F G) (pair_mor F' G')
%term prod_l {{A B C F G}} meq (comp fst (pair_mor F G)) F
%term prod_r {{A B C F G}} meq (comp snd (pair_mor F G)) G
%term prod_u {{A B C H}} meq (pair_mor (comp fst H) (comp snd H)) H

%term exp_obj {_ obj} {_ obj} obj

%term app_mor {{B C}} mor (prod (exp_obj B C) B) C
%term cur {{A B C}} {_ mor (prod A B) C} mor A (exp_obj B C)

%term eq_cur {{A B C F F'}} {_ meq F F'} meq (cur F) (cur F')
%term exp_e {{A B C F}} meq (comp app_mor (pair_mor (comp (cur F) fst) snd)) F
%term exp_u {{A B C G}} meq (cur (comp app_mor (pair_mor (comp G fst) snd))) G
|}

(* INCLL: INCLL sorts and terms.
   Ported from twelf/examples/incll/incll.elf.
   Renamed: numeric term names `1..5` → `v1..v5` (STELF identifiers must not start with digit).
   Dropped: Twelf abbreviation syntax (`<= = [x][y] y => x` etc.).
   Dropped: `|` infix list cons (single-char identifier, may fail — using `cons` instead).
   `^` prefix also risky — using `atm_frm` instead.
*)
let incll_syntax =
  {|
%sort sort
%term arrow {_ sort} {_ sort} sort
%term cross {_ sort} {_ sort} sort

%sort trm {_ sort}
%term app {{A B}} {_ trm (arrow A B)} {_ trm A} trm B
%term lam {{A B}} {_ {_ trm A} trm B} trm (arrow A B)
%term pair {{A B}} {_ trm A} {_ trm B} trm (cross A B)

%sort eval {_ trm _A} {_ trm _A}
%term eval_lam {{A B E}} eval (lam E) (lam E)
%term eval_app {{A B E E' V V'}} %<- (eval (app (lam E) E') V) (eval E' V') (eval (E V') V)

%sort atm
%sort frm
%term int sort

%term v1 trm int
%term v2 trm int
%term v3 trm int
%term v4 trm int
%term v5 trm int

%sort list_sort {_ sort}
%term list_sort/nil {{A}} list_sort A
%term list_sort/cons {{A}} {_ trm A} {_ list_sort A} list_sort A

%sort frm_atm {_ atm}
%term atm_frm {_ atm} frm

%sort imp {_ frm} {_ frm}
%term imp_i {_ frm} {_ frm} frm

%term forall {A sort} {_ {_ trm A} frm} frm
%term forall2 {A1 sort} {A2 sort} {_ {_ {_ trm A1} trm A2} frm} frm
|}

(* CRARY-LINEAR (syntax-only): linear substructural types.
   Ported from twelf/examples/crary/substruct/linear.elf (syntax block only).
   Type `!` in Twelf = the bang/of-course modality. Trying `!` as identifier verbatim.
   `with` is a keyword in OCaml but it's inside a raw string — should be fine in the parser.
*)
let crary_linear_syntax =
  {|
%sort atom
%sort tp
%sort term

%term atomic {_ atom} tp
%term lolli {_ tp} {_ tp} tp
%term tensor {_ tp} {_ tp} tp
%term with {_ tp} {_ tp} tp
%term plus {_ tp} {_ tp} tp
%term one tp
%term zero tp
%term top tp
%term bang {_ tp} tp

%term llam {_ {_ term} term} term
%term lapp {_ term} {_ term} term

%term tpair {_ term} {_ term} term
%term lett {_ term} {_ {_ term} {_ term} term} term

%term pair {_ term} {_ term} term
%term pi1 {_ term} term
%term pi2 {_ term} term

%term in1 {_ term} term
%term in2 {_ term} term
%term case {_ term} {_ {_ term} term} {_ {_ term} term} term

%term star term
%term leto {_ term} {_ term} term

%term any {_ term} term
%term unit term

%term bang_tm {_ term} term
%term letb {_ term} {_ {_ term} term} term

%term a atom
%term b atom
|}

(* CRARY-LINEAR (linearity sort and basic terms):
   The `linear` family from linear.elf.
*)
let crary_linear_linear =
  {|
%sort linear {_ {_ term} term}
%term linear/var linear ([x] x)
%term linear/llam {{M}} {_ {y term} linear ([x] M x y)} linear ([x] llam ([y] M x y))
%term linear/lapp1 {{M N}} {_ linear ([x] M x)} linear ([x] lapp (M x) N)
%term linear/lapp2 {{M N}} {_ linear ([x] N x)} linear ([x] lapp M (N x))
%term linear/tpair1 {{M N}} {_ linear ([x] M x)} linear ([x] tpair (M x) N)
%term linear/tpair2 {{M N}} {_ linear ([x] N x)} linear ([x] tpair M (N x))
%term linear/pair {{M N}} {_ linear ([x] M x)} {_ linear ([x] N x)} linear ([x] pair (M x) (N x))
%term linear/pi1 {{M}} {_ linear ([x] M x)} linear ([x] pi1 (M x))
%term linear/pi2 {{M}} {_ linear ([x] M x)} linear ([x] pi2 (M x))
%term linear/in1 {{M}} {_ linear ([x] M x)} linear ([x] in1 (M x))
%term linear/in2 {{M}} {_ linear ([x] M x)} linear ([x] in2 (M x))
%term linear/unit linear ([x] unit)
|}

(* CRARY-LINEARD (syntax-only): dual linear type system.
   Ported from twelf/examples/crary/substruct/lineard.elf (syntax only).
   Similar to linear.elf but adds `constant`, `pi` (dependent type), `ulam`/`uapp`.
*)
let crary_lineard_syntax =
  {|
%sort constant
%sort tp
%sort term

%term const {_ constant} {_ term} tp
%term pi {_ tp} {_ {_ term} tp} tp
%term lolli {_ tp} {_ tp} tp
%term tensor {_ tp} {_ tp} tp
%term with {_ tp} {_ tp} tp
%term plus {_ tp} {_ tp} tp
%term one tp
%term zero tp
%term top tp
%term bang {_ tp} tp

%term ulam {_ {_ term} term} term
%term uapp {_ term} {_ term} term

%term llam {_ {_ term} term} term
%term lapp {_ term} {_ term} term

%term tpair {_ term} {_ term} term
%term lett {_ term} {_ {_ term} {_ term} term} term

%term pair {_ term} {_ term} term
%term pi1 {_ term} term
%term pi2 {_ term} term

%term in1 {_ term} term
%term in2 {_ term} term
%term case {_ term} {_ {_ term} term} {_ {_ term} term} term

%term star term
%term leto {_ term} {_ term} term

%term any {_ term} term
%term unit term

%term bang_tm {_ term} term
%term letb {_ term} {_ {_ term} term} term
|}

(* CRARY-MODAL (syntax-only): modal substructural types.
   Ported from twelf/examples/crary/substruct/modal.elf (syntax block only).
   Dropped: `local/bx : local ([x] bx M) = local/closed.` definitional equality form.
*)
let crary_modal_syntax =
  {|
%sort atom
%sort tp
%sort term
%sort exp

%term atomic {_ atom} tp
%term arrow {_ tp} {_ tp} tp
%term box {_ tp} tp
%term diamond {_ tp} tp

%term lam {_ {_ term} term} term
%term app {_ term} {_ term} term

%term bx {_ term} term
%term letbx {_ term} {_ {_ term} term} term

%term di {_ exp} term

%term here {_ term} exp
%term eletbx {_ term} {_ {_ term} exp} exp
%term letdi {_ term} {_ {_ term} exp} exp

%term a atom
%term b atom
|}
