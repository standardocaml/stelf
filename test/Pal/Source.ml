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

%term f1 {{A}} {T i} hil ((forall [x i] A x) imp (A T))
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

%term hnd_n1 {{A B}} hilnd n1 (impi [u] impi [v] noti [p] [w] note (impe u w) p (impe v w))

%term hnd_n2 {{A B}} hilnd n2 (impi [u] impi [v] note u B v)

%term hnd_f1 {{A}} {T i} hilnd (f1 T) (impi [u] foralle u T)

%term hnd_f2 {{A B}} hilnd f2 (impi [u] impi [v] foralli [a] impe (foralle u a) v)

%term hnd_mp {{A B}} {H1 hil (A imp B)} {H2 hil A} {D1 nd (A imp B)} {D2 nd A} {_ hilnd H1 D1} {_ hilnd H2 D2} hilnd (mp H1 H2) (impe D1 D2)

%term hnd_ug {{A}} {H1 {a i} hil (A a)} {D1 {a i} nd (A a)} {_ {a i} hilnd (H1 a) (D1 a)} hilnd (ug H1) (foralli D1)
|}
(* %mode {%in x hil _A} {%out y nd _A} hilnd x y *)
let fol4_2 = {|
%mode hilnd %in %out
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
let fol5 =
  {|

The deduction theorem for Hilbert derivations

%sort ded {_ {_ hil A} hil B} {_ hil (A imp B)}

%term ded_id {{A}} ded ([u] u) (mp (mp s k) k)

%term ded_k {{A B}} ded ([u] k) (mp k k)
%term ded_s {{A B C}} ded ([u] s) (mp k s)

%term ded_n1 {{A B}} ded ([u] n1) (mp k n1)
%term ded_n2 {{A B}} ded ([u] n2) (mp k n2)

%term ded_f1 {{A B T}} ded ([u] f1 T) (mp k (f1 T))
%term ded_f2 {{A B}} ded ([u] f2) (mp k f2)

%term ded_mp {{A B C}} {H1 {_ hil A} hil (B imp C)} {H2 {_ hil A} hil B} {H1' hil (A imp (B imp C))} {H2' hil (A imp B)} {_ ded H1 H1'} {_ ded H2 H2'} ded ([u] mp (H1 u) (H2 u)) (mp (mp s H1') H2')

%term ded_ug {{A B}} {H1 {_ hil A} {_ i} hil _} {H1' {_ i} hil _} {_ {a i} ded ([u] H1 u a) (H1' a)} ded ([u] ug (H1 u)) (mp f2 (ug H1'))

%block lded [A o] {u nd A} {v hil A} {h {C o} ded ([w] v) (mp k v)}

%mode ded %in %out
%worlds (li lo lded) (ded H H')
%terminates H (ded H _)
%covers ded %in %out
%total H (ded H _)

|}

let fol6 =
  {|

Mapping natural deductions to Hilbert derivations.

%sort ndhil {_ nd A} {_ hil A}

%term ndh_impi {{A1 B}} {D1 {_ nd A1} nd B} {H1 {_ hil A1} hil B} {H1' hil (A1 imp B)} {_ ded H1 H1'} {_ {u nd A1} {v hil A1} {_ {C o} ded ([w] v) (mp k v)} {_ ndhil u v} ndhil (D1 u) (H1 v)} ndhil (impi D1) H1'

%term ndh_impe {{A B}} {D1 nd (A imp B)} {D2 nd A} {H1 hil (A imp B)} {H2 hil A} {_ ndhil D1 H1} {_ ndhil D2 H2} ndhil (impe D1 D2) (mp H1 H2)

%term ndh_noti {{A1}} {D1 {_ o} {_ nd A1} nd _} {H1 {_ o} {_ hil A1} hil _} {H1' hil _} {H1'' hil _} {_ ded (H1 A1) H1''} {_ ded (H1 (not A1)) H1'} {_ {p o} {u nd A1} {v hil A1} {_ {C o} ded ([w] v) (mp k v)} {_ ndhil u v} ndhil (D1 p u) (H1 p v)} ndhil (noti D1) (mp (mp n1 H1') H1'')

%term ndh_note {{A C}} {D1 nd (not A)} {D2 nd A} {H1 hil (not A)} {H2 hil A} {_ ndhil D1 H1} {_ ndhil D2 H2} ndhil (note D1 C D2) (mp (mp n2 H1) H2)

%term ndh_foralli {{A}} {D1 {_ i} nd _} {H1 {_ i} hil _} {_ {a i} ndhil (D1 a) (H1 a)} ndhil (foralli D1) (ug H1)

%term ndh_foralle {{A}} {T i} {D1 nd (forall A)} {H1 hil (forall A)} {_ ndhil D1 H1} ndhil (foralle D1 T) (mp (f1 T) H1)

%mode ndhil %in %out
%block lndhil [A o] {u nd A} {v hil A} {h {C o} ded ([w] v) (mp k v)} {nh ndhil u v}
%worlds (li lo lndhil) (ndhil D H)
%terminates D (ndhil D _)
%covers ndhil %in %out
%total D (ndhil D _)


|}

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
