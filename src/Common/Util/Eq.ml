type (_, _) eq = Refl : ('a, 'a) eq

module type EQ = sig
  type t
  type u

  val prf : (t, u) eq
end

module Refl (M : sig
  type t
end) : EQ with type t = M.t and type u = M.t = struct
  type t = M.t
  type u = M.t

  let prf = Refl
end

module Symm (E : EQ) : EQ with type t = E.u and type u = E.t = struct
  type t = E.u
  type u = E.t

  let prf : (t, u) eq = match E.prf with Refl -> Refl
end

module Tran (AB : EQ) (BC : EQ with type t = AB.u) :
  EQ with type t = AB.t and type u = BC.u = struct
  type t = AB.t
  type u = BC.u

  let prf : (t, u) eq = match (AB.prf, BC.prf) with Refl, Refl -> Refl
end

module Cast (E : EQ) : sig
  val cast : E.t -> E.u
end = struct
  let cast (x : E.t) : E.u = match E.prf with Refl -> x
end
