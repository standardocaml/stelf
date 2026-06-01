let todo : 'a = (fun () -> Obj.magic ()) ()

module Todo
    (M : sig
      module type S
    end)
    () : M.S =
  (val Stdlib.Obj.magic () : M.S)
