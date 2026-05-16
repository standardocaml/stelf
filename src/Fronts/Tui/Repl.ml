module Repl : REPL.REPL = struct 
  let term : LTerm.t ref Lwt.t = Lwt.map ref @@ Lazy.force LTerm.stdout 
  let stop = assert false 
  let read = assert false
  let show = assert false
end 