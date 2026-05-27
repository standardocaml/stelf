(** {1 Display Handlers}

    This is the display handling library, which handles queued messages and
    their display Note that this does not handle the actual formatting of the
    messages, but rather the message queue *)

module type DISPLAY = DISPLAY.DISPLAY
module type S = DISPLAY.S

open Lwt.Syntax

module Display : S = struct
  type t = Info.t array Lwt.t ref

  let setup () : t = ref (Lwt.return [||])

  let flush (d : t) () : Info.t array Lwt.t =
    let* pending = !d in
    let* () = Lwt.return (d := Lwt.return [||]) in
    Lwt.return pending

  let display (d : t) (info : Info.t) : unit =
    ignore
    @@
    let* pending = !d in
    let* () = Lwt.return (d := Lwt.return (Array.append pending [| info |])) in
    Lwt.return ()
end

module Info = Info
include Form

module Make_Display (D : S) : DISPLAY = struct
  include D

  let global : t = setup ()
  let setup' () : unit = ignore @@ global
  let flush' () : Info.t array Lwt.t = flush global ()
  let display' (info : Info.t) : unit = display global info

  let message ?src ?kind ?level (msg : Form.t) : unit =
    let info = Info.msg ?src ?kind ?level msg in
    display' info

  let debug ?src ?level (msg : Form.t) : unit =
    message ?src ?level ~kind:Info.Debug msg

  let info ?src ?level (msg : Form.t) : unit =
    message ?src ?level ~kind:Info.Info msg

  let warning ?src ?level (msg : Form.t) : unit =
    message ?src ?level ~kind:Info.Warning msg

  let error ?src ?level (msg : Form.t) : unit =
    message ?src ?level ~kind:Info.Error msg

  let response ?src ?level (msg : Form.t) : unit =
    message ?src ?level ~kind:Info.Response msg
end

include Make_Display (Display)
