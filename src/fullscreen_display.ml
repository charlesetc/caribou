open! Import
open! Base

module M () = struct
  type t = Notty_lwt.Term.t

  let global = Notty_lwt.Term.create ~mouse:false ()

  let uninitialize () = Lwt.return ()

  let reinitialize () = Lwt.return ()

  let render image = Notty_lwt.Term.image global image

  let events () = Notty_lwt.Term.events global
end
