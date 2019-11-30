open! Import
open! Base

type t = Notty_lwt.Term.t

let init () = Notty_lwt.Term.create ~mouse:false ()

let uninitialize _ = Lwt.return ()

let render t image = Notty_lwt.Term.image t image

let events t = Notty_lwt.Term.events t
