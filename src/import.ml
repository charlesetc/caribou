open Lwt
module I = Notty.I
module Attr = Notty.A
include Notty.Infix

let ( let* ) = ( >>= )

let ( let+ ) = ( >|= )
