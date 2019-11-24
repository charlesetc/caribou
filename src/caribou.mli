(** Caribou *)

module type App = sig
  type item

  val show : item -> selected:bool -> Notty.image

  val inspect : item -> Notty.image

  val list : unit -> item list
end

module type Display

module Display : sig
  module type S

  module Fullscreen : S

  module Tty : S
end

module Make (A : App) (D : Display.S) : sig
  val run : unit -> unit Lwt.t
end

module Tree : sig
  module Context : sig
    type t = {children : Notty.image; selected : bool; collapsed : bool}
  end

  module type App = sig
    type item

    val show : Context.t -> item -> Notty.image

    val children : item -> item list

    val inspect : item -> Notty.image

    val list : unit -> item list
  end

  module Make (A : App) (D : Display.S) : sig
    val run : unit -> unit Lwt.t
  end
end

module Notty_helpers : module type of Notty_helpers

module Debug : module type of Debug
