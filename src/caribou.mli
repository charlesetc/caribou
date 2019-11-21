(** Caribou *)

module type Caribou_app = sig
  type item

  val show : item -> selected:bool -> Notty.image

  val inspect : item -> Notty.image

  val list : unit -> item list
end

module type Display

module Fullscreen_display : Display

module Tty_display : Display

module Make (A : Caribou_app) (D : Display) : sig
  val run : unit -> unit Lwt.t
end

module Notty_helpers : module type of Notty_helpers
