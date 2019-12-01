(** Caribou *)

module Display : sig
  module type S

  module Fullscreen : S

  module Tty : S
end

(* module Const : sig
  module type App = sig
    val image : Notty.image
  end

  module Make (A : App) (D : Display.S) : sig
    val run : unit -> unit Lwt.t
  end
end *)

module List : sig
  module type App = sig
    type item

    val show : item -> selected:bool -> Notty.image

    val list : unit -> item list

    val bindings : (Action.t * (item -> unit Lwt.t)) list
  end

  module Make (A : App) (D : Display.S) : sig
    val run : unit -> unit Lwt.t
  end
end

module Tree : sig
  module type App = sig
    type item

    val show : children:Notty.image -> selected:bool -> item -> Notty.image

    val children : item -> item list

    val list : unit -> item list

    val bindings : (Action.t * (item -> unit Lwt.t)) list
  end

  module Make (A : App) (D : Display.S) : sig
    val run : unit -> unit Lwt.t
  end
end

(** Additional functions provided for convenience. Enjoy! *)
module Ext : sig
  (** Notty-related *)
  module Notty : sig
    val image_of_string : Notty.attr -> string -> Notty.image
  end

  (** Unix-related *)
  module Unix : sig
    module Process_status : sig
      type t = Unix.process_status =
        | WEXITED of int
        | WSIGNALED of int
        | WSTOPPED of int

      val to_string : t -> string
    end

    val exec : string -> string list -> Process_status.t
  end
end

module Debug :  sig
  val log : ('a, unit, string, unit) format4 -> 'a
end
