(** Caribou *)

module Display : sig
  module type S

  module Fullscreen () : S

  module Tty () : S
end

module Key : sig
  type mods = [ `Meta | `Ctrl | `Shift ] list

  type t =
    [ `Escape
    | `Enter
    | `Tab
    | `Backspace
    | `Insert
    | `Delete
    | `Home
    | `End
    | `Arrow of [ `Up | `Down | `Left | `Right ]
    | `Page of [ `Up | `Down ]
    | `Function of int
    | `Uchar of Uchar.t
    | `ASCII of char ]
  [@@deriving sexp, eq]
end

module Action : sig
  type 'item t =
    [ `Cursor_down
    | `Cursor_up
    | `Back
    | `Quit
    | `Scroll_up
    | `Scroll_down
    | `Page_up
    | `Page_down
    | `Custom of 'item -> unit Lwt.t ]
  [@@deriving sexp]

  val default_bindings : (Key.t * Key.mods * 'a t) list
end

module App (D : Display.S) : sig
  val const : Notty.image -> unit Lwt.t

  module List : sig
    module type S = sig
      type item [@@deriving show]

      val image_of_item : item -> selected:bool -> Notty.image

      val list : unit -> item list

      val bindings : (Key.t * Key.mods * item Action.t) list
    end

    module Make (A : S) : sig
      val run : unit -> unit Lwt.t
    end
  end

  module Tree : sig
    module type S = sig
      type item [@@deriving show]

      val image_of_item :
        children:Notty.image -> selected:bool -> item -> Notty.image

      val children : item -> item list

      val list : unit -> item list

      val bindings : (Key.t * Key.mods * item Action.t) list
    end

    module Make (A : S) : sig
      val run : unit -> unit Lwt.t
    end
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

    val exec :
      (module Display.S) -> string -> string list -> Process_status.t Lwt.t
  end
end

module Debug : sig
  val log : ('a, unit, string, unit) format4 -> 'a
end
