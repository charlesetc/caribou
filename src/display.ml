module type S = sig
  type t

  val init : unit -> t

  val uninitialize : t -> unit Lwt.t

  val render : t -> Notty.image -> unit Lwt.t

  val events : t -> [ Notty.Unescape.event | `Resize of int * int ] Lwt_stream.t
end
