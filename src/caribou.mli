module type Caribou_app = sig
  type item

  type id

  val id : item -> id

  val view : item -> selected:bool -> Notty.image

  val inspect : item -> Notty.image

  val get : id -> item option

  val list : unit -> item list
end

module Make (A : Caribou_app) : sig
  val run : unit -> unit Lwt.t
end
