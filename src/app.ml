module type S = sig
  type item

  val show : children:Notty.image -> selected:bool -> item -> Notty.image

  val children : item -> item list

  val bindings : (Action.t * (item -> unit Lwt.t)) list

  val list : unit -> item list
end
