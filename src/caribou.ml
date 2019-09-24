module type Caribou_app = sig
  type model

  type id

  val id : model -> id

  val view : model -> Notty.image

  val get : id -> model option

  val list : unit -> model list
end

module Make (A : Caribou_app) = struct
  let run = raise (Failure "unimplemented")
end
