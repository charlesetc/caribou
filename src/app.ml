(* Maybe call these configs? *)
module type Tree = sig
  type item [@@deriving show]

  val image_of_item :
    children:Notty.image -> selected:bool -> item -> Notty.image

  val children : item -> item list

  val list : unit -> item list

  val bindings : (Key.t * Key.mods * item Action.t) list
end

module type Const = sig
  val image : Notty.image
end
