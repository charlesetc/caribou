module type Tree = sig
  type item

  val show : children:Notty.image -> selected:bool -> item -> Notty.image

  val children : item -> item list

  val list : unit -> item list

  val bindings :
    (Key.t * Key.mods * [ Action.t | `Custom of item -> unit ]) list
end

module type Const = sig
  val image : Notty.image
end
