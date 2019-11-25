module type S = sig
  type item

  val show : children:Notty.image -> selected:bool -> item -> Notty.image

  val children : item -> item list

  val inspect : item -> Notty.image

  val list : unit -> item list
end
