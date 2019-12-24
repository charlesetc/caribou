open Base

module type S = sig
  type item [@@deriving show]

  val image_of_item : item -> selected:bool -> Notty.image

  val list : unit -> item list

  val bindings : (Key.t * Key.mods * item Action.t) list
end

module Make (D : Display.S) (A : S) = struct
  module App : Tree_app.S = struct
    include A

    let image_of_item ~children:_ ~selected item = image_of_item ~selected item

    let children _ = []
  end

  module Made = Tree_app.Make (D) (App)

  let run = Made.run
end
