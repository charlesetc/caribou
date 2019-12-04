open Import
open Base

(* Can this share [update] with tree_state maybe? *)

module Make (A : App.Const) (D : Display.S) = struct
  type t = { mutable scroll : int; image : Notty.image; display : D.t }

  let init ~display image = { scroll = 0; image; display }

  let show t = t.image

  let scroll t image = I.vcrop (-1 * t.scroll) (-1) image

  let quit t =
    let+ () = D.uninitialize t.display in
    Caml.exit 0

  let update t (action : Action.t) =
    match action with
    | `Scroll_up -> Lwt.return @@ (t.scroll <- t.scroll + 1)
    | `Scroll_down -> Lwt.return @@ (t.scroll <- t.scroll - 1)
    | `Page_up -> Lwt.return @@ (t.scroll <- t.scroll + 10)
    | `Page_down -> Lwt.return @@ (t.scroll <- t.scroll - 10)
    | `Quit -> quit t
    | `Back -> quit t
    | _ -> raise (Failure "unimplemented")

  let render t =
    let image = show t in
    let image = scroll t image in
    D.render t.display image
end
