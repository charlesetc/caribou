open Import
open Base

module Make (A : App.S) (D : Display.S) = struct
  module Cursor = Cursor.Make (A) (D)

  type t =
    { mutable cursor : int list
    ; mutable scroll : int
    ; items : A.item list
    ; display : D.t }

  let init ~display = {scroll = 0; items = A.list (); cursor = [0]; display}

  let image_of_items t items =
    let rev_cursor = List.rev t.cursor in
    let rec lp items cursor_base =
      List.fold items ~init:(0, I.empty) ~f:(fun (i, acc) item ->
          let position = i :: cursor_base in
          let selected = List.equal Int.equal position rev_cursor in
          let children = lp (A.children item) position |> snd in
          let image = A.show ~children ~selected item in
          (i + 1, acc <-> image))
    in
    lp items [] |> snd

  let show t = image_of_items t t.items

  let scroll t image = I.vcrop (-1 * t.scroll) (-1) image

  let quit t =
    let+ () = D.uninitialize t.display in
    Caml.exit 0

  let update t (action : Action.t) =
    (* TODO: eventually make these expose these all
     * as functions and provide a default keymapping. *)
    match action with
    | `Scroll_up ->
        Lwt.return @@ (t.scroll <- t.scroll + 1)
    | `Scroll_down ->
        Lwt.return @@ (t.scroll <- t.scroll - 1)
    | `Page_up ->
        Lwt.return @@ (t.scroll <- t.scroll + 10)
    | `Page_down ->
        Lwt.return @@ (t.scroll <- t.scroll - 10)
    | `Quit ->
        quit t
    | `Cursor_down ->
        let cursor = Cursor.increment t.items t.cursor in
        Lwt.return @@ (t.cursor <- cursor)
    | `Cursor_up ->
        let cursor = Cursor.decrement t.items t.cursor in
        Lwt.return @@ (t.cursor <- cursor)
    | `Choose_cursor -> (
        let item = Cursor.index t.items t.cursor in
        let f =
          List.Assoc.find_exn A.bindings ~equal:Action.equal `Choose_cursor
        in
        try f item
        with e ->
          let+ () = D.uninitialize t.display in
          raise e )
    | `Back ->
        quit t

  let render t =
    let image = show t in
    let image = scroll t image in
    D.render t.display image
end
