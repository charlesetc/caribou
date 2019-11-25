open Import
open Base

module Make (A : App.S) (D : Display.S) = struct
  module Cursor = Cursor.Make (A) (D)

  module View = struct
    type t = List | Show of {scroll : int; item : A.item}

    module List = struct
      type t = {scroll : int; items : A.item list}
    end
  end

  type t =
    { mutable view : View.t
    ; mutable list : View.List.t
          (** We always keep a List around since we want to go back to the same one. *)
    ; mutable cursor : int list
    ; mutable scroll : int
    ; display : D.t }

  let init ~display =
    { view = List
    ; scroll = 0
    ; list = {scroll = 0; items = A.list ()}
    ; cursor = [0]
    ; display }

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

  let show t =
    match t.view with
    | List ->
        t.scroll <- t.list.scroll ;
        image_of_items t t.list.items
    | Show {item; scroll} ->
        t.scroll <- scroll ;
        A.inspect item

  let scroll t image = I.vcrop (-1 * t.scroll) (-1) image

  let update t (action : Action.t) =
    match (t.view, action) with
    | _, Scroll_up ->
        Lwt.return @@ (t.scroll <- t.scroll + 1)
    | _, Scroll_down ->
        Lwt.return @@ (t.scroll <- t.scroll - 1)
    | _, Page_up ->
        Lwt.return @@ (t.scroll <- t.scroll + 10)
    | _, Page_down ->
        Lwt.return @@ (t.scroll <- t.scroll - 10)
    | _, Quit ->
        D.quit t.display
    | List, Cursor_down ->
        let cursor = Cursor.increment t.list.items t.cursor in
        Debug.log "cursor %s" (Cursor.string_of_cursor cursor) ;
        Lwt.return @@ (t.cursor <- cursor)
    | List, Cursor_up ->
        let cursor = Cursor.decrement t.list.items t.cursor in
        Debug.log "cursor %s" (Cursor.string_of_cursor cursor) ;
        Lwt.return @@ (t.cursor <- cursor)
    | List, Chose_cursor ->
        let item = Cursor.index t.list.items t.cursor in
        Lwt.return @@ (t.view <- Show {item; scroll = 0})
    | List, Back ->
        D.quit t.display
    | Show _, Back ->
        t.view <- List ;
        t.scroll <- t.list.scroll ;
        Lwt.return ()
    | Show _, _ ->
        (* unable to handle event... ? *)
        Lwt.return ()

  let render t =
    let image = show t in
    let image = scroll t image in
    D.render t.display image
end
