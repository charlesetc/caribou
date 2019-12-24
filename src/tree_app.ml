open Import
open Base

module type S = App.Tree

module Make (D : Display.S) (A : S) = struct
  module Cursor = Cursor.Make (A) (D)

  module Input_overlay = struct
    (* In the future: Search as well *)
    type t = Filter
  end

  type t = {
    mutable cursor : int list;
    mutable scroll : int;
    items : A.item list;
    (* TODO: make this filter better *)
    filter : string option;
    input_overlay : Input_overlay.t option;
  }

  let init () =
    {
      scroll = 0;
      items = A.list ();
      cursor = [ 0 ];
      filter = None;
      input_overlay = None;
    }

  let image_of_items t items =
    let rev_cursor = List.rev t.cursor in
    let rec lp items cursor_base =
      List.fold items ~init:(0, I.empty) ~f:(fun (i, acc) item ->
          let position = i :: cursor_base in
          let selected = List.equal Int.equal position rev_cursor in
          let children = lp (A.children item) position |> snd in
          let image = A.image_of_item ~children ~selected item in
          (i + 1, acc <-> image))
    in
    lp items [] |> snd

  let show t = image_of_items t t.items

  let scroll t image = I.vcrop (-1 * t.scroll) (-1) image

  let quit () =
    let+ () = D.uninitialize () in
    Caml.exit 0

  let update t (action : A.item Action.t) =
    let item = Cursor.index t.items t.cursor in
    match action with
    | `Scroll_up ->
        t.scroll <- t.scroll + 1;
        Lwt.return `Continue
    | `Scroll_down ->
        t.scroll <- t.scroll - 1;
        Lwt.return `Continue
    | `Page_up ->
        t.scroll <- t.scroll + 10;
        Lwt.return `Continue
    | `Page_down ->
        t.scroll <- t.scroll - 10;
        Lwt.return `Continue
    | `Quit -> quit ()
    | `Cursor_down ->
        t.cursor <- Cursor.increment t.items t.cursor;
        Lwt.return `Continue
    | `Cursor_up ->
        t.cursor <- Cursor.decrement t.items t.cursor;
        Lwt.return `Continue
    | `Custom f -> (
        try
          let+ () = f item in
          `Continue
        with e ->
          let+ () = D.uninitialize () in
          raise e )
    | `Back -> Lwt.return `Finished

  let render t =
    let image = show t in
    let image = scroll t image in
    D.render image

  let action_of_event event =
    match event with
    | `Key (key, mods) ->
        List.find (A.bindings @ Action.default_bindings)
          ~f:(fun (key', mods', _) ->
            Key.equal key key' && Key.equal_mods mods mods')
        |> Option.map ~f:(fun (_, _, action) -> action)
    | _ -> None

  let run () =
    let state = init () in
    (* display the initial screen *)
    let* () = render state in
    D.grab_events (fun event ->
        ( match action_of_event event with
        | Some action -> update state action
        | None -> Lwt.return `Continue )
        |>* fun () -> render state)
end
