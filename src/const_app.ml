open Import
open Base

module type S = App.Const

module Make (D : Display.S) (App : App.Const) = struct
  type t = { mutable scroll : int; image : Notty.image }

  let init image = { scroll = 0; image }

  let show t = t.image

  let scroll t image = I.vcrop (-1 * t.scroll) (-1) image

  let quit () =
    let+ () = D.uninitialize () in
    Caml.exit 0

  (* TODO: Try to share some of logic with tree_app *)
  let update t (action : unit Action.t) =
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
    | `Back -> Lwt.return `Finished
    | `Custom _ | `Cursor_up | `Cursor_down -> Lwt.return `Continue

  let render t =
    let image = show t in
    let image = scroll t image in
    D.render image

  let action_of_event event =
    match event with
    | `Key (key, mods) ->
        List.find
          ~f:(fun (key', mods', _) ->
            Key.equal key key' && Key.equal_mods mods mods')
          Action.default_bindings
        |> Option.map ~f:(fun (_, _, action) -> action)
    | _ -> None

  let run () =
    let state = init App.image in
    (* display the initial screen *)
    let* () = render state in
    D.grab_events (fun event ->
        ( match action_of_event event with
        | Some action -> update state action
        | None -> Lwt.return `Continue )
        |>* fun () -> render state)
end
