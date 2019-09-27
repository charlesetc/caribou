open Lwt_utils
open Base
module I = Notty.I
module Attr = Notty.A
open Notty.Infix
open! Debug

module type Caribou_app = sig
  type item

  type id

  val id : item -> id

  val view : item -> selected:bool -> Notty.image

  val inspect : item -> Notty.image

  val get : id -> item option

  val list : unit -> item list
end

module Action = struct
  type t = Cursor_down | Cursor_up | Chose_cursor | Back

  (* it could be fun to make this more modal at some point *)
  let of_event : [Notty.Unescape.event | `Resize of int * int] -> t option =
    function
    | `Key (`ASCII 'k', []) | `Key (`Arrow `Up, []) ->
        Some Cursor_up
    | `Key (`ASCII 'j', []) | `Key (`Arrow `Down, []) ->
        Some Cursor_down
    | `Key (`Enter, []) ->
        Some Chose_cursor
    | `Key (`Escape, []) ->
        Some Back
    | `Key (`ASCII 'q', []) ->
        Caml.exit 0
    | _ ->
        None
end

module State (A : Caribou_app) = struct
  type t = List of {cursor : int; items : A.item list} | Show of A.item

  let update t (action : Action.t) =
    match (t, action) with
    | List {cursor; items}, Cursor_down ->
        let length = List.length items in
        let cursor = if cursor >= length - 1 then length - 1 else cursor + 1 in
        List {cursor; items}
    | List {cursor; items}, Cursor_up ->
        let cursor = if cursor <= 0 then 0 else cursor - 1 in
        List {cursor; items}
    | List {cursor; items}, Chose_cursor ->
        let chosen = List.nth_exn items cursor in
        Show chosen
    | List _, Back ->
        Caml.exit 0
    | Show _, Back ->
        (* maybe do better than this.. keep track of cursor *)
        List {cursor = 0; items = []}
    | Show _, _ ->
        t

  let view = function
    | List {cursor; items = _} ->
        let items = A.list () in
        let _, image =
          List.fold items ~init:(0, I.empty) ~f:(fun (i, acc) item ->
              (i + 1, acc <-> A.view item ~selected:(i = cursor)))
        in
        (image, List {cursor; items})
    | Show item ->
        (A.inspect item, Show item)
end

module Make (A : Caribou_app) = struct
  module State = State (A)

  let state : State.t ref = ref (State.List {cursor = 0; items = []})

  let update_view term =
    let view, state' = State.view !state in
    state := state' ;
    Notty_lwt.Term.image term view

  let handle_event event =
    match Action.of_event event with
    | Some action ->
        state := State.update !state action
    | None ->
        ()

  let run () =
    let term = Notty_lwt.Term.create () in
    let* () = update_view term in
    let* () =
      Lwt_stream.iter_s
        (fun event -> handle_event event ; update_view term)
        (Notty_lwt.Term.events term)
    in
    Lwt_unix.sleep 2.0
end
