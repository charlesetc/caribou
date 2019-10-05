open Lwt_utils
open Base
module I = Notty.I
module Attr = Notty.A
open Notty.Infix
open! Debug

let stdin = Stdio.stdin

(* let stdout = Stdio.stdout *)

module type Caribou_app = sig
  type item

  val show : item -> selected:bool -> Notty.image

  val inspect : item -> Notty.image

  val list : unit -> item list
end

module Action = struct
  type t = Cursor_down | Cursor_up | Chose_cursor | Back | Quit

  let of_event = function
    | `Key (`ASCII 'k', []) | `Key (`Arrow `Up, []) ->
        Some Cursor_up
    | `Key (`ASCII 'j', []) | `Key (`Arrow `Down, []) ->
        Some Cursor_down
    | `Key (`Enter, []) | `Key (`ASCII 'M', [`Ctrl]) ->
        Some Chose_cursor
    | `Key (`Escape, []) ->
        Some Back
    | `Key (`ASCII 'q', []) | `Key (`ASCII 'C', [`Ctrl]) ->
        Some Quit
    | _ ->
        None
end

module type Display = sig
  type t

  val init : unit -> t

  val quit : t -> 'a Lwt.t

  val update : t -> Notty.image -> unit Lwt.t

  val events : t -> [Notty.Unescape.event | `Resize of int * int] Lwt_stream.t
end

module State (A : Caribou_app) (D : Display) = struct
  module View = struct
    type t = List of A.item list | Show of A.item
  end

  type t = {mutable view : View.t; mutable cursor : int; display : D.t}

  let init ~display = {view = List []; cursor = 0; display}

  let show t =
    match t.view with
    | List _ ->
        (* this is weird *)
        let items = A.list () in
        t.view <- List items ;
        List.fold items ~init:(0, I.empty) ~f:(fun (i, acc) item ->
            (i + 1, acc <-> A.show item ~selected:(i = t.cursor)))
        |> snd
    | Show item ->
        A.inspect item

  let update t (action : Action.t) =
    match (t.view, action) with
    | _, Quit ->
        D.quit t.display
    | List items, Cursor_down ->
        let length = List.length items in
        Lwt.return
        @@ ( t.cursor <-
             (if t.cursor >= length - 1 then length - 1 else t.cursor + 1) )
    | List _, Cursor_up ->
        Lwt.return @@ (t.cursor <- (if t.cursor <= 0 then 0 else t.cursor - 1))
    | List items, Chose_cursor ->
        let chosen = List.nth_exn items t.cursor in
        Lwt.return @@ (t.view <- Show chosen)
    | List _, Back ->
        D.quit t.display
    | Show _, Back ->
        Lwt.return @@ (t.view <- List [])
    | Show _, _ ->
        (* unable to handle event... ? *)
        Lwt.return ()

  let render t = D.update t.display (show t)
end

module Fullscreen_display : Display = struct
  type t = Notty_lwt.Term.t

  let init () = Notty_lwt.Term.create ~mouse:false ()

  let quit _ = Caml.exit 0

  let update t image = Notty_lwt.Term.image t image

  let events t = Notty_lwt.Term.events t
end

module Tty_display : Display = struct
  type t = {mutable last_height : int option}

  let init () =
    Tty.echo false ; Tty.raw true ; Tty.show_cursor false ; {last_height = None}

  let move_cursor_back t =
    match t.last_height with
    | None ->
        Lwt.return ()
    | Some l ->
        log (sprintf "last height was %d" l) ;
        Notty_lwt.move_cursor (`By (0, -1 * l))

  let quit t =
    let* () = move_cursor_back t in
    Tty.show_cursor true ; Tty.echo true ; Tty.raw false ; Caml.exit 0

  let update t image =
    let* () = move_cursor_back t in
    t.last_height <-
      Some (Int.max (Option.value t.last_height ~default:0) (I.height image)) ;
    let image = I.vsnap ~align:`Top (Option.value_exn t.last_height) image in
    Notty_lwt.output_image (Notty_unix.eol image)

  let events _ =
    let unescape = Notty.Unescape.create () in
    Lwt_stream.from_direct (fun () ->
        match Stdio.In_channel.input_char stdin with
        | None ->
            None
        | Some c -> (
            Notty.Unescape.input unescape (Bytes.of_char_list [c]) 0 1 ;
            match Notty.Unescape.next unescape with
            | `End ->
                None
            | `Await ->
                (* if this happens, you'll probably want to make a recursive
                 * function that waits until there's a key to send and only
                 * then returns to Lwt_stream. *)
                raise (Failure "unimplemented - hasn't happened yet")
            | #Notty.Unescape.event as event ->
                Some event ))
end

module Make (App : Caribou_app) (Display : Display) = struct
  module State = State (App) (Display)

  let run () =
    let state = State.init ~display:(Display.init ()) in
    (* display the intial screen *)
    let* () = State.render state in
    let events = Display.events state.display in
    Lwt_stream.iter_s
      (fun event ->
        let* () =
          match Action.of_event event with
          | Some action ->
              State.update state action
          | None ->
              Lwt.return ()
        in
        State.render state)
      events
end

module Notty_helpers = struct
  let image_of_string a t =
    t
    |> Str.global_replace (Str.regexp_string "\t") "    "
    |> String.split ~on:'\n'
    |> List.map ~f:(I.string a)
    |> List.fold ~init:I.empty ~f:I.( <-> )
end
