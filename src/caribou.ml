open Import
open Base

module type Display = sig
  type t

  val init : unit -> t

  val quit : t -> 'a Lwt.t

  val render : t -> Notty.image -> unit Lwt.t

  val events : t -> [Notty.Unescape.event | `Resize of int * int] Lwt_stream.t
end

module Display = struct
  module type S = Display

  module Fullscreen : S = Fullscreen_display

  module Tty : S = Tty_display
end

module Tree = struct
  module type App = sig
    type item

    val show : children:Notty.image -> selected:bool -> item -> Notty.image

    val children : item -> item list

    val inspect : item -> Notty.image

    val list : unit -> item list
  end

  module State (A : App) (D : Display) = struct
    module View = struct
      type t = List of A.item list | Show of A.item
    end

    type t =
      { mutable view : View.t
      ; mutable cursor : int list
      ; mutable scroll : int
      ; display : D.t }

    let init ~display = {view = List []; cursor = [0]; scroll = 0; display}

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
      | List _ ->
          (* this is weird *)
          let items = A.list () in
          t.view <- List items ;
          image_of_items t items
      | Show item ->
          A.inspect item

    let scroll t image = I.vcrop (-1 * t.scroll) (-1) image

    module Cursor = struct
      let rec string_of_cursor = function
        | [] ->
            "[]"
        | x :: rest ->
            Int.to_string x ^ " :: " ^ string_of_cursor rest

      let rec increment items =
        let children items n = A.children (List.nth_exn items n) in
        let attempt_index items i =
          if List.length items > i then `Done [i] else `Bubble_up
        in
        function
        | [n] -> (
          match children items n with
          | [] ->
              attempt_index items (n + 1)
          | _ ->
              `Done [n; 0] )
        | n :: rest -> (
          match increment (children items n) rest with
          | `Done rest ->
              `Done (n :: rest)
          | `Bubble_up ->
              attempt_index items (n + 1) )
        | [] ->
            raise (Failure "cursor cannot be empty")

      (* If a bubble up hits the top level, keep the cursor where it is *)
      let increment items cursor =
        match increment items cursor with `Bubble_up -> cursor | `Done x -> x

      let rec decrement items =
        let children items n = A.children (List.nth_exn items n) in
        function
        | [n] -> (
          match List.nth items (n - 1) |> Option.map ~f:A.children with
          | Some [] ->
              `Done [n - 1]
          | Some children ->
              `Done [n - 1; List.length children - 1]
          | None ->
              `Bubble_up )
        | n :: rest -> (
          match decrement (children items n) rest with
          | `Done rest ->
              `Done (n :: rest)
          | `Bubble_up ->
              `Done [n] )
        | [] ->
            raise (Failure "cursor cannot be empty")

      (* If a bubble up hits the top level, keep the cursor where it is *)
      let decrement items cursor =
        match decrement items cursor with `Bubble_up -> cursor | `Done x -> x

      let rec index items = function
        | [] ->
            raise (Failure "cursor cannot be empty")
        | [n] ->
            List.nth_exn items n
        | n :: rest ->
            let parent = List.nth_exn items n in
            index (A.children parent) rest
    end

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
      | List items, Cursor_down ->
          let cursor = Cursor.increment items t.cursor in
          Debug.log "cursor %s" (Cursor.string_of_cursor cursor) ;
          Lwt.return @@ (t.cursor <- cursor)
      | List items, Cursor_up ->
          let cursor = Cursor.decrement items t.cursor in
          Debug.log "cursor %s" (Cursor.string_of_cursor cursor) ;
          Lwt.return @@ (t.cursor <- cursor)
      | List items, Chose_cursor ->
          let item = Cursor.index items t.cursor in
          Lwt.return @@ (t.view <- Show item)
      | List _, Back ->
          D.quit t.display
      | Show _, Back ->
          Lwt.return @@ (t.view <- List [])
      | Show _, _ ->
          (* unable to handle event... ? *)
          Lwt.return ()

    let render t =
      let image = show t in
      let image = scroll t image in
      D.render t.display image
  end

  module Make (App : App) (Display : Display) = struct
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
end

module type App = sig
  type item

  val show : item -> selected:bool -> Notty.image

  val inspect : item -> Notty.image

  val list : unit -> item list
end

module Make (A : App) (D : Display.S) = struct
  module App : Tree.App = struct
    include A

    let show ~children:_ ~selected item = show ~selected item

    let children _ = []
  end

  module Made = Tree.Make (App) (D)

  let run = Made.run
end

module Notty_helpers = Notty_helpers
module Debug = Debug
