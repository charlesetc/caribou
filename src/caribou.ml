open Import
open Base

module type App = sig
  type item

  val show : item -> selected:bool -> Notty.image

  val inspect : item -> Notty.image

  val list : unit -> item list
end

module type Display = sig
  type t

  val init : unit -> t

  val quit : t -> 'a Lwt.t

  val render : t -> Notty.image -> unit Lwt.t

  val events : t -> [Notty.Unescape.event | `Resize of int * int] Lwt_stream.t
end

module State (A : App) (D : Display) = struct
  module View = struct
    type t = List of A.item list | Show of A.item
  end

  type t =
    { mutable view : View.t
    ; mutable cursor : int
    ; mutable scroll : int
    ; display : D.t }

  let init ~display = {view = List []; cursor = 0; scroll = 0; display}

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

  let render t =
    let image = show t in
    let image = scroll t image in
    D.render t.display image
end

module Display = struct
  module type S = Display

  module Fullscreen : S = Fullscreen_display

  module Tty : S = Tty_display
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
      let selected c = List.equal Int.equal c rev_cursor in
      let rec lp items cursor_base =
        List.fold items ~init:(0, I.empty) ~f:(fun (i, acc) item ->
            let position = i :: cursor_base in
            let children = lp (A.children item) position |> snd in
            let image = A.show ~children item ~selected:(selected position) in
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
      (* try to combine this with decrement too *)
      let rec increment items = function
        | [] ->
            raise (Failure "cursor cannot be empty")
        | [n] ->
            let length = List.length items in
            if n + 1 = length then `Bubble_up else `Done [n + 1]
        | n :: rest -> (
          match increment (A.children (List.nth_exn items n)) rest with
          | `Done rest ->
              `Done (n :: rest)
          | `Bubble_up ->
              increment items [n] )

      (* If a bubble up hits the top level, keep the cursor where it is *)
      let increment items cursor =
        match increment items cursor with `Bubble_up -> cursor | `Done x -> x

      let rec decrement items = function
        | [] ->
            raise (Failure "cursor cannot be empty")
        | [n] ->
            if n - 1 < 0 then `Bubble_up else `Done [n - 1]
        | n :: rest -> (
          match decrement (A.children (List.nth_exn items n)) rest with
          | `Done rest ->
              `Done (n :: rest)
          | `Bubble_up ->
              decrement items [n] )

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
          Lwt.return (t.cursor <- Cursor.increment items t.cursor)
      | List items, Cursor_up ->
          Lwt.return @@ (t.cursor <- Cursor.decrement items t.cursor)
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

module Notty_helpers = Notty_helpers
