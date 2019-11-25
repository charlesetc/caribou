open Import
open Base

module Display = struct
  module type S = Display.S

  module Fullscreen : S = Fullscreen_display

  module Tty : S = Tty_display
end

module Tree = struct
  module type App = App.S

  module State (A : App.S) (D : Display.S) = struct
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

  module Make (App : App.S) (Display : Display.S) = struct
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
