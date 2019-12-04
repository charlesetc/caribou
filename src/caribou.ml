open Import
open Base

module Display = struct
  module type S = Display.S

  module Fullscreen : S = Fullscreen_display

  module Tty : S = Tty_display
end

module Const = struct
  module type App = App.Const

  module Make (App : App.Const) (Display : Display.S) = struct
    module State = Const_state.Make (App) (Display)

    let action_of_event event =
      match event with
      | `Key (key, mods) ->
          List.find Action.default_bindings ~f:(fun (key', mods', _) ->
              Key.equal key key' && Key.equal_mods mods mods')
          |> Option.map ~f:(fun (_, _, action) -> action)
      | _ -> None

    let run () =
      let state = State.init ~display:(Display.init ()) App.image in
      (* display the initial screen *)
      let* () = State.render state in
      let events = Display.events state.display in
      Lwt_stream.iter_s
        (fun event ->
          let* () =
            match action_of_event event with
            | Some action -> State.update state action
            | None -> Lwt.return ()
          in
          State.render state)
        events
  end
end

module Tree = struct
  module type App = App.Tree

  module Make (App : App.Tree) (Display : Display.S) = struct
    module State = Tree_state.Make (App) (Display)

    let action_of_event event =
      match event with
      | `Key (key, mods) ->
          List.find (App.bindings @ Action.default_bindings)
            ~f:(fun (key', mods', _) ->
              Key.equal key key' && Key.equal_mods mods mods')
          |> Option.map ~f:(fun (_, _, action) -> action)
      | _ -> None

    let run () =
      let state = State.init ~display:(Display.init ()) in
      (* display the initial screen *)
      let* () = State.render state in
      let events = Display.events state.display in
      Lwt_stream.iter_s
        (fun event ->
          let* () =
            match action_of_event event with
            | Some action -> State.update state action
            | None -> Lwt.return ()
          in
          State.render state)
        events
  end
end

module List = struct
  module type App = sig
    type item

    val show : item -> selected:bool -> Notty.image

    val list : unit -> item list

    val bindings :
      (Key.t * Key.mods * [ Action.t | `Custom of item -> unit ]) list
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
end

module Ext = struct
  module Notty = Notty_ext
  module Unix = Unix_ext
end

module Debug = Debug
module Key = Key
module Action = Action
