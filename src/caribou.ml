open Import
open Base

module Display = struct
  module type S = Display.S

  module Fullscreen : S = Fullscreen_display

  module Tty : S = Tty_display
end

module Tree = struct
  module type App = App.S

  module Make (App : App.S) (Display : Display.S) = struct
    module State = State.Make (App) (Display)

    let run () =
      let state = State.init ~display:(Display.init ()) in
      (* display the initial screen *)
      let* () = State.render state in
      let events = Display.events state.display in
      Lwt_stream.iter_s
        (fun event ->
          let* () =
            match Action.of_event event with
            | Some action ->
                Debug.log "actn %s" (Sexp.to_string_hum (Action.sexp_of_t action));
                State.update state action
            | None ->
                Debug.log "no actn";
                Lwt.return ()
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

    val bindings : (Action.t * (item -> unit Lwt.t)) list
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
