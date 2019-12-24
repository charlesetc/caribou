module type Untransformed = sig
  val uninitialize : unit -> unit Lwt.t

  val reinitialize : unit -> unit Lwt.t

  val render : Notty.image -> unit Lwt.t

  val events :
    unit -> [ Notty.Unescape.event | `Resize of int * int ] Lwt_stream.t
end

module type S = sig
  val uninitialize : unit -> unit Lwt.t

  val reinitialize : unit -> unit Lwt.t

  val render : Notty.image -> unit Lwt.t

  val grab_events :
    ([ Notty.Unescape.event | `Resize of int * int ] ->
    [ `Finished | `Continue ] Lwt.t) ->
    unit Lwt.t
end

module Wrap_events (D : Untransformed) : S = struct
  open Import
  open Base
  include D

  let stack = ref []

  let handle_event event =
    match !stack with
    | [ (f, _) ] ->
        (* if there's a single callback, never finish *)
        let+ _ = f event in
        ()
    | (f, finished) :: rest -> (
        let+ res = f event in
        match res with
        | `Continue -> ()
        | `Finished ->
            (* invariant: we never set the stack to an empty list *)
            stack := rest;
            Lwt.wakeup finished () )
    | [] -> raise (Failure "invariant: list should be non empty!")

  let start_event_loop `Only_once =
    Lwt.async @@ fun () ->
    Lwt_stream.iter
      (fun event -> Lwt.async (fun () -> handle_event event))
      (D.events ())

  let start_event_loop_once =
    let started = ref false in
    fun () -> if !started then () else start_event_loop `Only_once

  let grab_events f =
    let finished, write = Lwt.wait () in
    (* invariant: the stack is nonempty before we start the event loop *)
    stack := (f, write) :: !stack;
    start_event_loop_once ();
    finished
end
