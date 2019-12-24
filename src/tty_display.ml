open Import
open Base

type t = { mutable last_height : int option }

module M () = struct
  let initialize () =
    Tty.echo false;
    Tty.raw true;
    Tty.show_cursor false

  let global =
    initialize ();
    { last_height = None }

  let move_cursor_back t =
    match t.last_height with
    | None -> Lwt.return ()
    | Some l -> Notty_lwt.move_cursor (`By (0, -1 * l))

  let uninitialize () =
    let+ () = move_cursor_back global in
    Tty.show_cursor true;
    Tty.echo true;
    Tty.raw false

  let reinitialize () =
    initialize ();
    (* Yeah off by one whatever *)
    for _ = 1 to Option.value global.last_height ~default:1 do
      Caml.print_newline ()
    done;
    Lwt.return ()

  let render image =
    let* () = move_cursor_back global in

    let term_height = Notty_lwt.winsize Lwt_unix.stdin |> Option.map ~f:snd in
    let viewport_height =
      I.height image
      |> Int.max (Option.value global.last_height ~default:0)
      |> Int.min (Option.value term_height ~default:Int.max_value - 4)
    in
    global.last_height <- Some viewport_height;
    let image = I.vsnap ~align:`Top viewport_height image in
    Notty_lwt.output_image (Notty_unix.eol image)

  let map_event = function
    (* might have to do something similar with backspace *)
    | `Key (`ASCII 'M', [ `Ctrl ]) -> `Key (`Enter, [])
    | e -> e

  let events () =
    let unescape = Notty.Unescape.create () in
    let stdin_stream = Lwt_io.read_chars Lwt_io.stdin in
    Lwt_stream.from (fun () ->
        let+ _ = Lwt_stream.peek stdin_stream in
        let bytes =
          Lwt_stream.get_available stdin_stream |> Bytes.of_char_list
        in
        Notty.Unescape.input unescape bytes 0 (Bytes.length bytes);
        match Notty.Unescape.next unescape with
        | `End -> None
        | `Await ->
            (* if this happens, you'll probably want to make a recursive
            * function that waits until there's a key to send and only
            * then returns to Lwt_stream. *)
            raise (Failure "not implemented - hasn't happened yet")
        | #Notty.Unescape.event as event -> Some (map_event event))
end
