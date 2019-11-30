open Import
open Base

type t = {mutable last_height : int option}

let init () =
  Tty.echo false ; Tty.raw true ; Tty.show_cursor false ; {last_height = None}

let move_cursor_back t =
  match t.last_height with
  | None ->
      Lwt.return ()
  | Some l ->
      Notty_lwt.move_cursor (`By (0, -1 * l))

let uninitialize t =
  let+ () = move_cursor_back t in
  for _ = 0 to Option.value t.last_height ~default:0 do
    Caml.print_newline ()
  done ;
  Tty.show_cursor true ;
  Tty.echo true ;
  Tty.raw false

let render t image =
  let* () = move_cursor_back t in
  t.last_height <-
    Some (Int.max (Option.value t.last_height ~default:0) (I.height image)) ;
  let image = I.vsnap ~align:`Top (Option.value_exn t.last_height) image in
  Notty_lwt.output_image (Notty_unix.eol image)

let events _ =
  let unescape = Notty.Unescape.create () in
  let stdin_stream = Lwt_io.read_chars Lwt_io.stdin in
  Lwt_stream.from (fun () ->
      let+ _ = Lwt_stream.peek stdin_stream in
      let bytes =
        Lwt_stream.get_available stdin_stream |> Bytes.of_char_list
      in
      Notty.Unescape.input unescape bytes 0 (Bytes.length bytes) ;
      match Notty.Unescape.next unescape with
      | `End ->
          None
      | `Await ->
          (* if this happens, you'll probably want to make a recursive
           * function that waits until there's a key to send and only
           * then returns to Lwt_stream. *)
          raise (Failure "not implemented - hasn't happened yet")
      | #Notty.Unescape.event as event ->
          Some event)
