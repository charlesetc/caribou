open Base
open Notty.Infix
module A = Notty.A
module I = Notty.I

module Display = Caribou.Display.Tty ()

module App = Caribou.App (Display)

let ( let+ ) = Lwt.( >|= )

module Sys = struct
  let is_file f = Caml.Sys.file_exists f && not (Caml.Sys.is_directory f)
end

module Example = struct
  type item = string [@@deriving show]
  (** [item] is filename in this example *)

  let list () =
    Caml.Sys.readdir "." |> Array.to_list |> List.filter ~f:Sys.is_file

  let image_of_item m ~selected =
    let a = if selected then A.(st underline ++ fg magenta) else A.empty in
    I.string ~attr:A.empty "* " <|> I.string ~attr:a m

  let inspect item =
    let+ res = Caribou.Ext.Unix.exec (module Display) "nvim" [ item ] in
    match res with
    | WEXITED 0 ->
        Caribou.Debug.log "exited";
        ()
    | s ->
        Caribou.Debug.log "vim exited with error %s"
          (Caribou.Ext.Unix.Process_status.to_string s)

  let bindings = [ (`Enter, [], `Custom inspect) ]
end

let () =
  let module A = App.List.Make (Example) in
  Lwt_main.run (A.run ())
