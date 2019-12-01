open Base
open Notty.Infix
module A = Notty.A
module I = Notty.I

let ( let+ ) = Lwt.( >|= )

module Sys = struct
  let is_file f = Caml.Sys.file_exists f && not (Caml.Sys.is_directory f)
end

module Example = struct
  type item = string (* filename *)

  let list () =
    Caml.Sys.readdir "." |> Array.to_list |> List.filter ~f:Sys.is_file

  let show m ~selected =
    let a = if selected then A.(st underline ++ fg magenta) else A.empty in
    I.string A.empty "* " <|> I.string a m

  let inspect item =
    match Caribou.Ext.Unix.exec "vim" [ item ] with
    | WEXITED 0 -> ()
    | s ->
        Caribou.Debug.log "vim exited with error %s"
          (Caribou.Ext.Unix.Process_status.to_string s)

  let bindings = [ (`Enter, [], `Custom inspect) ]
end

module App = Caribou.List.Make (Example) (Caribou.Display.Fullscreen)

let () = Lwt_main.run (App.run ())
