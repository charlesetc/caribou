open Core
open Notty.Infix
module A = Notty.A
module I = Notty.I

let ( let+ ) = Lwt.( >|= )

module Example = struct
  type item = string (* filename *)

  let list () = Sys.ls_dir "." |> List.filter ~f:Sys.is_file_exn

  let show m ~selected =
    let a = if selected then A.(st underline ++ fg magenta) else A.empty in
    I.string A.empty "* " <|> I.string a m

  let inspect item =
    Lwt.return @@
    match Caribou.Ext.Unix.exec "vim" [ item ] with
    | WEXITED 0 -> ()
    | s ->
        Caribou.Debug.log "vim exited with error %s"
          (Caribou.Ext.Unix.Process_status.to_string s)

  let bindings = [ (`Choose_cursor, inspect) ]
end

module App = Caribou.List.Make (Example) (Caribou.Display.Fullscreen)

let () = Lwt_main.run (App.run ())
