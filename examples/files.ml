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
    let+ _ = Lwt_process.exec ("vim", [| "vim"; item |]) in
    ()

  let bindings = [ (`Choose_cursor, inspect) ]
end

module App = Caribou.List.Make (Example) (Caribou.Display.Fullscreen)

let () = Lwt_main.run (App.run ())
