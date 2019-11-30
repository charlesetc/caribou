open Core
open Notty.Infix
module A = Notty.A
module I = Notty.I

let ( let+ ) = Lwt.( >|= )

module Example = struct
  type item = string

  let list () = Sys.ls_dir "." |> List.filter ~f:Sys.is_file_exn

  let show m ~selected =
    let a = if selected then A.(st underline ++ fg magenta) else A.empty in
    I.string A.empty "* " <|> I.string a m

  let inspect item =
    let+ status = Lwt_process.exec ("vim", [| "vim"; item |]) in
    match status with
    | WEXITED i -> Caribou.Debug.log "less exited %d" i
    | WSIGNALED i -> Caribou.Debug.log "less signaled %d" i
    | WSTOPPED i -> Caribou.Debug.log "less stopped %d" i

  let bindings = [ (`Choose_cursor, inspect) ]
end

module App = Caribou.List.Make (Example) (Caribou.Display.Fullscreen)

let () = Lwt_main.run (App.run ())
