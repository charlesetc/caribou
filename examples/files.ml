open Core
open Notty.Infix
module A = Notty.A
module I = Notty.I

module Example = struct
  type item = string

  let list () = Sys.ls_dir "." |> List.filter ~f:Sys.is_file_exn

  let show m ~selected =
    let a = if selected then A.(st underline ++ fg magenta) else A.empty in
    I.string A.empty "* " <|> I.string a m

  let inspect m =
    let text = Stdio.In_channel.read_all m in
    Caribou.Notty_helpers.image_of_string A.empty text
end

module App = Caribou.Make (Example) (Caribou.Display.Fullscreen)

let () = Lwt_main.run (App.run ())
