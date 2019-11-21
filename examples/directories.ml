open Core
open Notty.Infix
module A = Notty.A
module I = Notty.I

module Example = struct
  type item = File of string | Dir of string * item list

  let rec list dir =
    Sys.ls_dir dir
    |> List.map ~f:(fun entry ->
           if Sys.is_file_exn entry then File entry
           else Dir (entry, list (Filename.concat dir entry)))

  let list = list "."

  let children = function File _ -> [] | Dir (_, c's) -> c's

  let show ~children ~selected = function
    | File name ->
        let a = if selected then A.(st underline ++ fg magenta) else A.empty in
        I.string A.empty "* " <|> I.string a name
    | Dir (name, _) ->
        let a = if selected then A.(bg blue) else A.empty in
        let column = I.char a ' ' 1 (I.height children) in
        let image = column <|> children <|> column in
        let hr = I.char a ' ' (I.width image) 1 in
        (* TODO: Make it centered in the future *)
        let topline = I.string A.(fg black) name </> hr in
        topline <-> image <-> hr

  let inspect m =
    let text = Stdio.In_channel.read_all m in
    Caribou.Notty_helpers.image_of_string A.empty text
end

module App = Caribou.Tree.Make (Example) (Caribou.Fullscreen_display)

let () = Lwt_main.run (App.run ())
