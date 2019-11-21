open Core
open Notty.Infix
module A = Notty.A
module I = Notty.I

module Example = struct
  type item = File of string | Dir of string * item list

  let rec list dir =
    Sys.ls_dir dir
    |> List.map ~f:(fun entry ->
           match Sys.is_directory entry with
           | `Yes ->
               Dir (entry, list (Filename.concat dir entry))
           | `No | `Unknown ->
               File entry)

  let list () = list "."

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

  let inspect item =
    let name = match item with File name -> name | Dir (name, _) -> name in
    let text = Stdio.In_channel.read_all name in
    Caribou.Notty_helpers.image_of_string A.empty text
end

module App = Caribou.Tree.Make (Example) (Caribou.Display.Fullscreen)

let () = Lwt_main.run (App.run ())
