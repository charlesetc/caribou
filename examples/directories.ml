open Core
open Notty.Infix
module A = Notty.A
module I = Notty.I

module Example = struct
  type item = File of string | Dir of string * item list

  let rec list dir =
    Sys.ls_dir dir
    |> List.map ~f:(fun entry ->
           let entry = Filename.concat dir entry in
           match Sys.is_directory entry with
           | `Yes ->
               Dir (entry, list entry)
           | `Unknown | `No ->
               File entry)

  let list () = list "."

  let children = function File _ -> [] | Dir (_, c's) -> c's

  let show ~children ~selected = function
    | File name ->
        let a = if selected then A.(st underline ++ fg magenta) else A.empty in
        I.string A.empty "* " <|> I.string a (Filename.basename name)
    | Dir (name, _) ->
        let a = if selected then A.(bg blue ++ fg white) else A.empty in
        let column = I.char a ' ' 1 (I.height children) in
        let image = column <|> children <|> column in
        let hr = I.char a ' ' (I.width image) 1 in
        let title = I.string a (Filename.basename name ^ "/") in
        let topline =
          if selected then
            let padding = I.void ((I.width image - I.width title) / 2) 1 in
            padding <|> title </> hr
          else title </> hr
        in
        topline <-> image <-> hr

  (* let inspect = function *)
  (* | File name -> *)
  (* let text = Stdio.In_channel.read_all name in *)
  (* Caribou.Notty_helpers.image_of_string A.empty text *)
  (* | Dir (name, _) -> *)
  (* Caribou.Notty_helpers.image_of_string A.empty *)
  (* (sprintf "%s is a directory!" name) *)

  let bindings =
    [(`Choose_cursor, fun _item -> raise (Failure "unimplemented"))]
end

module App = Caribou.Tree.Make (Example) (Caribou.Display.Fullscreen)

let () = Lwt_main.run (App.run ())
