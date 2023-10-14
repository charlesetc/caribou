open Core
open Notty.Infix
module A = Notty.A
module I = Notty.I

module Display = Caribou.Display.Fullscreen ()

module App = Caribou.App (Display)

let ( let+ ) = Lwt.( >|= )

module Example = struct
  type item = File of string | Dir of string * item list [@@deriving show]

  let rec list dir =
    Sys_unix.ls_dir dir
    |> List.map ~f:(fun entry ->
           let entry = Filename.concat dir entry in
           match Sys_unix.is_directory entry with
           | `Yes -> Dir (entry, list entry)
           | `Unknown | `No -> File entry)

  let list () = list "."

  let string_of_item = function File s -> s | Dir (s, _) -> s

  let children = function File _ -> [] | Dir (_, c's) -> c's

  let image_of_item ~children ~selected = function
    | File name ->
        let a = if selected then A.(st underline ++ fg magenta) else A.empty in
        I.string ~attr:A.empty "* " <|> I.string ~attr:a (Filename.basename name)
    | Dir (name, _) ->
        let a = if selected then A.(bg blue ++ fg white) else A.empty in
        let column = I.char ~attr:a ' ' 1 (I.height children) in
        let image = column <|> children <|> column in
        let hr = I.char ~attr:a ' ' (I.width image) 1 in
        let title = I.string ~attr:a (Filename.basename name ^ "/") in
        let topline =
          if selected then
            let padding = I.void ((I.width image - I.width title) / 2) 1 in
            padding <|> title </> hr
          else title </> hr
        in
        topline <-> image <-> hr

  let inspect item =
    let filename = match item with File a -> a | Dir (a, _) -> a in
    let+ status = Caribou.Ext.Unix.exec (module Display) "vim" [ filename ] in
    match status with
    | WEXITED 0 -> ()
    | s ->
        Caribou.Debug.log "vim exited with error %s"
          (Caribou.Ext.Unix.Process_status.to_string s)

  let bindings = [ (`Enter, [], `Custom inspect) ]
end

let () =
  let module A = App.Tree.Make (Example) in
  Lwt_main.run (A.run ())
