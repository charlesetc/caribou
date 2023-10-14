open Core

module App = Caribou.App (Caribou.Display.Tty ())

module Debug = Caribou.Debug

module Example = struct
  type item = { pid : int; cwd : string; progress : int * int }
  [@@deriving show, sexp]

  let list =
    [
      { pid = 1; cwd = "~/desk"; progress = (5, 5324) };
      { pid = 0; cwd = "~/code"; progress = (2, 5324) };
      { pid = 1; cwd = "~/write"; progress = (5, 5324) };
      { pid = 2; cwd = "~/build"; progress = (23, 5324) };
    ]

  let list () = list

  let image_of_item m ~selected =
    let attr =
      if selected then Notty.A.(bg (rgb_888 ~r:53 ~g:142 ~b:160) ++ fg black)
      else Notty.A.empty
    in
    Notty.I.string ~attr (sexp_of_item m |> Sexplib.Sexp.to_string)

  let inspect item =
    let text =
      Core_unix.open_process_in (sprintf "ls %s 2>/dev/null | head" item.cwd)
      |> In_channel.input_all
    in
    let attr = Notty.A.(bg blue) in
    let image = Caribou.Ext.Notty.image_of_string attr text in
    App.const image

  let bindings = [ (`Enter, [], `Custom inspect) ]
end

let () =
  let module A = App.List.Make (Example) in
  Lwt_main.run (A.run ())
