open Core

module Example = struct
  type item = {pid : int; cwd : string; progress : int * int} [@@deriving sexp]

  let list =
    [ {pid = 0; cwd = "~/code"; progress = (2, 5324)}
    ; {pid = 1; cwd = "~/write"; progress = (5, 5324)}
    ; {pid = 2; cwd = "~/build"; progress = (23, 5324)} ]

  let list () = list

  let show m ~selected =
    let attr =
      if selected then Notty.A.(bg @@ rgb_888 ~r:53 ~g:242 ~b:160)
      else Notty.A.empty
    in
    Notty.I.string attr (sexp_of_item m |> Sexplib.Sexp.to_string)

  let inspect m =
    let text =
      Unix.open_process_in (sprintf "ls %s | head" m.cwd)
      |> In_channel.input_all
    in
    let attr = Notty.A.(bg blue) in
    Caribou.Notty_helpers.image_of_string attr text
end

module App = Caribou.Make (Example) (Caribou.Tty_display)

let () = Lwt_main.run (App.run ())
