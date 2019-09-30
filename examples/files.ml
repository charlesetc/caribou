open Core

module Example = struct
  type item = string

  let list () = Sys.ls_dir "." |> List.filter ~f:Sys.is_file_exn

  let show m ~selected =
    let attr =
      if selected then Notty.A.(bg @@ rgb_888 ~r:53 ~g:242 ~b:160)
      else Notty.A.empty
    in
    Notty.I.string attr m

  let inspect m =
    let text = Stdio.In_channel.read_all m in
    let attr = Notty.A.(bg lightblue) in
    Caribou.Notty_helpers.image_of_string attr text
end

module App = Caribou.Make (Example) (Caribou.Fullscreen_display)

let () = Lwt_main.run (App.run ())
