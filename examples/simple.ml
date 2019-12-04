open Core
module Display = Caribou.Display.Tty

module Example = struct
  type item = { pid : int; cwd : string; progress : int * int }
  [@@deriving sexp]

  let list =
    [
      { pid = 1; cwd = "~/desk"; progress = (5, 5324) };
      { pid = 0; cwd = "~/code"; progress = (2, 5324) };
      { pid = 1; cwd = "~/write"; progress = (5, 5324) };
      { pid = 2; cwd = "~/build"; progress = (23, 5324) };
    ]

  let list () = list

  let show m ~selected =
    let attr =
      if selected then Notty.A.(bg @@ rgb_888 ~r:53 ~g:242 ~b:160)
      else Notty.A.empty
    in
    Notty.I.string attr (sexp_of_item m |> Sexplib.Sexp.to_string)

  let ls item =
    let text =
      Unix.open_process_in (sprintf "ls %s 2>/dev/null | head" item.cwd)
      |> In_channel.input_all
    in
    let attr = Notty.A.(bg blue) in
    Caribou.Ext.Notty.image_of_string attr text

  (* TODO: pass in a context or something so you don't have to fork,
   * and to keep track of the display.t *)
  let inspect item =
    let module A : Caribou.Const.App = struct
      let image = ls item
    end in
    let module C = Caribou.Const.Make (A) (Display) in
    match Caribou.Ext.Unix.in_subprocess (fun () -> C.run ()) with
    | WEXITED 0 -> ()
    | s ->
        Caribou.Debug.log "caribou exited with error %s"
          (Caribou.Ext.Unix.Process_status.to_string s)

  let bindings = [ (`ASCII 'M', [ `Ctrl ], `Custom inspect) ]
end

module App = Caribou.List.Make (Example) (Display)

let () = Lwt_main.run (App.run ())
