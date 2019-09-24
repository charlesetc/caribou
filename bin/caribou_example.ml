open Core

module Example = struct
  type model = {pid : int; cwd : string; progress : int * int}
  [@@deriving sexp]

  type id = int

  let id m = m.pid

  let list =
    [ {pid = 0; cwd = "~/code"; progress = (2, 5324)}
    ; {pid = 1; cwd = "~/write"; progress = (5, 5324)}
    ; {pid = 2; cwd = "~/build"; progress = (23, 5324)} ]

  let get =
    let index =
      list |> List.map ~f:(fun m -> (m.pid, m)) |> Int.Map.of_alist_exn
    in
    fun id -> Map.find index id

  let list () = list

  let view m =
    Notty.I.string
      Notty.A.(fg white ++ bg lightgreen)
      (sexp_of_model m |> Sexplib.Sexp.to_string)
end

module App = Caribou.Make (Example)

let () = Lwt.async App.run
