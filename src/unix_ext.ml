open Base
open Import

module Process_status = struct
  type t = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string (sexp_of_t t)
end

let exec (module D : Display.S) prog args =
  let* () = D.uninitialize () in
  let argv = Array.of_list (prog :: args) in
  match Unix.fork () with
  | 0 -> ( try Unix.execvp prog argv with _ -> Caml.exit 127 )
  | pid ->
      let status = try Unix.waitpid [] pid |> snd with Unix.Unix_error(_) ->
        Process_status.WEXITED 0 in
      let+ () = D.reinitialize () in
      status
