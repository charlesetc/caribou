open Base

module Process_status = struct
  type t = Unix.process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string (sexp_of_t t)
end

let exec prog args =
  let argv = Array.of_list (prog :: args) in
  match Unix.fork () with
  | 0 ->
      (try Unix.execvp prog argv
       with _ -> Caml.exit 127)
  | pid -> Unix.waitpid [] pid |> snd
