open Sexplib0

let logfile = Caml.open_out "/tmp/caribou_log"

let log fmt =
  Printf.ksprintf
    (fun s ->
      Caml.output_string logfile s;
      Caml.output_char logfile '\n';
      Caml.flush logfile)
    fmt

let log_s f t = log "%s" ("logged sexp: " ^ Sexp.to_string_hum (f t))
