let logfile = Caml.open_out "/tmp/caribou_log"

let log fmt =
  Printf.ksprintf
    (fun s ->
      Caml.output_string logfile s ;
      Caml.output_char logfile '\n' ;
      Caml.flush logfile)
    fmt
