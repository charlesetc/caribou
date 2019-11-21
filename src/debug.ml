let logfile = lazy (Caml.open_out "/tmp/caribou_log")

let log fmt =
  Printf.ksprintf
    (fun s ->
      let logfile = Lazy.force logfile in
      Caml.output_string logfile s ;
      Caml.output_char logfile '\n' ;
      Caml.flush logfile)
    fmt
