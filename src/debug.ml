open Stdio.Out_channel

let output = open_out "/tmp/caribou-1687.log"

let log string =
  output_string output (string ^ "\n") ;
  flush output
