open Stdio.Out_channel

let output = create "/tmp/caribou-1687.log"

let log string =
  output_string output (string ^ "\n") ;
  flush output

include Printf
