exception Stty_failure of string * int

exception Tput_failure of string * int

let stty s yes =
  let open Printf in
  let cmd = if yes then sprintf "stty %s" s else sprintf "stty -%s" s in
  let err = Sys.command cmd in
  if err <> 0 then raise (Stty_failure (s, err))

let raw = stty "raw"

let echo = stty "echo"

let show_cursor = function
  | true ->
      let err = Sys.command "tput cnorm" in
      if err <> 0 then raise (Tput_failure ("cnorm", err))
  | false ->
      let err = Sys.command "tput civis" in
      if err <> 0 then raise (Tput_failure ("civis", err))
