open! Import
open! Base

let ( << ) f g x = Fn.compose g f x

let image_of_string a t =
  let lines =
    t
    |> Str.global_replace (Str.regexp_string "\t") "    "
    |> String.split ~on:'\n'
  in
  let combine =
    List.map ~f:(I.string ~attr:a) << List.fold ~init:I.empty ~f:I.( <-> )
  in
  try combine lines with _ -> List.map lines ~f:String.escaped |> combine
