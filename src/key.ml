open Base

type mod_ = [ `Meta | `Ctrl | `Shift ] [@@deriving sexp, eq]

type mods = mod_ list [@@deriving sexp, eq]

type t =
  [ `Escape
  | `Enter
  | `Tab
  | `Backspace
  | `Insert
  | `Delete
  | `Home
  | `End
  | `Arrow of [ `Up | `Down | `Left | `Right ]
  | `Page of [ `Up | `Down ]
  | `Function of int
  | `Uchar of Uchar.t
  | `ASCII of char ]
[@@deriving sexp, eq]
