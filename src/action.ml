type t =
  [ `Cursor_down
  | `Cursor_up
  | `Back
  | `Quit
  | `Scroll_up
  | `Scroll_down
  | `Page_up
  | `Page_down (* | `Custom_lwt of (unit -> unit Lwt.t) *) ]
[@@deriving sexp]

let default_bindings =
  [
    (`ASCII 'j', [], `Cursor_down);
    (`ASCII 'k', [], `Cursor_up);
    (`Arrow `Up, [], `Cursor_up);
    (`Arrow `Down, [], `Cursor_down);
    (`Arrow `Up, [ `Ctrl ], `Scroll_up);
    (`Arrow `Down, [ `Ctrl ], `Scroll_down);
    (`Page `Up, [], `Page_up);
    (`Page `Down, [], `Page_down);
    (`ASCII 'U', [ `Ctrl ], `Page_up);
    (`ASCII 'D', [ `Ctrl ], `Page_up);
    (`Backspace, [], `Back);
    (`Escape, [], `Back);
    (`ASCII 'q', [], `Quit);
    (`ASCII 'C', [ `Ctrl ], `Quit);
  ]
