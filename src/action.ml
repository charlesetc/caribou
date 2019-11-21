type t =
  | Cursor_down
  | Cursor_up
  | Chose_cursor
  | Back
  | Quit
  | Scroll_up
  | Scroll_down
  | Page_up
  | Page_down

let of_event = function
  | `Key (`ASCII 'k', []) | `Key (`Arrow `Up, []) ->
      Some Cursor_up
  | `Key (`ASCII 'j', []) | `Key (`Arrow `Down, []) ->
      Some Cursor_down
  | `Key (`Enter, []) | `Key (`ASCII 'M', [`Ctrl]) ->
      Some Chose_cursor
  | `Key (`Backspace, []) | `Key (`Escape, []) ->
      Some Back
  | `Key (`ASCII 'U', [`Ctrl]) | `Key (`Page `Up, []) ->
      Some Page_up
  | `Key (`ASCII 'D', [`Ctrl]) | `Key (`Page `Down, []) ->
      Some Page_down
  | `Key (`Arrow `Down, [`Ctrl]) | `Key (`ASCII 'N', [`Ctrl]) ->
      Some Scroll_down
  | `Key (`Arrow `Up, [`Ctrl]) | `Key (`ASCII 'P', [`Ctrl]) ->
      Some Scroll_up
  | `Key (`ASCII 'q', []) | `Key (`ASCII 'C', [`Ctrl]) ->
      Some Quit
  | _ ->
      None
