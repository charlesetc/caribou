(* A wrapper around the tty unix utilities, namely
 * [stty] and [tput]. *)

val raw : bool -> unit

val echo : bool -> unit

val show_cursor : bool -> unit
