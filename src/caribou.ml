open! Base

module Display = struct
  module type S = Display.S

  module Fullscreen () : S = Display.Wrap_events (Fullscreen_display.M ())

  module Tty () : S = Display.Wrap_events (Tty_display.M ())
end

module Debug = Debug
module Key = Key
module Action = Action

module App (D : Display.S) = struct
  module Tree = struct
    include Tree_app
    module Make = Make (D)
  end

  module List = struct
    include List_app
    module Make = Make (D)
  end

  module Const = struct
    include Const_app
    module Make = Make (D)
  end

  let const image =
    let module A = Const.Make (struct
      let image = image
    end) in
    A.run ()
end

module Ext = struct
  module Notty = Notty_ext
  module Unix = Unix_ext
end
