# Caribou Plan

Caribou should be a UI for:

  1. Listing things.
  2. Finding things
  3. Inspecting things.
  4. Calling functions on things.

The last point is the most challenging and I'll leave that for some time
in the future. For the first prototype, I just want to allow people to
make a list of things, choose one (via fuzzy finding) and then inspect it
(basically call a print or sexp\_of function on it.)

# Task

* Move the cursor to the right spot upon quitting.
* Represent a tree structure, not just a list structure.
* Make a `wrap` helper command

    val wrap : int -> Notty.image -> Notty.image
