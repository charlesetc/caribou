# Caribo Plan

Caribou should be a UI for:

  1. Listing things.
  2. Finding things
  3. Inspecting things.
  4. Calling functions on things.

The last point is the most challenging and I'll leave that for some time in
the future. For the first prototype, I just want to allow people to make a
list of things, choose one (via fuzzy finding) and then inspect it (basically
call a print or sexp\_of function on it.)

# Tasks

* Searching
* Fuzzy finding
  - recursively? <- not sure what "recursively" means lol
* Scroll-to-cursor on move.
* Support text-wrapping via a helper function.
* Block layout

# Bugs

* Once you've scrolled in the List view, the scroll stays the same in the Show
  view, even though it can be much shorter.
* "List.nth\_exn 0 called on list of length 0" when list () returns []
* Doesn't work when executed from vim with :make

# Feature requests

* Open up a sub-process on enter.
* Make a Caribou.constant program

# Things to use Caribou with

* Files
* Processes
* OPAM packages
* Wikipedia browsing
  - (can add laziness)
* Code review features
* OCaml functions / documentation
* Hg bookmarks, commits, branches, etc
* Tmux sessions, windows, & panes
* Finances and budgeting
* Users and usergroups
* Music library viewer
* Application chooser
* Bash completions
* Note-taking
* Recipes
* Emails
