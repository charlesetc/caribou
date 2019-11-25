open Base

module Make (A : App.S) (D : Display.S) = struct
  type t = int list

  let rec string_of_cursor = function
    | [] ->
        "[]"
    | x :: rest ->
        Int.to_string x ^ " :: " ^ string_of_cursor rest

  let rec increment items =
    let children items n = A.children (List.nth_exn items n) in
    let attempt_index items i =
      if List.length items > i then `Done [i] else `Bubble_up
    in
    function
    | [n] -> (
      match children items n with
      | [] ->
          attempt_index items (n + 1)
      | _ ->
          `Done [n; 0] )
    | n :: rest -> (
      match increment (children items n) rest with
      | `Done rest ->
          `Done (n :: rest)
      | `Bubble_up ->
          attempt_index items (n + 1) )
    | [] ->
        raise (Failure "cursor cannot be empty")

  (* If a bubble up hits the top level, keep the cursor where it is *)
  let increment items cursor =
    match increment items cursor with `Bubble_up -> cursor | `Done x -> x

  let rec decrement items =
    let children items n = A.children (List.nth_exn items n) in
    function
    | [n] -> (
      match List.nth items (n - 1) |> Option.map ~f:A.children with
      | Some [] ->
          `Done [n - 1]
      | Some children ->
          `Done [n - 1; List.length children - 1]
      | None ->
          `Bubble_up )
    | n :: rest -> (
      match decrement (children items n) rest with
      | `Done rest ->
          `Done (n :: rest)
      | `Bubble_up ->
          `Done [n] )
    | [] ->
        raise (Failure "cursor cannot be empty")

  (* If a bubble up hits the top level, keep the cursor where it is *)
  let decrement items cursor =
    match decrement items cursor with `Bubble_up -> cursor | `Done x -> x

  let rec index items = function
    | [] ->
        raise (Failure "cursor cannot be empty")
    | [n] ->
        List.nth_exn items n
    | n :: rest ->
        let parent = List.nth_exn items n in
        index (A.children parent) rest
end
