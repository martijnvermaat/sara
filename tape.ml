(*
  Implementation for the tape datatype.

  A tape is an infinite (in two directions) list of cells, of which a
  finite contains an integer an infinite number is blank.

  This representation is a mapping from `int' to `int option'.
*)


type symbol    = int option
type position  = int
type direction = Left | Right

module PositionMap = Map.Make(struct
                                type t = position
                                let compare = compare
                              end)

type cells = int PositionMap.t
type tape  = cells * position


(*
  Empty tape.
*)
let empty = PositionMap.empty, 0


(*
  Update symbol at reading head position and move the reading head.
*)
let step tape symbol direction =
  let cells, position = tape in
  let new_cells =
    match symbol with
      | Some v -> PositionMap.add position v cells
      | None   -> PositionMap.remove position cells
  and new_position =
    match direction with
      | Left  -> position - 1
      | Right -> position + 1
  in
  new_cells, new_position


(*
  Tape constructed from string with reading head on first symbol.
*)
let parse string =
  let rec symbols s =
    try
      let head = match String.sub s 0 1 with
        | " " -> None
        | "B" -> None
        | c   -> Some (int_of_string c)
      and tail = String.sub string 1 ((String.length s) - 1)
      in
      head :: (symbols tail)
    with
      | Invalid_argument _ -> []
      | Failure _          ->
          let e = "Only digits and blanks are allowed on the tape" in
          raise (Failure e)
  in
  let f tape symbol = step tape symbol Right in
  let cells, _ = (List.fold_left f empty (symbols string)) in
  cells, 0


(*
  Pretty print symbol to string.
*)
let pretty_print_symbol symbol =
  match symbol with
    | None   -> "B"
    | Some i -> string_of_int i


(*
  Pretty print direction to string.
*)
let pretty_print_direction direction =
  match direction with
    | Left  -> "l"
    | Right -> "r"


(*
   Symbol at reading head position.
*)
let read tape =
  let cells, position = tape in
  try
    Some (PositionMap.find position cells)
  with
    | Not_found -> None


(*
  Get contents of tape as a triple:
  - list of symbols before the reading head
  - symbol at the reading head
  - list of symbols after the reading head

  TODO: clean up
*)
let contents tape =
  let cells, position = tape
  in
  let rec blanks n = (* TODO: use ExtList.List.Make *)
    if (n > 0) then
      None :: (blanks (n - 1))
      else
        []
  in
  let combine position symbol (list, last) =
      ((list @ (blanks (position - last - 1)) @ [Some symbol]), position)
  in
  let (c, p) = PositionMap.fold combine cells ([], (position - 1))
  in
  let l = c @ (blanks (position - p))
  in
  let rec take n l =
    match n, l with
        0, _      -> []
      | _, []     -> []
      | _, (h::t) -> h :: take (n - 1) t
  in
  let rec drop n l =
    match n, l with
        0, _ -> l
      | _, [] -> []
      | _, h::t -> drop (n - 1) t
  in
  let q = (List.length l) - (max p position) + position - 1
  in
    (take q l), (List.nth l q), (drop (q + 1) l)
