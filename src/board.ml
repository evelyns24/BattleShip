open Yojson.Basic.Util
open Ship

type s =
  | Empty
  | Full
  | Hit
  | Miss

type t = {
  x : int;
  y : int;
  state : s;
}

type b = {
  height : int;
  width : int;
  squares : t list;
}

exception Collide

let from_json (j : Yojson.Basic.t) : b =
  raise (Failure "Unimplemented Evelyn's Problem")

let get_height (board : b) : int = board.height
let get_width (board : b) : int = board.width
let check_collision (board : b) : bool = raise (Failure "Alisha's problem")

(**[from_state state] takes state s and turns it into a string according to:
   Empty -> "-"; Full -> "S"; Hit -> "X"; Miss -> "M" *)
let from_state (state : s) : string =
  match state with
  | Empty -> "-"
  | Full -> "S"
  | Hit -> "X"
  | Miss -> "M"

(**[to_string_list lst] takes lst, a t list, and turns it into a string list,
   using from_state*)
let rec to_string_list = function
  | [] -> []
  | h :: t -> from_state h.state :: to_string_list t

(**[first_w lst w] takes a lst and returns the first w elements. Example:
   first_w \[1;9;7;3\] 2 returns \[1;9\]*)
let rec first_w lst w =
  match lst with
  | [] -> []
  | h :: t ->
      if w = 0 then []
      else
        let tail = if w = 0 then [] else first_w t (w - 1) in
        h :: tail

(**[after_w lst w] takes a lst and returns all of the elements after the first
   w. Example: after_w \[1;9;7;3\] 2 returns \[7;3\]*)
let rec after_w lst w =
  match lst with
  | [] -> []
  | h :: t ->
      let tail = if w <= 0 then h :: after_w t (w - 1) else after_w t (w - 1) in
      tail

(**[dimensionalize lst w] takes a one dimensional list, [lst] and creates a
   2-dimensional list with lists of length [w]. Example: dimensionalize
   \[1;2;3;4;5;6\] 2 returns \[\[1;2\]; \[3;4\]; \[5;6\]\]*)
let rec dimensionalize (lst : 'a list) (w : int) : 'a list list =
  if List.length lst = w then [ first_w lst w ]
  else [ first_w lst w ] @ dimensionalize (after_w lst w) w

let rec get_board (board : b) (w : int) : string list list =
  let string_list = to_string_list board.squares in
  dimensionalize string_list w

let rec response (board : b) (x : int) (y : int) : bool =
  match board.squares with
  | [] -> false
  | h :: t ->
      if h.x = x && h.y = y then h.state = Full || h.state = Hit
      else
        response { height = board.height; width = board.width; squares = t } x y

let update (board : b) (x : int) (y : int) : b =
  raise (Failure "unimplemented update")

let rec score (board : b) (acc : int) : int =
  match board.squares with
  | [] -> acc
  | h :: t ->
      if h.state = Hit then
        score
          { height = board.height; width = board.width; squares = t }
          (acc + 1)
      else score { height = board.height; width = board.width; squares = t } acc
