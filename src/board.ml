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
  ships : Ship.t list;
}

exception Collide

let rec lst_to_int lst =
  match lst with
  | [] -> []
  | h :: t ->
      ( h |> to_list |> List.hd |> to_int,
        h |> to_list |> List.tl |> List.hd |> to_int )
      :: lst_to_int t

(**[to_loc lst] returns a list of all of the ship coordinates*)
let rec to_loc lst =
  match lst with
  | [] -> []
  | h :: t ->
      let coord = h |> member "location" |> to_list |> lst_to_int in
      coord @ to_loc t

(** [to_ship lst] returns a list of ship **)
let rec to_ship lst =
  match lst with
  | [] -> []
  | h :: t ->
      let n = h |> member "name" |> to_string in
      let coord = h |> member "location" |> to_list |> lst_to_int in
      make n coord :: to_ship t

let rec row ship_list r w id =
  if id = w then []
  else
    let st = if List.mem (id, r) ship_list then Full else Empty in
    { x = id; y = r; state = st } :: row ship_list r w (id + 1)

let rec full_list ship_list h w id =
  if id = h then [] else row ship_list id w 0 @ full_list ship_list h w (id + 1)

let from_json (j : Yojson.Basic.t) : b =
  let h = j |> member "height" |> to_int in
  let w = j |> member "width" |> to_int in
  let lst = j |> member "ships" |> to_list |> to_loc in
  let s = j |> member "ships" |> to_list |> to_ship in
  { height = h; width = w; squares = full_list lst h w 0; ships = s }

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
  else dimensionalize (after_w lst w) w @ [ first_w lst w ]

let rec get_board (board : b) (w : int) : string list list =
  let string_list = to_string_list board.squares in
  dimensionalize string_list w

let rec response (board : b) (x : int) (y : int) : bool =
  match board.squares with
  | [] -> false
  | h :: t ->
      if h.x = x && h.y = y then h.state = Full || h.state = Hit
      else
        response
          {
            height = board.height;
            width = board.width;
            squares = t;
            ships = board.ships;
          }
          x y

let update (board : b) (x : int) (y : int) : b =
  raise (Failure "unimplemented update")

let rec score (board : b) (acc : int) : int =
  match board.squares with
  | [] -> acc
  | h :: t ->
      if h.state = Hit then
        score
          {
            height = board.height;
            width = board.width;
            squares = t;
            ships = board.ships;
          }
          (acc + 1)
      else
        score
          {
            height = board.height;
            width = board.width;
            squares = t;
            ships = board.ships;
          }
          acc
