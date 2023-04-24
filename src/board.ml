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
  mutable state : s;
  name : string;  (** "none" if this square is Empty *)
}

type b = {
  height : int;
  width : int;
  squares : t list;
  ships : Ship.t list;
}

exception Collide
exception ShipNotFound

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
      let coord = location h in
      coord @ to_loc t

(** [to_ship lst] returns a list of ships **)
let rec to_ship lst =
  match lst with
  | [] -> []
  | h :: t ->
      let n = h |> member "name" |> to_string in
      let coord = h |> member "location" |> to_list |> lst_to_int in
      make n coord :: to_ship t

(**[identify_ship b r c lst] returns the name of the ship located at (r,c), or
   raises ShipNotFound. [lst] is the list of all ships on the board*)
let rec identify_ship r c lst =
  match lst with
  | [] -> raise ShipNotFound
  | h :: t ->
      if List.mem (r, c) (location h) then get_name h else identify_ship r c t

(**[row ship_list r w id] returns a t list that represents the [r]th row, with
   width [w] and [ship_list], the list of ships*)
let rec row ship_list r w id =
  if id = w then []
  else
    let st = if List.mem (id, r) (to_loc ship_list) then Full else Empty in
    let n = try identify_ship id r ship_list with ShipNotFound -> "none" in
    { x = id; y = r; state = st; name = n } :: row ship_list r w (id + 1)

let rec full_list ship_list h w id =
  if id = h then [] else row ship_list id w 0 @ full_list ship_list h w (id + 1)

let from_json (j : Yojson.Basic.t) : b =
  let h = j |> member "height" |> to_int in
  let w = j |> member "width" |> to_int in
  let ship_lst = j |> member "ships" |> to_list |> to_ship in
  {
    height = h;
    width = w;
    squares = full_list ship_lst h w 0;
    ships = ship_lst;
  }

let get_height (board : b) : int = board.height
let get_width (board : b) : int = board.width

(**[from_state state] takes state s and turns it into a string according to:
   Empty -> "-"; Full -> "S"; Hit -> "X"; Miss -> "M" *)
let from_state (state : s) : string =
  match state with
  | Empty -> "-"
  | Full -> "S"
  | Hit -> "X"
  | Miss -> "M"

(**[to_string_list lst] takes lst, a t list, and turns it into a (string,
   string) list, using from_state*)
let rec to_pair_list = function
  | [] -> []
  | h :: t -> (from_state h.state, h.name) :: to_pair_list t

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

let rec get_board (board : b) (w : int) =
  let string_list = to_pair_list board.squares in
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

let rec get_ship (board : b) (ship_name : string) =
  match board.ships with
  | [] -> raise (Failure "Ship not found")
  | h :: t ->
      if get_name h = ship_name then h
      else
        get_ship
          {
            height = board.height;
            width = board.width;
            squares = board.squares;
            ships = t;
          }
          ship_name

let update_squares squares ship = raise (Failure "unimplemented")
(*Ideas: first remove the original ship from squares. Then add in the new
  square*)

let move_ship (board : b) (ship_name : string) (rotate : bool) (x : int)
    (y : int) : b =
  if rotate then
    let ship_of_interest = get_ship board ship_name in
    let new_ship = Ship.rotate (x, y) ship_of_interest in
    {
      height = board.height;
      width = board.width;
      squares = update_squares board.squares new_ship;
      ships = board.ships;
    }
  else
    {
      height = board.height;
      width = board.width;
      squares = board.squares;
      ships = board.ships;
    }

(* returns false if there is a collision on a specific coordinate, true
   otherwise *)
let coord_check (board : b) (x : int) (y : int) (loc : (int * int) list) : bool
    =
  if response board (x - 1) (y + 1) && not (List.mem (x - 1, y + 1) loc) then
    false
  else if response board x (y + 1) && not (List.mem (x, y + 1) loc) then false
  else if response board (x + 1) (y + 1) && not (List.mem (x + 1, y + 1) loc)
  then false
  else if response board (x + 1) y && not (List.mem (x + 1, y) loc) then false
  else if response board (x + 1) (y - 1) && not (List.mem (x + 1, y - 1) loc)
  then false
  else if response board x (y - 1) && not (List.mem (x, y - 1) loc) then false
  else if response board (x - 1) (y - 1) && not (List.mem (x - 1, y - 1) loc)
  then false
  else if response board (x - 1) y && not (List.mem (x - 1, y) loc) then false
  else true

(* returns false if there is a collision on a ship, true otherwise *)
let rec collision_h (board : b) (s : Ship.t) (loc : (int * int) list) : bool =
  match loc with
  | [] -> true
  | (a, b) :: t ->
      coord_check board a b (Ship.location s) && collision_h board s t

(* checks all of the ships on the board for collisions. returns false if there
   is a collision, true otherwise *)
let rec check_collision_h (all_ships : Ship.t list) (board : b) =
  match all_ships with
  | [] -> true
  | h :: t -> collision_h board h (Ship.location h) && check_collision_h t board

let rec check_collision (board : b) : bool =
  not (check_collision_h board.ships board)

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
