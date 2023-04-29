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

(**[lst_to_int lst] takes a Yojson.Basic.t lst and converts it into a coordinate
   list of type (int * int) list*)
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

(**[identify_ship b r c lst] returns the ship located at (r,c), or raises
   ShipNotFound. [lst] is the list of all ships on the board*)
let rec identify_ship r c lst =
  match lst with
  | [] -> raise ShipNotFound
  | h :: t -> if List.mem (r, c) (location h) then h else identify_ship r c t

(**[row ship_list r w id] returns a t list that represents the [r]th row, with
   width [w] and [ship_list], the list of ships*)
let rec row ship_list r w id =
  if id = w then []
  else
    let st = if List.mem (id, r) (to_loc ship_list) then Full else Empty in
    let n =
      try get_name (identify_ship id r ship_list) with ShipNotFound -> "none"
    in
    { x = id; y = r; state = st; name = n } :: row ship_list r w (id + 1)

(**[full_list ship_list h w id] returns a full list of squares using
   [ship_list], the board's list of ships, as well as the dimensions of the
   board. [id] is passed onto function [row]*)
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

let make_empty board =
  let new_list =
    List.map
      (fun t -> { x = t.x; y = t.y; state = Empty; name = "none" })
      board.squares
  in
  {
    height = board.height;
    width = board.width;
    squares = new_list;
    ships = board.ships;
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
  | [] -> raise ShipNotFound
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

(**[remove_ship ship_list ship] returns a list of ships having removed [ship]*)
let rec remove_ship (ship_list : Ship.t list) (ship : Ship.t) =
  match ship_list with
  | [] -> []
  | h :: t ->
      if Ship.get_name h = Ship.get_name ship then t
      else h :: remove_ship t ship

(** [coord_check board x y loc] returns false if there is a collision on a
    specific coordinate, true otherwise *)
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

(***[collision_h board s loc] returns false if there is a collision on a ship,
  true otherwise *)
let rec collision_h (board : b) (s : Ship.t) (loc : (int * int) list) : bool =
  match loc with
  | [] -> true
  | (a, b) :: t ->
      coord_check board a b (Ship.location s) && collision_h board s t

(** [check_collision_h all_ships board] checks all of the ships on the board for
    collisions. returns false if there is a collision, true otherwise *)
let rec check_collision_h (all_ships : Ship.t list) (board : b) =
  match all_ships with
  | [] -> true
  | h :: t -> collision_h board h (Ship.location h) && check_collision_h t board

let rec check_collision (board : b) : bool =
  not (check_collision_h board.ships board)

let move_ship (board : b) (ship_name : string) move_func (x : int) (y : int) : b
    =
  let ship_of_interest = get_ship board ship_name in
  let new_ship = move_func ship_of_interest x y board.height board.width in
  let new_ship_list = new_ship :: remove_ship board.ships ship_of_interest in
  let potential_board =
    {
      height = board.height;
      width = board.width;
      squares = full_list new_ship_list board.height board.width 0;
      ships = new_ship_list;
    }
  in
  if check_collision potential_board then raise Collide else potential_board

(**[replace_square squares_list target new_square] returns a new list of squares
   where the target square, [target], is replaced by [new_square]*)
let rec replace_square (squares_list : t list) (target : t) (new_square : t) =
  match squares_list with
  | [] -> []
  | h :: t ->
      if h.x = target.x && h.y = target.y then new_square :: t
      else h :: replace_square t target new_square

(**[replace_ship ship_list target new_ship] returns a new list of ships where
   the target ship, [target], is replaced by [new_ship]*)
let rec replace_ship (ship_list : Ship.t list) (target : Ship.t)
    (new_ship : Ship.t) =
  match ship_list with
  | [] -> []
  | h :: t ->
      if get_name h = get_name target then new_ship :: t
      else replace_ship t target new_ship

let update (board : b) (x : int) (y : int) : b =
  let target_square = List.nth board.squares ((y * board.width) + x) in
  let new_square =
    if target_square.state = Full || target_square.state = Hit then
      { x; y; state = Hit; name = target_square.name }
    else { x; y; state = Miss; name = target_square.name }
  in
  match identify_ship x y board.ships with
  | exception ShipNotFound ->
      {
        height = board.height;
        width = board.width;
        squares = replace_square board.squares target_square new_square;
        ships = board.ships;
      }
  | target_ship ->
      let new_ship = hit target_ship x y in
      {
        height = board.height;
        width = board.width;
        squares = replace_square board.squares target_square new_square;
        ships = replace_ship board.ships target_ship new_ship;
      }

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

let update_outer_board (inner_board : b) (outer_board : b) (x : int) (y : int) :
    b =
  let target_square =
    List.nth inner_board.squares ((y * inner_board.width) + x)
  in
  let new_square =
    if target_square.state = Full || target_square.state = Hit then
      { x; y; state = Hit; name = target_square.name }
    else { x; y; state = Miss; name = target_square.name }
  in
  {
    height = outer_board.height;
    width = outer_board.width;
    squares = replace_square outer_board.squares target_square new_square;
    ships = outer_board.ships;
  }
