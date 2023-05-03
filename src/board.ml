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
exception RedundantHit

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
   Empty -> "∙"; Full -> "S"; Hit -> "X"; Miss -> "M" *)
let from_state (state : s) : string =
  match state with
  | Empty -> "∙"
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
let coord_check (board : b) (x : int) (y : int) (ship : Ship.t)
    (loc : (int * int) list) : bool =
  let this_ship = identify_ship x y board.ships in
  if response board x y && ship <> this_ship then false
  else if response board (x - 1) (y + 1) && not (List.mem (x - 1, y + 1) loc)
  then false
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
      coord_check board a b s (Ship.location s) && collision_h board s t

(** [check_collision_h all_ships board] checks all of the ships on the board for
    collisions. returns false if there is a collision, true otherwise *)
let rec check_collision_h (all_ships : Ship.t list) (board : b) =
  match all_ships with
  | [] -> true
  | h :: t -> collision_h board h (Ship.location h) && check_collision_h t board

(**[check_collision board] returns true if any ship is within a one square
   distance of any other ship. [Raises] [Collide] if any ships are within 1
   square of each other.*)
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
      else h :: replace_ship t target new_ship

(**[in_bounds board x y] returns true if the points (x,y) is in bounds of the
   board*)
let in_bounds board x y =
  not (x < 0 || x >= get_width board || y < 0 || y >= get_height board)

(**[get_square board point] returns the square at [point]*)
let get_square board point =
  let x, y = point in
  List.nth board.squares ((y * board.width) + x)

(**[get_square_border board x y loc] returns a list of coordinates that surround
   the point ([x],[y]) if this point is not already in [loc]*)
let get_square_border (board : b) (x : int) (y : int) (loc : (int * int) list) =
  let ret = [] in
  let top_left =
    if in_bounds board (x - 1) (y + 1) && not (List.mem (x - 1, y + 1) loc) then
      get_square board (x - 1, y + 1) :: ret
    else ret
  in
  let top_middle =
    if in_bounds board x (y + 1) && not (List.mem (x, y + 1) loc) then
      get_square board (x, y + 1) :: top_left
    else top_left
  in
  let top_right =
    if in_bounds board (x + 1) (y + 1) && not (List.mem (x + 1, y + 1) loc) then
      get_square board (x + 1, y + 1) :: top_middle
    else top_middle
  in
  let left =
    if in_bounds board (x + 1) y && not (List.mem (x + 1, y) loc) then
      get_square board (x + 1, y) :: top_right
    else top_right
  in
  let bottom_left =
    if in_bounds board (x + 1) (y - 1) && not (List.mem (x + 1, y - 1) loc) then
      get_square board (x + 1, y - 1) :: left
    else left
  in
  let right =
    if in_bounds board x (y - 1) && not (List.mem (x, y - 1) loc) then
      get_square board (x, y - 1) :: bottom_left
    else bottom_left
  in
  let bottom_right =
    if in_bounds board (x - 1) (y - 1) && not (List.mem (x - 1, y - 1) loc) then
      get_square board (x - 1, y - 1) :: right
    else right
  in
  let bottom_middle =
    if in_bounds board (x - 1) y && not (List.mem (x - 1, y) loc) then
      get_square board (x - 1, y) :: bottom_right
    else bottom_right
  in
  bottom_middle

(**[get_border board ship_coords] returns a coordinate list of the border around
   a ship with [ship_coords]. May contain duplicates*)
let rec get_border (board : b) (ship_coords : (int * int) list) =
  match ship_coords with
  | [] -> []
  | (a, b) :: t -> get_square_border board a b ship_coords @ get_border board t

(**[replace_all squares border] replaces all of the squares in [squares] that
   are also in [border] with an identical square except for a new state, Miss*)
let rec replace_all (squares : t list) = function
  | [] -> squares
  | h :: t ->
      if h.state = Hit then replace_all squares t
      else
        replace_all
          (replace_square squares h
             { x = h.x; y = h.y; state = Miss; name = h.name })
          t

(**[cmp_squares s1 s2] returns different values based on the x and y coords of
   the two squares*)
let cmp_squares s1 s2 =
  if s1.x = s2.x && s1.y = s2.y then 0
  else if s1.x = s2.x && s1.y < s2.y then ~-1
  else if s1.x = s2.x && s1.y > s2.y then 1
  else if s1.x < s2.x && s1.y < s2.y then ~-3
  else if s1.x < s2.x && s1.y > s2.y then ~-2
  else if s1.x > s2.x && s1.y < s2.y then 2
  else 3

(**[reveal_border board squares ship] returns a list of squares where all of the
   border squares around ship [ship] are revealed.*)
let reveal_border (board : b) (squares : t list) (ship : Ship.t) =
  let border_squares =
    List.sort_uniq cmp_squares (get_border board (location ship))
  in
  replace_all squares border_squares

let update (board : b) (x : int) (y : int) : b =
  if x < 0 || x >= get_width board || y < 0 || y >= get_height board then
    raise OutOfBounds
  else
    let target_square = List.nth board.squares ((y * board.width) + x) in
    let new_square =
      if target_square.state = Full then
        { x; y; state = Hit; name = target_square.name }
      else if target_square.state = Empty then
        { x; y; state = Miss; name = target_square.name }
      else raise RedundantHit
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
        let new_ship = sunk (hit target_ship x y) in
        let new_squares =
          replace_square board.squares target_square new_square
        in
        if not (get_status new_ship) then
          {
            height = board.height;
            width = board.width;
            squares = new_squares;
            ships = replace_ship board.ships target_ship new_ship;
          }
        else
          {
            height = board.height;
            width = board.width;
            squares = reveal_border board new_squares new_ship;
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

let is_lost (board : b) : bool =
  List.fold_left (fun acc ship -> acc && get_status ship) true board.ships

let rec new_outer_squares (inner_board_squares : t list) =
  match inner_board_squares with
  | [] -> []
  | { x; y; state; name } :: t ->
      let new_state = if state = Full then Empty else state in
      { x; y; state = new_state; name = "none" } :: new_outer_squares t

let update_outer_board (inner_board : b) : b =
  {
    height = inner_board.height;
    width = inner_board.width;
    squares = new_outer_squares inner_board.squares;
    ships = [];
  }
