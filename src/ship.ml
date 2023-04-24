open Yojson.Basic.Util

type h = {
  x : int;
  y : int;
  hit : bool;
}

type t = {
  name : string;
  hits : h list;
  status : bool;
}

exception OutOfBounds

(**[make_h points] creates an h list of points; essentially takes all the (x,y)
   coords in points and creates a record that includes x, y, and hit*)
let rec make_h (points : (int * int) list) : h list =
  match points with
  | [] -> []
  | (a, b) :: t -> { x = a; y = b; hit = false } :: make_h t

let make (name : string) (loc : (int * int) list) =
  { name; hits = make_h loc; status = false }

(**[location_h hit_list] creates a list of (int * int) from [hit_list],
   representing all of the coordinates that a ship covers on the board*)
let rec location_h (hit_list : h list) : (int * int) list =
  match hit_list with
  | [] -> []
  | h :: t -> (h.x, h.y) :: location_h t

let location (ship : t) : (int * int) list = location_h ship.hits
let get_status (ship : t) : bool = ship.status
let get_name (ship : t) : string = ship.name

(**[square_status_helper hlist a b] helper for finding the status of a square in
   the ship*)
let rec square_status_helper (hlist : h list) (a : int) (b : int) : bool =
  match hlist with
  | [] -> false
  | h :: t -> if h.x = a && h.y = b then h.hit else square_status_helper t a b

let get_square_status (ship : t) (point : int * int) : bool =
  let x, y = point in
  square_status_helper ship.hits x y

(**[sunk_h hit_list] returns true if every single location in [hit_list] has
   been hit, representing that the ship has been sunk. false is returned
   otherwise*)
let rec sunk_h (hit_list : h list) : bool =
  match hit_list with
  | [] -> true
  | h :: t -> h.hit && sunk_h t

let sunk (ship : t) : t =
  if sunk_h ship.hits then { name = ship.name; hits = ship.hits; status = true }
  else { name = ship.name; hits = ship.hits; status = false }

(**[hit_h hit_list x y] checks which position (or h) of the [h_list] is hit with
   [x] and [y] coordinates and constructs a new h list*)
let rec hit_h (hit_list : h list) (x : int) (y : int) : h list =
  match hit_list with
  | [] -> []
  | h :: t ->
      if h.x = x && h.y = y then { x = h.x; y = h.y; hit = true } :: t
      else h :: hit_h t x y

let hit (ship : t) (x : int) (y : int) : t =
  { name = ship.name; hits = hit_h ship.hits x y; status = ship.status }

(** [h_list lst v h height width] returns an h list where v is added to a.x and
    h is added to a.y for all a in lst. Raises OutOfBounds if the new coords are
    outside of board *)
let rec h_list lst v h height width =
  match lst with
  | [] -> []
  | head :: tail ->
      let x1 = head.x + v in
      let y1 = head.y + h in
      if x1 < 0 || x1 > width || y1 < 0 || y1 > height then raise OutOfBounds
      else { x = x1; y = y1; hit = head.hit } :: h_list tail v h height width

let rec place (ship : t) (x : int) (y : int) (height : int) (width : int) : t =
  let lst = h_list ship.hits x y height width in
  { name = ship.name; hits = lst; status = ship.status }

(**[rotate_list h_lst c_x c_y] takes in an h list [h] that rotates the (x,y)
   portion of the h about the point (c_x, c_y) for every h in the h list*)
let rec rotate_list (h_lst : h list) (c_x : int) (c_y : int) : h list =
  match h_lst with
  | [] -> []
  | h :: t ->
      { x = c_x + c_y - h.y; y = h.x - c_x + c_y; hit = h.hit }
      :: rotate_list t c_x c_y

let rotate (point : int * int) (ship : t) : t =
  let x, y = point in
  let rotated_lst = rotate_list ship.hits x y in
  { name = ship.name; hits = rotated_lst; status = ship.status }
