open Yojson.Basic.Util

type h = {
  x : int;
  y : int;
  hit : bool;
}

type t = {
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

let make (loc : (int * int) list) = { hits = make_h loc; status = false }

let rec location (ship : t) : (int * int) list =
  raise (Failure "Unimplmented location")

let sunk (ship : t) : t = raise (Failure "Unimplemented sunk")
let hit (ship : t) (x : int) (y : int) : t = raise (Failure "Unimplemented hit")

let place (ship : t) (x : int) (y : int) (height : int) (width : int) : t =
  raise (Failure "Unimplemented place")

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
  { hits = rotated_lst; status = ship.status }

(*(** [remains ship] returns how much of the ship is left based on the number of
  locations hit. This is a helper to [sunk ship]*) let remains : t = raise
  (Failure "Unimplemented remains")*)
