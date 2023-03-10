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

let location (ship : t) : int * int list =
  raise (Failure "Unimplemented location")

let sunk (ship : t) : t = raise (Failure "Unimplemented sunk")
let hit (ship : t) (x : int) (y : int) : t = raise (Failure "Unimplemented hit")

let place (ship : t) (x : int) (y : int) (height : int) (width : int) : t =
  raise (Failure "Unimplemented place")

let rotate (ship : t) (point : int * int) : t =
  raise (Failure "Unimplemented rotate")

(** [remains ship] returns how much of the ship is left based on the number of
    locations hit. This is a helper to [sunk ship]*)
let remains : t = raise (Failure "Unimplemented remains")
