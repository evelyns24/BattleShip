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
  squares : s list list;
}

exception Collide

let from_json (j : Yojson.Basic.t) : b =
  raise (Failure "Unimplemented Evelyn's Problem")

let get_height (board : b) : int = board.height
let get_width (board : b) : int = board.width
let check_collision (board : b) : bool = raise (Failure "Alisha's problem")

let response (board : b) (x : int) (y : int) : bool =
  raise (Failure "Gloria's Problem")

(**[score_one_row row acc] returns the score of one row by incrementing [acc]
   when it reaches a square that is Hit. This function is tail recursive*)
let rec score_one_row (row : s list) (acc : int) : int =
  match row with
  | [] -> acc
  | h :: t -> if h = Hit then score_one_row t (acc + 1) else score_one_row t acc

let rec score (board : b) : int =
  match board.squares with
  | [] -> 0
  | h :: t ->
      score_one_row h 0
      + score { height = board.height; width = board.width; squares = t }
