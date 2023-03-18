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
