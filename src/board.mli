(** Representation of a board, which has a height, width, and list of
    ships.contents Each board is a list of squares, that have four possible
    states, Empty, Full, Hit, Miss. *)

type t
(**This abstract type of values representing a board square*)

type b
(**This abstract type represents the board*)

exception Collide
exception ShipNotFound

val from_json : Yojson.Basic.t -> b
(** [from_json b] is the board that [b] represents. Requires: [b] is a valid
    JSON board representation. *)

val make_empty : b -> b
(**[make_empty board] returns the empty version of [board], so that there are no
   ships and all of the squares are in the Emtpy state*)

val get_height : b -> int
(**[get_height board] returns the height of the board. Requires board is a valid
   board*)

val get_width : b -> int
(**[get_width board] returns the width of the board. Requires board is a valid
   board*)

val get_board : b -> int -> (string * string) list list
(**[get_board board w] returns a list of (string, string) lists that represent
   the board with rows of lenght w. [Requires]: length of board is divisible by
   w*)

val check_collision : b -> bool
(**[check_collision board] returns true if any ship is within a one square
   distance of any other ship.*)

val update : b -> int -> int -> b
(**[update board x y] returns a new board that responds to a hit at (x,y)*)

val move_ship :
  b ->
  string ->
  (Ship.t -> int -> int -> int -> int -> Ship.t) ->
  int ->
  int ->
  b
(**[move_ship board ship_name move_func x y] returns a new board given that the
   ship named [ship_name] has been moved according to the function[move_func].
   If the ship is moved out of bounds, the original board is returned*)

val response : b -> int -> int -> bool
(**[response board x y] returns true if the board square located at (x,y) is
   Full or Hit*)

val score : b -> int -> int
(**[score board acc] returns the score associated with this board, essentially
   the other player's score. This function is tail recursive*)
