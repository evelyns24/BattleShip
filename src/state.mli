(** Representation of dynamic battleship state.

    This module represents the state of a battleship game as it is being played,
    including the two players' current boards, the covered up boards, and
    functions that cause the state to change. *)

(**********************************************************************)

type t
(** The abstract type of values representing the game state. *)

val init_state : Board.b -> Board.b -> t
(** [init_state b1 b2] is the initial state of the game when playing battleship.
    It reads in two boards and creates a state consisting of 4 boards,
    representing the two players' inner boards and outer boards. *)

val get_p1_inner : t -> Board.b
(** [get_p1_inner state] returns player one's inner board, which has information
    on where player one placed their battleships*)

val get_p1_outer : t -> Board.b
(** [get_p1_outer state] returns player one's outer board, which has information
    on where player two has attempted to hit*)

val get_p2_inner : t -> Board.b
(** [get_p2_inner state] returns player two's inner board, which has information
    on where player two placed their battleships*)

val get_p2_outer : t -> Board.b
(** [get_p2_outer state] returns player two's outer board, which has information
    on where player one has attempted to hit*)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal

val move : int -> string -> int -> int -> t
(**[move player ship_name x y] returns a new state where player [player] moves
   the ship named [ship_name] to the right by x and up by y*)

val rotate : int -> string -> int -> int -> t
(**[rotate player ship_name x y] returns a new state where player [player]
   rotates the ship named [ship_name] counterclockwise about point ([x], [y])*)

val hit : int -> int -> int -> t
(**[hit player  x y] returns a new state where player [player] has made a hit on
   the opponet's board at point ([x], [y])*)
