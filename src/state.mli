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

val get_inner : t -> int -> Board.b
(** [get_inner state player] returns [player] inner board, which has information
    on where player one placed their battleships*)

val get_outer : t -> int -> Board.b
(** [get_p1_outer state player] returns [player] outer board, which has
    information on where player two has attempted to hit*)

val move : t -> int -> string -> int -> int -> t
(**[move state player ship_name x y] returns a new state where player [player],
   currently at state [state], moves the ship named [ship_name] to the right by
   x and up by y*)

val rotate : t -> int -> string -> int -> int -> t
(**[rotate player ship_name x y] returns a new state where player [player]
   rotates the ship named [ship_name] counterclockwise about point ([x], [y])*)

val hit : t -> int -> int -> int -> t
(**[hit player  x y] returns a new state where player [player] has made a hit on
   the opponet's board at point ([x], [y])*)

val is_hit : t -> int -> int -> int -> bool
(**[is_hit state player x y] returns true if [player] just made a hit on their
   opponent's board at point ([x], [y]). *)
