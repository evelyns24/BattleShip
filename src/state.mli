(** Representation of dynamic adventure state.

    This module represents the state of an adventure as it is being played,
    including the adventurer's current room, the rooms that have been visited,
    and functions that cause the state to change. *)

(**********************************************************************)

type t
(** The abstract type of values representing the game state. *)

val init_state : Ship.t -> Ship.t -> t
(** [init_state a] is the initial state of the game when playing adventure [a].
    In that state the adventurer is currently located in the starting room, and
    they have visited only that room. *)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal

val go : string -> Ship.t -> t -> result
(** [go exit adv st] is the result of attempting to go through the exit named
    [exit] in state [st] and adventure [adv]:

    - If [exit] is the name of an exit from the adventurer's current room, then
      the result is [Legal st'], where in [st'] the adventurer is now located in
      the room to which [exit] leads.

    - Otherwise, the result is [Illegal].

    Effects: none. In particular, [go] does not print anything. *)
