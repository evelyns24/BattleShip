(** Representation of the ship data

    This module represents a ship, which represents a ship's location and the
    status of each unit of the ship. Think of the board as the first quadrant of
    the x-y plane, where the origin is at the bottom left corner. A ship's
    status is false while it is not sunk, and becomes true when it is sunk*)

type t
(** The abstract type of values representing ships *)

exception OutOfBounds
(**Raise when a portion of the ship is out of bounds*)

val make : string -> (int * int) list -> t
(**[make loc] takes in a list of points and creates a ship*)

val location : t -> (int * int) list
(** [location ship] returns the location of the ship as a list of tuple of 2
    integers.*)

val get_status : t -> bool
(** [get_status ship] returns the status for the ship. If it sank, then [true],
    else [false]*)

val get_name : t -> string
(** [get_name ship] returns the name of the ship. *)

val get_square_status : t -> int * int -> bool
(**[get_square_state ship point] returns the status of the point on that ship.
   Requires point is on the ship*)

val sunk : t -> t
(**[sunk ship] updates [ship] if it has been sunk. *)

val hit : t -> int -> int -> t
(**[hit ship x y] processes a hit on this ship at location [(x, y)] and returns
   a new ship. *)

val place : t -> int -> int -> int -> int -> t
(** [place ship x y height width] moves the ship [t] [x] units horizontally and
    [y] units vertically. Raises [OutofBounds] exception if part of the ship
    exceeds the bounds defined by [height] and [width]*)

val rotate : int * int -> t -> t
(** [rotate point ship ] rotates the orientation of [ship] about [point] by 90
    degrees counter clockwise. Requires [point] is on the ship*)
