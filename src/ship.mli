(** Representation of the ship data

    This module represents a ship, which includes its size, shape, and location *)

type t
(** The abstract type of values representing ships *)

exception OutOfBounds
(**Raise when a portion of the ship is out of bounds*)

val location : t -> int * int list
(** [location ship] returns the location of the ship as a list of tuple of 2
    integers.*)

val sunk : t -> t
(**[sunk ship] updates [ship] if it has been sunk. *)

val hit : t -> int -> int -> t
(**[hit ship x y] processes a hit on this ship at location [(x, y)] and returns
   a new ship. *)

val place : t -> int -> int -> int -> int -> t
(** [place ship x y height width] moves the ship [t] [x] units horizontally and
    [y] units vertically. Raises [OutofBounds] exception if part of the ship
    exceeds the bounds defined by [height] and [width]*)

val rotate : t -> int * int -> t
(** [rotate ship point] rotates the orientation of [ship] about [point] by 90
    degrees counter clockwise. Requires [point] is on the ship*)
