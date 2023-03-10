(** Representation of the ship data

    This module represents a ship, which includes its size, shape, and location *)

type t
(** The abstract type of values representing ships *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the ship that [j] represents. Requires: [j] is a valid ship
    representation *)

(** function that returns the location of the ship *)

(** function that returns the shape of the ship *)

(** function that returns the size of the ship *)

val place : int -> int -> t
(** [place x y] is the ship after placing it [x] units horizontally and [y]
    units vertically*)

val rotate : int -> t
(** [rotate d] is the ship *)

(** function that changes the ship after it's hit *)
