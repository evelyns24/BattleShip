open Game

module type ShipSig = sig
  type t

  exception OutOfBounds

  val make : string -> (int * int) list -> t
  val location : t -> (int * int) list
  val get_status : t -> bool
  val get_name : t -> string
  val get_square_status : t -> int * int -> bool
  val sunk : t -> t
  val hit : t -> int -> int -> t
  val place : t -> int -> int -> int -> int -> t
  val rotate : int * int -> t -> t
end

module ShipCheck : ShipSig = Ship

module type BoardSig = sig
  type t
  type b

  exception Collide

  val from_json : Yojson.Basic.t -> b
  val get_height : b -> int
  val get_width : b -> int
  val get_board : b -> int -> string list list
  val check_collision : b -> bool
  val move_ship : b -> string -> bool -> int -> int -> b
  val update : b -> int -> int -> b
  val response : b -> int -> int -> bool
  val score : b -> int -> int
end

module BoardCheck : BoardSig = Board

module type CommandSig = sig
  type coord = int * int
  type object_phrase = string list

  type command =
    | Move of {
        name : string;
        coordinate : coord;
      }
    | Rotate of {
        name : string;
        coordinate : coord;
      }
    | Hit of coord
    | Quit

  exception Empty
  exception Malformed

  val parse : string -> command
end

module CommandCheck : CommandSig = Command

module type StateSig = sig
  type t

  val init_state : Ship.t -> Ship.t -> t

  type result =
    | Legal of t
    | Illegal

  val go : string -> Ship.t -> t -> result
end

module StateCheck : StateSig = State

module type AuthorSig = sig
  val hours_worked_gloria : int
  val hours_worked_alisha : int

  val hours_worked_evelyn : int
  (** [hours_worked] is the number of hours Evelyn worked on this assignment. *)
end

module AuthorCheck : AuthorSig = Author

let _ =
  if
    Author.hours_worked_gloria < 0
    || Author.hours_worked_alisha < 0
    || Author.hours_worked_evelyn < 0
  then exit 1
