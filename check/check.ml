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
  val rotate : t -> int -> int -> int -> int -> t
end

module ShipCheck : ShipSig = Ship

module type BoardSig = sig
  type t
  type b

  exception Collide
  exception ShipNotFound
  exception RedundantHit

  val from_json : Yojson.Basic.t -> b
  val make_empty : b -> b
  val get_height : b -> int
  val get_width : b -> int
  val get_board : b -> int -> (string * string) list list
  val check_collision : b -> bool

  val move_ship :
    b ->
    string ->
    (Ship.t -> int -> int -> int -> int -> Ship.t) ->
    int ->
    int ->
    b

  val update : b -> int -> int -> b
  val response : b -> int -> int -> bool
  val score : b -> int -> int
  val is_lost : b -> bool
  val update_outer_board : b -> b -> int -> int -> b
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

  val init_state : Board.b -> Board.b -> t
  val get_inner : t -> int -> Board.b
  val get_outer : t -> int -> Board.b
  val move : t -> int -> string -> int -> int -> t
  val rotate : t -> int -> string -> int -> int -> t
  val hit : t -> int -> int -> int -> t
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
