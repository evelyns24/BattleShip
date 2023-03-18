open Game

module type ShipSig = sig
  type t

  exception OutOfBounds

  val make : (int * int) list -> t
  val location : t -> (int * int) list
  val get_status : t -> bool
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
  val check_collision : b -> bool
  val update : b -> int -> int -> b
  val response : b -> int -> int -> bool
  val score : b -> int -> int
end

module BoardCheck : BoardSig = Board

module type CommandSig = sig
  type object_phrase = string list

  type command =
    | Go of object_phrase
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
  val hours_worked : int
end

module AuthorCheck : AuthorSig = Author

let _ = if Author.hours_worked < 0 then exit 1
