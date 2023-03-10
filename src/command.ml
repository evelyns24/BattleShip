(* Note: You may introduce new code anywhere in this file. *)

type object_phrase = string list

type command =
  | Go of object_phrase
  | Quit

exception Empty
exception Malformed

let parse str = raise (Failure "Unimplemented parse")
