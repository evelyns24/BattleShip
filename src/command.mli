(** Parsing of player commands. *)

type coord = int * int
(** the first element of this list is the x coord and the second is the y coord.
    Must be length 2 **)

(** The type [command] represents a player command that is decomposed into a
    verb and possibly an object phrase. Invariant: the [name] and [coord]
    carried by [Move] and [Rotate] must not be empty. The [coord] carried by
    [Hit] must not be empty*)
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
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. The rest of the words, if any, become the object phrase. Examples:

    - [parse "        hit    1  5   "] is [Hit \(1,5\)]
    - [parse " quit   "] is [Quit]
    - [parse "    move   Cruiser    1  5   "] is [Move \{"Cruiser"; 1; 5\}]
    - [parse "    rotate   Destroyer    2  1   "] is
      [Rotate \{"Destroyer"; 2; 1\}]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is not malformed
    only if:

    - the verb is "quit" and there is an empty object phrase; or
    - the verb is "move"/"rotate" and there is an object phrase of 3 elements,
      where the first element is a string and both the second and third are
      integers; or
    - the verb is "hit" and there is an object phrase of size 2, where both
      elements are integers. *)
