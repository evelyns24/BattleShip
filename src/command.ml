(* Note: You may introduce new code anywhere in this file. *)

type coord = int * int

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

let is_int x = try int_of_string x with Failure e -> raise Malformed

let parse str =
  let stripped_list =
    List.filter (fun x -> x <> "") (String.split_on_char ' ' str)
  in
  match stripped_list with
  | [] -> raise Empty
  | [ "quit" ] -> Quit
  | "quit" :: t -> raise Malformed
  | [ "move"; n; x; y ] -> Move { name = n; coordinate = (is_int x, is_int y) }
  | [ "rotate"; n; x; y ] ->
      Rotate { name = n; coordinate = (is_int x, is_int y) }
      (*board handles nonexisting ships*)
  | [ "hit"; x; y ] -> Hit (is_int x, is_int y)
  | _ -> raise Malformed
