open Game
open Board
open Ship
open State
open Command

(**[string_of_row row] takes a string list, [row] and turns it into a string.
   Requires [row] is a string list*)
let rec string_of_row (row : string list) =
  match row with
  | [] -> "\n"
  | h :: t -> h ^ string_of_row t

(**[print_board board] prints the board. Requires [board] is a string list list.*)
let rec print_board (board : string list list) =
  match board with
  | [] -> print_string "\n"
  | h :: t ->
      print_string (string_of_row h);
      print_board t

(** [play_game f] starts the battle ship game in file [f]. *)
let play_game f =
  if Sys.file_exists f then
    let board1 = from_json (Yojson.Basic.from_file f) in
    print_board (get_board board1 (get_width board1))
  else print_endline "File not found. Exiting";
  exit 0

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Battle Ship.\n";
  print_endline "Please enter the name of the board file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()
