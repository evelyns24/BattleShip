open Game
open Board
open Ship
open State
open Command

(**[string_of_row row] takes a string list, [row] and turns it into a string.
   Requires [row] is a string list*)
let rec print_row (row : (string * string) list) =
  match row with
  | [] -> print_string "\n"
  | (symbol, name) :: t ->
      if name = "none" then print_string symbol
      else if name = "Aircraft_Carrier_1" then
        ANSITerminal.print_string [ ANSITerminal.white ] symbol
      else if name = "Aircraft_Carrier_2" then
        ANSITerminal.print_string [ ANSITerminal.blue ] symbol
      else if name = "Destroyer" then
        ANSITerminal.print_string [ ANSITerminal.magenta ] symbol
      else if name = "Submarine_1" then
        ANSITerminal.print_string [ ANSITerminal.red ] symbol
      else if name = "Submarine_2" then
        ANSITerminal.print_string [ ANSITerminal.cyan ] symbol
      else if name = "Cruiser" then
        ANSITerminal.print_string [ ANSITerminal.green ] symbol
      else if name = "Frigate" then
        ANSITerminal.print_string [ ANSITerminal.yellow ] symbol;
      print_row t

(**[print_board board] prints the board. Requires [board] is a string list list.*)
let rec print_board (board : (string * string) list list) =
  match board with
  | [] -> print_string "\n"
  | h :: t ->
      print_row h;
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
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Battle Ship.\n";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "> '-' represents an empty square\n\
     > 'S' represents a full square\n\
     > 'X' represents a hit\n\
     > 'M' represents a miss\n";
  print_endline
    "If you see the ship's color on your board, then you have that ship in \
     your disposal. Here are the possible ships: ";

  ANSITerminal.print_string [ ANSITerminal.default ] "> Aircraft_Carrier_1 \n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "> Aircraft_Carrier_2 \n";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "> Destroyer \n";
  ANSITerminal.print_string [ ANSITerminal.red ] "> Submarine_1 \n";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "> Submarine_2 \n";
  ANSITerminal.print_string [ ANSITerminal.green ] "> Cruiser \n";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "> Frigate \n";
  print_endline "When you enter a file, you will see the full board";
  print_endline "Please enter the name of the board file you want to load.\n";
  print_string "> ";

  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()
