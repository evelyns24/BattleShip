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
let rec print_board_h (board : (string * string) list list) =
  match board with
  | [] -> print_string "\n"
  | h :: t ->
      print_row h;
      print_board_h t

(**[print_board board] prints [board]*)
let print_board board = board |> get_width |> get_board board |> print_board_h

(**[print_ships ()] prints the ships in their colors*)
let print_ships () =
  ANSITerminal.print_string [ ANSITerminal.default ] "> Aircraft_Carrier_1 \n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "> Aircraft_Carrier_2 \n";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "> Destroyer \n";
  ANSITerminal.print_string [ ANSITerminal.red ] "> Submarine_1 \n";
  ANSITerminal.print_string [ ANSITerminal.cyan ] "> Submarine_2 \n";
  ANSITerminal.print_string [ ANSITerminal.green ] "> Cruiser \n";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "> Frigate \n"

(**[get_command command] attempts to parse the command*)
let rec get_command (command : string) =
  try parse command
  with excepction ->
    print_string
      "Please input a valid command. Commands can be formated as \n\
       1. move [ship_name] x y \n\
       2. rotate [ship_name] x y \n\
       3. hit x y \n\
       > ";
    get_command
      (match read_line () with
      | exception End_of_file -> ""
      | s -> s)

(**[customize_board state player] returns a new state after [player]'s board has
   been modified*)
let rec customize_board state player =
  print_endline "\nThis is what your board currently looks like:";
  print_board (get_inner state player);
  print_endline "\nHere are all of the possible ships: ";
  print_ships ();
  print_string "\nAre you done customizing your board? \n> ";
  match read_line () with
  | "quit" -> exit 0
  | "yes" ->
      print_endline "This is your final board";
      print_board (get_inner state player);
      state
  | "no" -> (
      print_string "\nWhat would you like to do? \n> ";
      match get_command (read_line ()) with
      | Move { name; coordinate = x, y } ->
          handle_customization move state player name x y
      | Rotate { name; coordinate = x, y } ->
          handle_customization rotate state player name x y
      | Quit -> exit 0
      | Hit (x, y) ->
          print_endline
            "This is not the attack phase of the game. Please input a move or \
             rotate command \n\
             > ";
          customize_board state player)
  | exception End_of_file -> customize_board state player
  | _ ->
      print_endline "Please input \'yes\', \'no\', \'quit\'";
      customize_board state player

and handle_customization func state player name x y =
  try customize_board (func state player name x y) player with
  | ShipNotFound ->
      print_endline "No such ship exists. Please try again";
      customize_board state player
  | OutOfBounds ->
      print_endline "You've moved out of bounds. Please try again";
      customize_board state player
  | Collide ->
      print_endline "You've collided with another ship. Please try again";
      customize_board state player

(**[play state] facilitates one round of game play. One round includes player
   1's turn AND player 2's turn*)
let rec play state = ()
(* if notWin (get_inner state 1) *)

(** [play_game f] starts the battle ship game in file [f]. *)
let play_game f1 f2 =
  let b1 =
    try from_json (Yojson.Basic.from_file f1)
    with excepction ->
      print_endline "File for board1 not found. Exiting";
      exit 0
  in
  let b2 =
    try from_json (Yojson.Basic.from_file f2)
    with excepction ->
      print_endline "File for board 2 not found. Exiting";
      exit 0
  in
  let game_state = init_state b1 b2 in
  print_endline
    "\n\
     Player 1, please begin customizing your board. Moving ships out of bounds \
     or within 1 square of another ship will do nothing. You may use \n\
     move [ship_name] x y or rotate [ship_name] x y commands.";
  let new_state = customize_board game_state 1 in
  print_endline
    "\n\
     Player 2, please begin customizing your board. You may use \n\
     Move x y or Rotate x y commands.";
  let completed_state = customize_board new_state 2 in
  print_endline "We are now ready to begin";
  play completed_state

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
  print_ships ();
  print_endline "When you enter a file, you will see the full board";
  print_endline "Please enter the name of the board file you want to load.\n";
  print_string "> ";

  match read_line () with
  | exception End_of_file -> ()
  | file_name1 -> (
      print_endline "\nPlease enter the name of the second board file\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | file_name2 ->
          play_game
            (data_dir_prefix ^ file_name1 ^ ".json")
            (data_dir_prefix ^ file_name2 ^ ".json"))

(* Execute the game engine. *)
let () = main ()
