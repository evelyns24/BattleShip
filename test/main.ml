open OUnit2
open Game
open Ship
open Command

(* Test Plan: We will create both blackbox and glassbox test cases. First, we
   will create test cases based on the specs of the functions (this will be the
   blackbox test cases). Then, we will add more test cases based on the coverage
   shown in Bisect (this will be the glassbox test cases) to achieve a higher
   coverage. We will test all functions in ship.mli using helper functions that
   create OUnit test. We will then test all functions, except for from_json, in
   board.mli also using helper fucntions that create OUint test. We wll omit
   from_json because it is used in other functions, which we will be testing. *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_coord (point : int * int) =
  let x, y = point in
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let string_of_list ?(open_delim = "[") ?(close_delim = "]") ?(sep = "; ")
    string_of_elt lst =
  let len = List.length lst in
  let open Buffer in
  (* As a rough lower bound assume that each element takes a minimum of 3
     characters to represent including a separator, e.g., ["v, "]. The buffer
     will grow as needed, so it's okay if that estimate is low. *)
  let buf = create (3 * len) in
  add_string buf open_delim;
  List.iteri
    (fun i v ->
      add_string buf (string_of_elt v);
      if i < len - 1 then add_string buf sep)
    lst;
  add_string buf close_delim;
  contents buf

let string_of_bindings key_to_string value_to_string lst =
  let string_of_binding (k, v) =
    Printf.sprintf "%s: %s" (key_to_string k) (value_to_string v)
  in
  string_of_list ~open_delim:"{" ~close_delim:"}" ~sep:", " string_of_binding
    lst

(** [string_tuple_list lst] is the string version of all elements in [lst] *)
let rec string_tuple_list lst =
  match lst with
  | [] -> ""
  | h :: t -> string_of_bindings Fun.id Fun.id h ^ string_tuple_list t

(********************************************************************
   End helper functions.
 ********************************************************************)

let data_dir_prefix = "data" ^ Filename.dir_sep
let basic_ship = make "submarine" [ (1, 1) ]
let two_long = make "frigate" [ (3, 1); (3, 2) ]
let three_long = make "cruiser" [ (2, 2); (2, 3); (2, 1) ]
let four_long = make "destroyer" [ (6, 4); (5, 4); (4, 4); (3, 4) ]
let l_shaped = make "aircraft carrier" [ (2, 4); (2, 5); (3, 5); (4, 5) ]

(**[test_location name ship output] builds an OUnit test named [name] that tests
   whether or not [location ship] returns the expected output [output]*)
let test_location (name : string) (ship : t) (output : (int * int) list) =
  name >:: fun _ ->
  assert_equal output (location ship) ~cmp:cmp_set_like_lists
    ~printer:(pp_list pp_coord)

(**[test_get_name name ship output] constructs an OUnit test named [name] that
   asserts the quality of [expected_output] with [get_name ship]. *)
let test_get_name name ship output =
  name >:: fun _ -> assert_equal output (get_name ship) ~printer:Fun.id

(**[test_rotate name ship point output] builds an OUnit test named [name] that
   tests whether or not [rotate point ship] rotates the ship correctly by
   comparing the location after the rotatation with the expected output [output]*)
let test_rotate (name : string) (ship : t) x y (output : (int * int) list) =
  name >:: fun _ ->
  assert_equal output
    (rotate ship x y 8 8 |> location)
    ~cmp:cmp_set_like_lists ~printer:(pp_list pp_coord)

(** [place_test name ship x y height width expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [place ship x y height width]. *)
let place_test (name : string) (ship : Ship.t) (x : int) (y : int)
    (height : int) (width : int) (expected_output : (int * int) list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (location (place ship x y height width))
    ~cmp:cmp_set_like_lists ~printer:(pp_list pp_coord)

(** [status_test name ship expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [get_status (sunk ship)]. This also tests hit, since the only way to change
    the status of a ship is through hit *)
let status_test (name : string) (ship : Ship.t) (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (get_status (sunk ship))

(** [square_status_test name ship point expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [get_square_status ship point]. This also tests hit, since the only way to
    change the status of a ship square is through hit *)
let square_status_test (name : string) (ship : t) (point : int * int)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (get_square_status ship point)

let glassbox_place =
  [
    ( "out of bound: negative x" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> place l_shaped (-3) 1 8 8) );
    ( "out of bound: x too big" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> place l_shaped 5 1 8 8) );
    ( "out of bound: y too big" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> place l_shaped 1 5 8 8) );
    ( "out of bound: negative y" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> place l_shaped 5 (-6) 8 8) );
    place_test "up 1 left 2" l_shaped (-2) 1 8 8
      [ (0, 5); (0, 6); (1, 6); (2, 6) ];
    place_test "down 4 left 2" l_shaped (-2) (-4) 8 8
      [ (0, 0); (0, 1); (1, 1); (2, 1) ];
  ]

let glass_rotate_test =
  [
    test_rotate "rotate basic 1x" basic_ship 1 1 [ (1, 1) ];
    test_rotate "rotate basic 2x" (rotate basic_ship 1 1 8 8) 1 1 [ (1, 1) ];
    test_rotate "rotate two 1x" two_long 3 1 [ (3, 1); (2, 1) ];
    test_rotate "rotate two 3x"
      (rotate (rotate two_long 3 1 8 8) 3 1 8 8)
      3 1
      [ (3, 1); (4, 1) ];
    test_rotate "rotate L 1x" l_shaped 3 5 [ (3, 6); (3, 5); (3, 4); (4, 4) ];
    test_rotate "rotate L 4x"
      (rotate (rotate (rotate l_shaped 3 5 8 8) 3 5 8 8) 3 5 8 8)
      3 5
      [ (2, 4); (2, 5); (3, 5); (4, 5) ];
    test_rotate "rotate three 2 pts"
      (rotate three_long 2 3 8 8)
      4 3
      [ (4, 3); (4, 2); (4, 1) ];
  ]

let glass_status_test =
  [
    status_test "status of l-shaped ship before any hits" l_shaped false;
    status_test "status l_shaped ship after 1 hit" (hit l_shaped 4 5) false;
    square_status_test "square stat after 1 hit" (hit l_shaped 4 5) (4, 5) true;
    status_test "status l_shaped ship after a missed shot" (hit l_shaped 1 1)
      false;
    square_status_test "square stat after 1 missed shot" (hit l_shaped 1 1)
      (1, 1) false;
    status_test "status l_shaped ship after 2 hits"
      (hit (hit l_shaped 4 5) 3 5)
      false;
    square_status_test "square stat after 2 hits"
      (hit (hit l_shaped 4 5) 3 5)
      (3, 5) true;
    status_test "status l_shaped ship after 3 hits"
      (hit (hit (hit l_shaped 4 5) 3 5) 2 5)
      false;
    square_status_test "square stat after 3 hits"
      (hit (hit (hit l_shaped 4 5) 3 5) 2 5)
      (3, 5) true;
    status_test "status l_shaped ship after 4 hits"
      (hit (hit (hit (hit l_shaped 4 5) 3 5) 2 5) 2 4)
      true;
    square_status_test "square stat after 3 hits"
      (hit (hit (hit (hit l_shaped 4 5) 3 5) 2 5) 2 4)
      (2, 4) true;
  ]

let ship_glassbox_tests =
  List.flatten [ glass_rotate_test; glassbox_place; glass_status_test ]

let ship_blackbox_tests =
  [
    test_location "loc basic" basic_ship [ (1, 1) ];
    test_location "loc two_long" two_long [ (3, 1); (3, 2) ];
    test_location "loc four_long" four_long [ (6, 4); (3, 4); (4, 4); (5, 4) ];
    test_rotate "basic" basic_ship 1 1 [ (1, 1) ];
    test_rotate "rotate three about one end " three_long 2 1
      [ (0, 1); (1, 1); (2, 1) ];
    test_rotate "rotate three about another end " three_long 2 3
      [ (2, 3); (3, 3); (4, 3) ];
    test_rotate "rotate three about center " three_long 2 2
      [ (1, 2); (2, 2); (3, 2) ];
    test_rotate "rotate l about one end " l_shaped 2 4
      [ (2, 4); (1, 4); (1, 5); (1, 6) ];
    test_rotate "rotate l abut corner " l_shaped 2 5
      [ (3, 5); (2, 5); (2, 6); (2, 7) ];
    test_rotate "rotate l about another end " l_shaped 4 5
      [ (4, 5); (4, 4); (4, 3); (5, 3) ];
    test_rotate "rotate l about (3,5)" l_shaped 3 5
      [ (3, 4); (4, 4); (3, 5); (3, 6) ];
    place_test "place basic_ship up 1 right 1" basic_ship 1 1 8 8 [ (2, 2) ];
    ( "out of bound: negative value y's for two_long ship" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> place two_long (-2) (-3) 8 8) );
    place_test "place l-shaped ship down 4 and left 1" l_shaped (-1) (-4) 8 8
      [ (1, 0); (1, 1); (2, 1); (3, 1) ];
    ( "out of bound: x value too large for l_shaped ship" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> place l_shaped 7 2 8 8) );
    status_test "status floating basic ship" basic_ship false;
    status_test "status sunk basic ship" (hit basic_ship 1 1) true;
    status_test "status floating 2 long with one hit" (hit two_long 3 1) false;
    status_test "status sunk 2 long" (hit (hit two_long 3 1) 3 2) true;
    square_status_test "square stat floating 2 long" two_long (3, 1) false;
    square_status_test "sqr stat hit 2 long" (hit two_long 3 1) (3, 1) true;
    square_status_test "sqr stat non hit 2 long" (hit two_long 3 1) (3, 2) false;
    test_get_name "two_long: frigate" two_long "frigate";
    test_get_name "l-shape: aircraft carrier" l_shaped "aircraft carrier";
  ]

(*------------------------------------------------------------------------------*)
(*Board tests*)

let basic =
  Yojson.Basic.from_file (data_dir_prefix ^ "basic.json") |> Board.from_json

let complex =
  Yojson.Basic.from_file (data_dir_prefix ^ "complex.json") |> Board.from_json

let two_ship =
  Yojson.Basic.from_file (data_dir_prefix ^ "two_ship.json") |> Board.from_json

let three_ship =
  Yojson.Basic.from_file (data_dir_prefix ^ "three_ship.json")
  |> Board.from_json

let two_sub =
  Yojson.Basic.from_file (data_dir_prefix ^ "two_sub.json") |> Board.from_json

(**[test_make_empty name board output] constructs an OUnit test named [name]
   that asserts the quality of [output] with
   [get_board (make_empty board) (get_width board)]. *)
let test_make_empty name board output =
  name >:: fun _ ->
  assert_equal output
    (string_tuple_list
       (Board.get_board (Board.make_empty board) (Board.get_width board)))
    ~printer:Fun.id

(**[test_get_height name board output] constructs an OUnit test named [name]
   that asserts the quality of [output] with [get_height board]. *)
let test_get_height name board output =
  name >:: fun _ ->
  assert_equal output (Board.get_height board) ~printer:string_of_int

(**[test_get_width name board output] constructs an OUnit test named [name] that
   asserts the quality of [output] with [get_width board]. *)
let test_get_width name board output =
  name >:: fun _ ->
  assert_equal output (Board.get_width board) ~printer:string_of_int

(**[test_get_board name board output] constructs an OUnit test named [name] that
   asserts the quality of [output] with
   [string_tuple_list (get_board board (get_width board))]. *)
let test_get_board name board output =
  name >:: fun _ ->
  assert_equal output
    (string_tuple_list (Board.get_board board (Board.get_width board)))
    ~printer:Fun.id

(**[test_update name board x y output] constructs an OUnit test named [name]
   that asserts the quality of [output] with
   [string_tuple_list (get_board (update board x y) (get_width board))]. *)
let test_update name board x y output =
  name >:: fun _ ->
  assert_equal output
    (Board.get_board (Board.update board x y) (Board.get_width board)
    |> string_tuple_list)
    ~printer:Fun.id

(**[test_move_ship name board move_fun ship x y output] constructs an OUnit test
   named [name] that asserts the quality of [output] with the string version of
   the board [move_ship board ship move_fun x y]. *)
let test_move_ship name board move_function ship x y output =
  name >:: fun _ ->
  let new_b = Board.move_ship board ship move_function x y in
  assert_equal output
    (string_tuple_list (Board.get_board new_b (Board.get_width board)))
    ~printer:Fun.id

(**[test_response name board x y output] constructs an OUnit test named [name]
   that asserts the quality of [output] with [repsonse board x y]. *)
let test_response name board x y output =
  name >:: fun _ ->
  assert_equal output (Board.response board x y) ~printer:string_of_bool

(**[test_is_lost name board output] constructs an OUnit test named [name] that
   asserts the quality of [output] with [is_lost board]. *)
let test_is_lost name board output =
  name >:: fun _ ->
  assert_equal output (Board.is_lost board) ~printer:string_of_bool

(**[test_update_outer name board output] constructs an OUnit test named [name]
   that asserts the quality of [output] with the string version of the board
   [update_outer_board board]. *)
let test_update_outer name board output =
  name >:: fun _ ->
  assert_equal output
    (Board.get_board (Board.update_outer_board board) (Board.get_width board)
    |> string_tuple_list)
    ~printer:Fun.id

let board_blackbox_tests =
  [
    test_make_empty "empty" basic
      "{∙: none, ∙: none, ∙: none}{∙: none, ∙: none, ∙: none}{∙: none, ∙: \
       none, ∙: none}";
    test_get_height "complex board height = 16" complex 16;
    test_get_height "basic board height = 3" basic 3;
    test_get_width "complex board width = 16" complex 16;
    test_get_width "basic board width = 3" basic 3;
    test_get_board "basic board" basic
      "{∙: none, ∙: none, ∙: none}{∙: none, S: Submarine_1, ∙: none}{∙: none, \
       ∙: none, ∙: none}";
    test_get_board "two_ship board" two_ship
      "{∙: none, ∙: none, S: Aircraft_Carrier_1}{∙: none, ∙: none, ∙: none}{S: \
       Submarine_1, ∙: none, ∙: none}";
    test_update "attempt hit at (0,1) on basic board" basic 0 1
      "{∙: none, ∙: none, ∙: none}{M: none, S: Submarine_1, ∙: none}{∙: none, \
       ∙: none, ∙: none}";
    test_update "attempt hit at (1,1) on basic board" basic 1 1
      "{M: none, M: none, M: none}{M: none, X: Submarine_1, M: none}{M: none, \
       M: none, M: none}";
    ( "hit out of bounds" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> Board.update basic 4 4) );
    ( "redunant hit" >:: fun _ ->
      assert_raises Board.RedundantHit (fun () ->
          Board.update (Board.update basic 0 0) 0 0) );
    test_move_ship "move Submarine_1 up 1 right 1" basic Ship.place
      "Submarine_1" 1 1
      "{∙: none, ∙: none, S: Submarine_1}{∙: none, ∙: none, ∙: none}{∙: none, \
       ∙: none, ∙: none}";
    ( "move out of bounds" >:: fun _ ->
      assert_raises OutOfBounds (fun () ->
          Board.move_ship basic "Submarine_1" Ship.place 2 4) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship two_ship "Submarine_1" Ship.place 2 2) );
    ( "ship not found" >:: fun _ ->
      assert_raises Board.ShipNotFound (fun () ->
          Board.move_ship two_ship "Sub" Ship.place 2 2) );
    test_response "basic board (1,1) is Full" basic 1 1 true;
    test_response "basic board (1,1) is Hit" (Board.update basic 1 1) 1 1 true;
    test_response "basic board (1,0) is Empty" basic 1 0 false;
    test_response "basic board (1,0) is Miss" (Board.update basic 1 0) 1 0 false;
    test_is_lost "no ships are sunk" two_ship false;
    test_is_lost "only one ship is sunk" (Board.update two_ship 0 0) false;
    test_is_lost "all ships are sunk"
      (Board.update (Board.update two_ship 0 0) 2 2)
      true;
    test_update_outer "outer board doesnt change yet" two_ship
      "{∙: none, ∙: none, ∙: none}{∙: none, ∙: none, ∙: none}{∙: none, ∙: \
       none, ∙: none}";
    test_update_outer "update outer after missing"
      (Board.update two_ship 1 1)
      "{∙: none, ∙: none, ∙: none}{∙: none, M: none, ∙: none}{∙: none, ∙: \
       none, ∙: none}";
    test_update_outer "update outer after hitting a ship but not sinking it"
      (Board.update three_ship 2 2)
      "{∙: none, ∙: none, ∙: none, ∙: none}{∙: none, ∙: none, X: none, ∙: \
       none}{∙: none, ∙: none, ∙: none, ∙: none}{∙: none, ∙: none, ∙: none, ∙: \
       none}";
    test_update_outer "update outer after sinking a ship"
      (Board.update two_ship 0 0)
      "{∙: none, ∙: none, ∙: none}{M: none, M: none, ∙: none}{X: none, M: \
       none, ∙: none}";
  ]

let board_glassbox_tests =
  [
    test_move_ship "move Aircraft_Carrier_1 down 1 left 1" two_ship Ship.place
      "Aircraft_Carrier_1" (-1) 0
      "{∙: none, S: Aircraft_Carrier_1, ∙: none}{∙: none, ∙: none, ∙: none}{S: \
       Submarine_1, ∙: none, ∙: none}";
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 1 0) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 2 0) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 3 0) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 3 1) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 3 2) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 3 3) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 2 3) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 1 3) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 1 2) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 1 1) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 1 3) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 0 2) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship three_ship "Submarine_1" Ship.place 2 1) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship two_sub "Submarine_1" Ship.place 1 2) );
    ( "collision of ships" >:: fun _ ->
      assert_raises Board.Collide (fun () ->
          Board.move_ship two_sub "Submarine_1" Ship.place 3 2) );
    ( "hit out of bounds" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> Board.update basic (-1) 0) );
    ( "hit out of bounds" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> Board.update basic 0 (-1)) );
    ( "hit out of bounds" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> Board.update basic 4 0) );
    ( "hit out of bounds" >:: fun _ ->
      assert_raises OutOfBounds (fun () -> Board.update basic 0 4) );
  ]

open State

(*let init_state_test (name : string) (b1 : Board.b) (b2 : Board.b)
  (expected_output : State.t) = name >:: fun _ -> assert_equal expected_output
  (init_state b1 b2) *)
(* Testing [init_state b1 b2] while testing other functions *)
let s1 = init_state basic basic
let s2 = init_state two_ship three_ship
let s3 = init_state complex complex

let get_inner_test (name : string) (st : State.t) (p : int)
    (expected_output : Board.b) =
  name >:: fun _ -> assert_equal expected_output (get_inner st p)

let get_outer_test (name : string) (st : State.t) (p : int)
    (expected_output : Board.b) =
  name >:: fun _ -> assert_equal expected_output (get_outer st p)

let get_inner_string (name : string) (st : State.t) (p : int)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (string_tuple_list
       (Board.get_board (get_inner st p) (Board.get_width (get_inner st p))))
    ~printer:Fun.id

let get_outer_string (name : string) (st : State.t) (p : int)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (string_tuple_list
       (Board.get_board (get_outer st p) (Board.get_width (get_outer st p))))
    ~printer:Fun.id

let is_hit_test (name : string) (st : State.t) (p : int) (x : int) (y : int)
    (expected : bool) =
  name >:: fun _ -> assert_equal expected (is_hit st p x y)

let state_blackbox_tests =
  [
    get_inner_test "inner board for p1 in state s1 should be basic" s1 1 basic;
    get_inner_test "inner board for p2 in state s1 should be basic" s1 2 basic;
    get_inner_test "inner board for p2 in state s2 should be three_ship" s2 2
      three_ship;
    get_outer_test "outer board for p1 in state s2 with no hits should be empty"
      s2 1
      (Board.make_empty three_ship);
    get_outer_test "outer board for p2 in state s3 with no hits should be empty"
      s3 2 (Board.make_empty complex);
    get_inner_string "move p1's Submarine_1 in state s1 one right, one up"
      (move s1 1 "Submarine_1" 1 1)
      1
      "{∙: none, ∙: none, S: Submarine_1}{∙: none, ∙: none, ∙: none}{∙: none, \
       ∙: none, ∙: none}";
    get_inner_string "move p2's Submarine_2 in state s2 none right, one down"
      (move s2 2 "Submarine_2" 0 (-1))
      2
      "{∙: none, ∙: none, ∙: none, ∙: none}{S: Submarine_2, ∙: none, S: \
       Aircraft_Carrier_1, ∙: none}{∙: none, ∙: none, S: Aircraft_Carrier_1, \
       ∙: none}{S: Submarine_1, ∙: none, ∙: none, ∙: none}";
    get_inner_string
      "move p1's Aircraft_Carrier_1 in state s2 left one, down\n      none"
      (move s2 1 "Aircraft_Carrier_1" (-1) 0)
      1
      "{∙: none, S: Aircraft_Carrier_1, ∙: none}{∙: none, ∙: none, ∙: none}{S: \
       Submarine_1, ∙: none, ∙: none}";
    get_inner_string
      "rotate p1's Submarine_1 in state s1 counterclockwise around (0,1)"
      (rotate s1 1 "Submarine_1" 0 1)
      1
      "{S: Submarine_1, ∙: none, ∙: none}{∙: none, ∙: none, ∙: none}{∙: none, \
       ∙: none, ∙: none}";
    get_inner_string
      "rotate p2's Aircraft_Carrier_1 in state s2 counterclockwise around (2,2)"
      (rotate s2 2 "Aircraft_Carrier_1" 2 2)
      2
      "{S: Submarine_2, ∙: none, ∙: none, ∙: none}{∙: none, ∙: none, S: \
       Aircraft_Carrier_1, S: Aircraft_Carrier_1}{∙: none, ∙: none, ∙: none, \
       ∙: none}{S: Submarine_1, ∙: none, ∙: none, ∙: none}";
    get_outer_string "p1 hit p2's Submarine_1 in state s1 at (1,1)"
      (hit s1 1 1 1) 2
      "{M: none, M: none, M: none}{M: none, X: none, M: none}{M: none, M: \
       none, M: none}";
    get_outer_string "p2 hit p1's Submarine_1 in state s2 at (0, 0)"
      (hit s2 2 0 0) 1
      "{∙: none, ∙: none, ∙: none}{M: none, M: none, ∙: none}{X: none, M: \
       none, ∙: none}";
    get_outer_string "p1 missed p2's Submarine_2 in state s2 at (0, 2)"
      (hit s2 1 0 2) 2
      "{∙: none, ∙: none, ∙: none, ∙: none}{M: none, ∙: none, ∙: none, ∙: \
       none}{∙: none, ∙: none, ∙: none, ∙: none}{∙: none, ∙: none, ∙: none, ∙: \
       none}";
    is_hit_test "(1,1) is hit (true) by p2 in state s1" (hit s1 1 1 1) 2 1 1
      true;
    is_hit_test "p2's (0, 2) is not hit (false) by p1 in state s2"
      (hit s2 1 0 2) 1 0 2 false;
    is_hit_test "p1's (0, 0) is hit (true) by p2 in state s2" (hit s2 2 0 0) 2 0
      0 true;
  ]

let parse_test (name : string) (s : string) (expected : command) =
  name >:: fun _ -> assert_equal expected (parse s)

let parse_empty (name : string) (s : string) =
  name >:: fun _ -> assert_raises Empty (fun () -> parse s)

let parse_malformed (name : string) (s : string) =
  name >:: fun _ -> assert_raises Malformed (fun () -> parse s)

let command_tests =
  [
    parse_empty "an empty command" "";
    parse_empty "command with multiple white spaces" "   ";
    parse_test "command 'quit'" "quit" Quit;
    parse_test "command 'quit'" "   quit    " Quit;
    parse_malformed "malformed 'quit' command" "quit error";
    parse_test "command with 'move' to (1,1) with Submarine_1"
      "move Submarine_1 1 1"
      (Move { name = "Submarine_1"; coordinate = (1, 1) });
    parse_test "command with 'move' to (10, 11) with Cruiser"
      "move Cruiser 10 11"
      (Move { name = "Cruiser"; coordinate = (10, 11) });
    parse_test "command with 'rotate' around (0,0) with Frigate"
      "rotate Frigate 0   0"
      (Rotate { name = "Frigate"; coordinate = (0, 0) });
    parse_test "command with 'hit' to (1, 3) " "    hit  1   3" (Hit (1, 3));
    parse_malformed "malformed 'move' command" "move error";
    parse_malformed "malformed 'Move' command" "Move";
    parse_malformed "malformed 'Quit' command" " Quit ";
    parse_malformed "malformed 'rotate' command" " rotate name 8 ";
    parse_malformed "malformed 'rotate' command" " Rotate name            ";
    parse_malformed "malformed 'hit' command with non-integers" "hit int1 int2";
  ]

let suite =
  "test suite for BattleShip"
  >::: List.flatten
         [
           ship_blackbox_tests;
           ship_glassbox_tests;
           board_blackbox_tests;
           board_glassbox_tests;
           state_blackbox_tests;
           command_tests;
         ]

let _ = run_test_tt_main suite
