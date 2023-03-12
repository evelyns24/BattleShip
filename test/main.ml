open OUnit2
open Game
open Ship
open Command

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

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

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests. You can also use the JSON files in the data directory
   as tests. And you can add JSON files in this directory and use them, too. *)

(* Here is an example of how to load files from the data directory: *)
let data_dir_prefix = "data" ^ Filename.dir_sep

(*let lonely = Yojson.Basic.from_file (data_dir_prefix ^ "lonely_room.json") let
  ho = Yojson.Basic.from_file (data_dir_prefix ^ "ho_plaza.json")*)

let basic_ship = make [ (1, 1) ]
let two_long = make [ (3, 1); (3, 2) ]
let three_long = make [ (2, 2); (2, 3); (2, 1) ]
let four_long = make [ (6, 4); (3, 4); (4, 4); (5, 4) ]
let l_shaped = make [ (2, 4); (2, 5); (3, 5); (4, 5) ]

(**[test_location name ship output] builds an OUnit test named [name] that tests
   whether or not [location ship] returns the expected output [output]*)
let test_location (name : string) (ship : t) (output : (int * int) list) =
  name >:: fun _ ->
  assert_equal output (location ship) ~cmp:cmp_set_like_lists
    ~printer:(pp_list pp_coord)

(**[test_rotate name ship point output] builds an OUnit test named [name] that
   tests whether or not [rotate point ship] rotates the ship correctly by
   comparing the location after the rotatation with the expected output [output]*)
let test_rotate (name : string) (ship : t) (point : int * int)
    (output : (int * int) list) =
  name >:: fun _ ->
  assert_equal output
    (rotate point ship |> location)
    ~cmp:cmp_set_like_lists ~printer:(pp_list pp_coord)

let ship_blackbox_tests =
  [
    test_location "loc basic" basic_ship [ (1, 1) ];
    test_location "loc two_long" two_long [ (3, 1); (3, 2) ];
    test_location "loc four_long" four_long [ (6, 4); (3, 4); (4, 4); (5, 4) ];
  ]

let glass_rotate_test =
  [
    test_rotate "rotate basic 1x" basic_ship (1, 1) [ (1, 1) ];
    test_rotate "rotate basic 2x" (rotate (1, 1) basic_ship) (1, 1) [ (1, 1) ];
    test_rotate "rotate two 1x" two_long (3, 1) [ (3, 1); (2, 1) ];
    test_rotate "rotate two 3x"
      (rotate (3, 1) (rotate (3, 1) two_long))
      (3, 1)
      [ (3, 1); (4, 1) ];
    test_rotate "rotate L 1x" l_shaped (3, 5) [ (3, 6); (3, 5); (3, 4); (4, 4) ];
    test_rotate "rotate L 4x"
      (rotate (3, 5) (rotate (3, 5) (rotate (3, 5) l_shaped)))
      (3, 5)
      [ (2, 4); (2, 5); (3, 5); (4, 5) ];
    test_rotate "rotate three 2 pts"
      (rotate (2, 3) three_long)
      (4, 3)
      [ (4, 3); (4, 2); (4, 1) ];
  ]

let ship_glassbox_tests = List.flatten [ glass_rotate_test ]

let suite =
  "test suite for A2"
  >::: List.flatten [ ship_blackbox_tests; ship_glassbox_tests ]

let _ = run_test_tt_main suite
