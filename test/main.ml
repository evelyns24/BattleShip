open OUnit2
open Game
open Ship
open Command
open State

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
let pp_string s = "\"" ^ s ^ "\""

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
let dummy_test name output = name >:: fun _ -> assert_equal output output
let ship_blackbox_tests = []
let ship_glassbox_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ ship_blackbox_tests; ship_glassbox_tests ]

let _ = run_test_tt_main suite
