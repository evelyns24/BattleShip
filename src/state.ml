(* Note: You may introduce new code anywhere in this file. *)
open Board
open Ship

type t = {
  p1_inner : Board.b;
  p1_outer : Board.b;
  p2_inner : Board.b;
  p2_outer : Board.b;
}

let init_state b1 b2 =
  {
    p1_inner = b1;
    p1_outer = make_empty b2;
    p2_inner = b2;
    p2_outer = make_empty b1;
  }

let get_p1_inner (state : t) : Board.b = state.p1_inner
let get_p1_outer (state : t) : Board.b = state.p1_outer
let get_p2_inner (state : t) : Board.b = state.p2_inner
let get_p2_outer (state : t) : Board.b = state.p2_outer

type result =
  | Legal of t
  | Illegal

let move player ship_name x y = failwith "Unimplemented : Evelyn's problem"
let rotate player ship_name x = failwith "Unimplemented : Alisha's problem"
let hit player x y = failwith "Unimplemented : Gloria's problem"
