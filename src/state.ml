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

let get_inner (state : t) (player : int) : Board.b =
  if player = 1 then state.p1_inner else state.p2_inner

let get_outer (state : t) (player : int) : Board.b =
  if player = 1 then state.p1_outer else state.p2_outer

type result =
  | Legal of t
  | Illegal

let move state player ship_name x y =
  if player = 1 then
    let b = get_inner state 1 in
    {
      p1_inner = move_ship b ship_name place x y;
      p1_outer = state.p1_outer;
      p2_inner = state.p2_inner;
      p2_outer = state.p2_outer;
    }
  else
    let b = get_inner state 2 in
    {
      p1_inner = state.p1_inner;
      p1_outer = state.p1_outer;
      p2_inner = move_ship b ship_name place x y;
      p2_outer = state.p2_outer;
    }

let rotate state player ship_name x y =
  if player = 1 then
    let b1 = get_inner state 1 in
    {
      p1_inner = move_ship b1 ship_name rotate x y;
      p1_outer = state.p1_outer;
      p2_inner = state.p2_inner;
      p2_outer = state.p2_outer;
    }
  else
    let b2 = get_inner state 2 in
    {
      p1_inner = state.p1_inner;
      p1_outer = state.p1_outer;
      p2_inner = move_ship b2 ship_name rotate x y;
      p2_outer = state.p2_outer;
    }

let hit state player x y =
  if player = 1 then
    {
      p1_inner = state.p1_inner;
      p1_outer = state.p1_outer;
      p2_inner = update state.p2_inner x y;
      p2_outer = update_outer_board state.p2_inner state.p2_outer x y;
    }
  else
    {
      p1_inner = update state.p1_inner x y;
      p1_outer = update_outer_board state.p1_inner state.p1_outer x y;
      p2_inner = state.p2_inner;
      p2_outer = state.p2_outer;
    }
