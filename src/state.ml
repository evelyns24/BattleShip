(* Note: You may introduce new code anywhere in this file. *)
open Ship

type t = {
  current : string;
  visited : string list;
}

let init_state ship1 ship2 = raise (Failure "Unimplemented init_state")

type result =
  | Legal of t
  | Illegal

(** [refine_list elt lst] adds elt to lst if elt is not already in lst,
    otherwise returns lst*)
let rec refine_list elt lst = if List.mem elt lst then lst else elt :: lst

let go ex adv st = raise (Failure "Unimplemented go")
