type litteral = X of int | NonX of int
type fnc = litteral list list

(* [suduko_enc dig x y] *)
val suduko_enc : int -> int -> int -> int

val fnc_exmple1 : fnc

val simplifie_antagonistes : fnc -> fnc

val nouveau_lit_isole : fnc -> litteral option
val simplification : litteral -> litteral list list -> litteral list list
val propagation : fnc -> fnc

val variables : fnc -> int list

val test_conflict : fnc -> bool

val deduction : int -> litteral list list -> litteral option
val propagation_infructuex : litteral list list -> litteral list list

(** [read_DIMACS_cnf_file filename] *)
val read_DIMACS_cnf_file : string -> fnc
val print_DIMACS : int -> int -> fnc -> unit

(* DPLL sat solver *)
val dpll : fnc -> bool

val random_literal :int -> litteral
val random_clause : int -> int -> litteral list
val random_instance : int -> int -> int -> fnc 

(** [proportion_npsat v n p]*)
val proportion_npsat : int -> int -> int -> float

(** [proprion_sat_pro v p ro]*)
val proprion_sat_pro : int -> int -> float -> float

