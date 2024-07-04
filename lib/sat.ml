(*
   n - nombre de clauses
   p - nombre de varibles par clause
   v - nombre de variable totale
*)

type litteral =
  | X of int
  | NonX of int
;;

type fnc = litteral list list
;;

let suduko_enc dig x y =
  dig + 9* (x + 9*y)
;;

let fnc_exmple1 = [
  [NonX(suduko_enc 3 0 2)];
  [NonX(suduko_enc 3 0 2); X(suduko_enc 6 2 5)];
  [X(suduko_enc 7 1 4); X(suduko_enc 3 0 2)]
]
;;
(*
let rec suprimer_cl clause lit =
  match clause with
  | [] -> []
  | head:: tail when head = lit -> suprimer_cl tail lit
  | head:: tail -> head :: suprimer_cl tail lit
;; *)

(* let not_l lit =
  match lit with
  | NonX(i) -> X(i)
  | X(i) -> NonX(i) *)
;;
let get_lit_type lit =
  match lit with
  | NonX(_) -> false
  | X(_) -> true
;;

let get_lit_num lit =
  match lit with
  | NonX(i) | X(i) -> i
;;

exception Tiers_exclue

let simplifie_antagonistes formule_fnc =
  let prosses_clause clause=
    let set = Hashtbl.create 64 in
    let rec aux clause =
      match clause with
      | [] -> []
      | head :: tail ->
        match Hashtbl.find_opt set (get_lit_num head) with
        | None ->
          Hashtbl.add set (get_lit_num head) (get_lit_type head);
          head :: (aux tail)
        | Some(b) ->
          if b = (get_lit_type head) then
            aux tail (* tail *)
          else
            raise Tiers_exclue
    in
    aux clause
  in
  let rec simplify_formula fnc =
    match fnc with
    | [] -> []
    | head :: tail ->
      try
        (prosses_clause head) :: (simplify_formula tail)
      with
      | Tiers_exclue -> (simplify_formula tail)
    in
    simplify_formula formule_fnc
;;


let rec nouveau_lit_isole formule_fnc = (* O(n) *)
  match formule_fnc with
  | [] -> None
  | [isole] :: _ -> Some(isole)
  | _ :: tail -> nouveau_lit_isole tail
;;

exception ClauseSatisfait
let simplification lit formule_fnc = (* O(n*p) *)
  let rec simplifier_clause clause = (* O(p) *)
    match clause with
    | [] -> []
    | head :: _ when head = lit -> raise ClauseSatisfait (*suprimer literal *)
    | head :: tail when (get_lit_num head) = (get_lit_num lit) ->  simplifier_clause tail (*suprimer clause*)
    | head :: tail -> head :: (simplifier_clause tail) (*suprimer  rien*)
  in
  let rec simplifier_formule fnc = (* O(n*p) *)
    match fnc with
    | [] -> []
    | clause :: tail -> 
      try 
        let clause_simple = simplifier_clause clause in
        clause_simple :: (simplifier_formule tail)
      with
      | ClauseSatisfait -> (simplifier_formule tail)
    in
    simplifier_formule formule_fnc
;;

let rec propagation formule_fnc = (* O(vnp) (c'est polynomiale) *)
  match nouveau_lit_isole formule_fnc with (* O(n) *)
  | None -> formule_fnc
  | Some(lit) -> (* Printf.eprintf "X \n%d" (get_lit_num lit); *)
    simplification lit formule_fnc (* O(np) *)
    |> propagation
    (* il peut avoir au plus v literaux isolés car chaque fois il est suprimé *)
;;

let variables formule_fnc =
  let set = Hashtbl.create 42 in
  let rec lit_to_var_suprimer_doublons list =
    match list with 
    | NonX(i) :: rest | X(i) :: rest when Hashtbl.mem set i -> lit_to_var_suprimer_doublons rest
    | NonX(i) :: rest | X(i) :: rest -> Hashtbl.add set i () ; 
      i :: (lit_to_var_suprimer_doublons rest)
    | [] -> []
  in
  formule_fnc
  |> List.flatten
  |> lit_to_var_suprimer_doublons
;;

(* On peut ptere tester ca directement dans [simplification] *)
let test_conflict : fnc -> bool = List.mem []

let deduction var formule_fnc =
  if test_conflict (propagation ([NonX(var)] :: formule_fnc)) then (* l'ordre est important *)
    Some(X(var))
  else if test_conflict (propagation ([X(var)] :: formule_fnc)) then 
    Some(NonX(var))
  else
    None
;;

let rec propagation_infructuex formule_fnc =
  let rec trouver_infructuex variables formule_fnc = (* O(n)*)
    match variables with
    | [] -> None
    | i :: tail ->
       begin
        match deduction i formule_fnc with
        | Some(lit) -> Some(lit)
        | None -> trouver_infructuex tail formule_fnc
       end
  in
  match nouveau_lit_isole formule_fnc with
  | Some(lit) ->
    simplification lit formule_fnc (* O(np) *)
    |> propagation_infructuex
  | None ->
    begin
      (* c'est pas claire si recalculer les varibles c'est bien ou mal
          + on test que les variables qui sont dedans
          - on recale a chaque etape *)
      let variables = variables formule_fnc in
      match trouver_infructuex variables formule_fnc with
      | Some(lit) -> propagation_infructuex ([lit] :: formule_fnc) (* c'est bien car le literal isole est trouve tout de suite*)
      | None -> formule_fnc (* on peut plus simplifier *)
    end
;;

let dpll formule_fnc =
  let rec dfs formule_fnc =
    let formule_simplifie = propagation_infructuex formule_fnc in
    if [] = formule_simplifie then
      true
    else if test_conflict formule_simplifie then
      false
    else
      variables formule_simplifie
      |> List.fold_left (fun acc var ->
        acc
        || dfs ([NonX var] :: formule_simplifie) (* On essaie de supposer Xvar*)
        || dfs ([X var] :: formule_simplifie) (* On essaie de supposer Xvar*)
        ) false                               (* si ni x1 ni nonX1 marche, alors on essai les aures variables*)
  in
  let simple_fnc = simplifie_antagonistes formule_fnc in
  dfs simple_fnc
;;


let read_DIMACS_cnf_file filename =
  let file = In_channel.open_text filename in
  let (_n_variables, n_clauses) =
    let rec get_problem () =
      let line = input_line file in
      if String.starts_with ~prefix:"c" line then
        get_problem ()
      else
        (Scanf.sscanf line "p cnf %d %d" (fun _n_variables n_clauses -> (_n_variables,n_clauses)))
    in
    get_problem ()
  in
  let line_to_clause line =
    let convert_lieral lit =
      let i = Scanf.sscanf lit "%d" (fun i -> i) in
      if i < 0 then
        Some(NonX(-i))
      else if i > 0 then
        Some(X(i))
      else
        None
    in
    String.split_on_char ' ' line
    |> List.filter_map convert_lieral
  in
  List.init n_clauses (fun _ -> input_line file)
  |> List.map line_to_clause
;;

let print_DIMACS v n fnc =
  Printf.printf "p cnf %d %d\n" v n;
  let rec print_clause clause =
    match clause with
    | X(i) :: tail -> Printf.printf "%d " i; print_clause tail
    | NonX(i) :: tail -> Printf.printf "%d " (-i); print_clause tail
    | [] -> Printf.printf "0\n"
  in
  List.iter print_clause fnc
;;


(* 6 trasistion de phase *)
(* N -> v *)

let random_literal v =
  let i = (1+Random.int v) in
  if Random.bool () then
    X(i)
  else
    NonX(i)
;;

let random_clause v p =
  List.init p (fun _ -> random_literal v)
;;

let random_instance v n p =
  List.init n (fun _ -> random_clause v p)
;;

let nombre_raisonable = 50;;
let proportion_npsat v n p  =
  let instances = List.init nombre_raisonable (fun _ -> random_instance v n p) in
  let results = List.map dpll instances in
  let count_sats = List.fold_left (fun acc satisfibilite -> acc + if satisfibilite then 1 else 0) 0 results in
  (float_of_int count_sats) /. (float_of_int nombre_raisonable)
;;

let proprion_sat_pro v p ro =
  let n = (int_of_float (ro *. float_of_int p)) in
  proportion_npsat v n p
;;

