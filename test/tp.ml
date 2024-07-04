open TP

let () =
  assert(B.answer = 42);
  assert(A.message.[0] = 'F')

;;


open Sat
let () =
  let example_antagoniste = [
    [X(0); X(1); X(0)];
    [X(0); X(2); NonX(1); X(2); X(0)];
    [X(3); NonX(3)];
    [NonX(0); X(0)]
  ] in

  let example_antagoniste_attendue = [
    [X(0); X(1)];
    [X(0); X(2); NonX(1)]

  ] in

  assert (example_antagoniste_attendue = simplifie_antagonistes example_antagoniste);
;;

let propag_example = [
  [X(suduko_enc 1 0 0)];
  [X(suduko_enc 4 2 2); X(suduko_enc 6 3 6); X(suduko_enc 7 7 7)];
  [NonX(suduko_enc 1 0 0); NonX(suduko_enc 6 3 6)]
];;

let propag_example_solution = [[X(suduko_enc 4 2 2); X(suduko_enc 7 7 7)]]

let () =
  assert (Some (X(suduko_enc 1 0 0)) = (nouveau_lit_isole propag_example));
  assert ((propag_example_solution) = (propagation propag_example))
;;

(* test variables *)
let () =
  let fnc = [
    [X(0); X(1); X(0); X(4) ; X(7)];
    [X(0); X(2); NonX(2); NonX(4); X(100)];
    [X(2) ; NonX(101)];
  ] in
  let expected_vars = [0; 1; 2; 4; 7; 100; 101] in
  assert (expected_vars = List.sort compare (Sat.variables fnc))
;;

let example_deduction = [
  [X(0); X(1)];
  [NonX(0); X(1)]; (* si on met X(1) a faux, alors on X1 et NonX1. donc X1 *)
  [X(2); NonX(0); NonX(1)]
];;

let () = (* test deduction *)
  assert (Some(X(1)) = deduction 1 example_deduction)
;;

let test_files = [
  ("false.cnf", false);
  ("true.cnf", true);
  ("unit1.cnf", true);
  ("unit2.cnf", true);
  ("unit3.cnf", true);
  ("unit4.cnf", true);
  ("unit5.cnf", false);
  ("unit6.cnf", false);
  ("unit7.cnf", true);
  ("unit8.cnf", false);
  ("unit9.cnf", false);
  ("full2.cnf", false);
  ("full3.cnf", false);
  ("full4.cnf", false);
]
;;

let root_dir = ".."

(* Test DPLL *)
let () =
  List.iter (fun (test_file, result) -> Printf.printf "%s\n" test_file; assert (result = dpll (read_DIMACS_cnf_file (root_dir ^ "/sample_cnf/" ^ test_file))))
  test_files
;;

let () = 
  Printf.printf "Tests Compleated!\n"
;;
