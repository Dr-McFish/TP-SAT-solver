open TP


let () = 
  (* Printf.printf "c v,n,ro,proba_sat\n"; *)
  Random.self_init ();
  try
    let v = int_of_string Sys.argv.(1) in 
    let n = int_of_string Sys.argv.(2) in 
    let p = int_of_string Sys.argv.(3) in 
    let instance = Sat.random_instance v n p in
    Sat.print_DIMACS v n instance;
    (* Printf.printf "%s\n" (if Sat.dpll instance then "SATISFIABLE" else "UNSATISFIABLE") *)
  with
  | Invalid_argument _ -> Printf.eprintf "Error. Usage: %s v n p\n" Sys.argv.(0); exit 1
;;