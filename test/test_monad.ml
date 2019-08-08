open Batteries
open Jhupllib
open OUnit2

let test_monad _ = 
  let open Eager_nondeterminism.Nondeterminism_monad
  in let m_result = 
       pick_enum @@ List.enum [1]
  in let b_result = bind m_result (fun x -> pick_enum @@ List.enum [x;x])
  in let lst_result = List.of_enum @@ enum b_result
  in assert_equal lst_result [1;1]

(* let test_sequence _ =
   let open Eager_nondeterminism.Nondeterminism_monad
   in let m_result = 
       pick_enum @@ List.enum [1]
   in let list_m = [m_result; m_result]
   in let seq_result = sequence @@ List.enum list_m
   in let r_result = 
   in let lst_result = List.of_enum @@ enum seq_result
   in assert_equal lst_result [1;1] *)

let _tests = [
  "test_monad" >:: test_monad;
  (* "test_sequence" >:: test_sequence *)
]

let tests = "Monads tests" >::: _tests;;