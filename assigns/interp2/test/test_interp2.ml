open OUnit2
open Interp2

(* ================================================================
   TYPE_OF TESTS
   ================================================================ *)

let type_of_tests =
  let t expected input =
    assert_equal expected (type_of_expr Env.empty (parse_expr input))
  in
  "type_of tests" >:::
  [

    (* ── Literals ── *)
    "unit" >::
    (fun _ -> t (Ok TUnit) "()");

    "bool_true" >::
    (fun _ -> t (Ok TBool) "true");

    "bool_false" >::
    (fun _ -> t (Ok TBool) "false");

    "int_zero" >::
    (fun _ -> t (Ok TInt) "0");

    "int_positive" >::
    (fun _ -> t (Ok TInt) "42");

    "nil" >::
    (fun _ -> t (Ok TInt_list) "[]");

    (* ── Var ── *)
    "var_unbound" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "x") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error for unbound var");

    "var_bound" >::
    (fun _ ->
      let ctx = Env.add "x" TInt Env.empty in
      assert_equal (Ok TInt) (type_of_expr ctx (parse_expr "x")));

    (* ── Negate ── *)
    "negate_int" >::
    (fun _ -> t (Ok TInt) "-(1)");

    "negate_bool_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "-(true)") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: negate bool");

    (* ── Assert ── *)
    "assert_bool" >::
    (fun _ -> t (Ok TUnit) "assert true");

    "assert_false" >::
    (fun _ -> t (Ok TUnit) "assert false");

    "assert_int_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "assert 1") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: assert int");

    (* ── Bop: arithmetic ── *)
    "add" >::
    (fun _ -> t (Ok TInt) "1 + 2");

    "sub" >::
    (fun _ -> t (Ok TInt) "5 - 3");

    "mul" >::
    (fun _ -> t (Ok TInt) "4 * 5");

    "div" >::
    (fun _ -> t (Ok TInt) "8 / 2");

    "mod" >::
    (fun _ -> t (Ok TInt) "7 mod 3");

    "add_bool_lhs_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "true + 1") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error");

    "add_bool_rhs_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "1 + true") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error");

    "mul_bool_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "true * false") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error");

    (* ── Bop: int comparisons ── *)
    "lt_int" >::
    (fun _ -> t (Ok TBool) "1 < 2");

    "lte_int" >::
    (fun _ -> t (Ok TBool) "2 <= 2");

    "gt_int" >::
    (fun _ -> t (Ok TBool) "3 > 1");

    "gte_int" >::
    (fun _ -> t (Ok TBool) "3 >= 3");

    "eq_int" >::
    (fun _ -> t (Ok TBool) "1 = 1");

    "neq_int" >::
    (fun _ -> t (Ok TBool) "1 <> 2");

    (* ── Bop: bool comparisons ── *)
    "eq_bool" >::
    (fun _ -> t (Ok TBool) "true = false");

    "neq_bool" >::
    (fun _ -> t (Ok TBool) "true <> true");

    "eq_mixed_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "1 = true") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: int = bool");

    (* ── Bop: logical ── *)
    "and" >::
    (fun _ -> t (Ok TBool) "true && false");

    "or" >::
    (fun _ -> t (Ok TBool) "false || true");

    "and_int_lhs_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "1 && true") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: int && bool");

    "and_int_rhs_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "true && 0") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: bool && int");

    "or_int_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "false || 1") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error");

    (* ── Bop: Cons ── *)
    "cons_nil" >::
    (fun _ -> t (Ok TInt_list) "1 :: []");

    "cons_list" >::
    (fun _ -> t (Ok TInt_list) "1 :: 2 :: []");

    "cons_bad_lhs_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "true :: []") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: bool :: int list");

    "cons_bad_rhs_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "1 :: 2") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: int :: int");

    (* ── If ── *)
    "if_int" >::
    (fun _ -> t (Ok TInt) "if true then 1 else 2");

    "if_bool" >::
    (fun _ -> t (Ok TBool) "if false then true else false");

    "if_unit" >::
    (fun _ -> t (Ok TUnit) "if true then () else ()");

    "if_list" >::
    (fun _ -> t (Ok TInt_list) "if true then [] else 1 :: []");

    "if_cond_not_bool_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "if 1 then 2 else 3") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: int condition");

    "if_branch_mismatch_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "if true then 1 else false") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: branch type mismatch");

    "if_nested" >::
    (fun _ -> t (Ok TInt) "if true then (if false then 1 else 2) else 3");

    (* ── Fun ── *)
    "fun_int_to_int" >::
    (fun _ -> t (Ok (TFun (TInt, TInt))) "fun (x : int) -> x");

    "fun_bool_to_bool" >::
    (fun _ -> t (Ok (TFun (TBool, TBool))) "fun (x : bool) -> x");

    "fun_int_to_bool" >::
    (fun _ -> t (Ok (TFun (TInt, TBool))) "fun (x : int) -> x > 0");

    "fun_two_args" >::
    (fun _ -> t (Ok (TFun (TInt, TFun (TInt, TInt)))) "fun (x : int) (y : int) -> x + y");

    "fun_returns_fun" >::
    (fun _ -> t
        (Ok (TFun (TInt, TFun (TInt, TInt))))
        "fun (x : int) -> fun (y : int) -> x + y");

    "fun_body_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "fun (x : int) -> x && true") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: int used as bool in body");

    (* ── App ── *)
    "app_identity" >::
    (fun _ -> t (Ok TInt) "(fun (x : int) -> x) 5");

    "app_two_args" >::
    (fun _ -> t (Ok TInt) "(fun (x : int) (y : int) -> x + y) 1 2");

    "app_wrong_arg_type_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "(fun (x : int) -> x) true") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: bool passed to int param");

    "app_not_a_function_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "1 2") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: int is not a function");

    "app_too_many_args_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "(fun (x : int) -> x) 1 2") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: too many args");

    (* ── Tuple ── *)
    "tuple_int_bool" >::
    (fun _ -> t (Ok (TTuple [TInt; TBool])) "(1, true)");

    "tuple_three" >::
    (fun _ -> t (Ok (TTuple [TInt; TBool; TInt_list])) "(1, true, [])");

    "tuple_nested" >::
    (fun _ -> t (Ok (TTuple [TTuple [TInt; TInt]; TBool])) "((1, 2), true)");

    "tuple_err_in_element" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "(1 + true, 2)") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: bad tuple element");

    (* ── Let (plain) ── *)
    "let_int" >::
    (fun _ -> t (Ok TInt) "let x = 5 in x");

    "let_bool" >::
    (fun _ -> t (Ok TBool) "let b = true in b");

    "let_shadow" >::
    (fun _ -> t (Ok TBool) "let x = 5 in let x = true in x");

    "let_use_binding" >::
    (fun _ -> t (Ok TInt) "let x = 3 in x + 1");

    "let_binding_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "let x = 1 + true in x") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error in let binding");

    (* ── Let fun (no annotation) ── *)
    "letfun_no_annot" >::
    (fun _ -> t (Ok TInt) "let f (x : int) = x + 1 in f 3");

    "letfun_two_args" >::
    (fun _ -> t (Ok TInt) "let add (x : int) (y : int) = x + y in add 1 2");

    "letfun_returns_bool" >::
    (fun _ -> t (Ok TBool) "let pos (x : int) = x > 0 in pos 5");

    "letrec_no_args_err" >::
(fun _ ->
  match type_of_expr Env.empty (parse_expr "let rec x = 5 in x") with
  | Error _ -> ()
  | Ok _ -> assert_failure "expected error: rec with no args");

    (* ── Let fun (with annotation) ── *)
    "letfun_annot_int" >::
    (fun _ -> t (Ok TInt) "let f (x : int) : int = x in f 0");

    "letfun_annot_mismatch_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "let f (x : int) : bool = x in f 0") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: annot mismatch");

    (* ── Let rec ── *)
    "letrec_factorial" >::
    (fun _ -> t (Ok TInt)
        "let rec fact (n : int) : int = if n = 0 then 1 else n * fact (n - 1) in fact 5");

    "letrec_sum" >::
    (fun _ -> t (Ok TInt)
        "let rec sum (n : int) : int = if n = 0 then 0 else n + sum (n - 1) in sum 10");

    "letrec_missing_annot_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "let rec f (x : int) = x in f 0") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: rec without annot");

    "letrec_wrong_annot_err" >::
    (fun _ ->
      match type_of_expr Env.empty
          (parse_expr "let rec f (n : int) : bool = if n = 0 then 1 else f (n-1) in f 0")
      with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: rec body doesn't match annot");

    (* ── Match ── *)
    "match_nil_cons" >::
    (fun _ -> t (Ok TInt)
        "match [] with | [] -> 0 | x :: xs -> x");

    "match_bool" >::
    (fun _ -> t (Ok TInt)
        "match true with | true -> 1 | false -> 0");

    "match_int" >::
    (fun _ -> t (Ok TBool)
        "match 0 with | 0 -> true | _ -> false");

    "match_tuple" >::
    (fun _ -> t (Ok TInt)
        "match (1, 2) with | (x, y) -> x + y");

    "match_branch_mismatch_err" >::
    (fun _ ->
      match type_of_expr Env.empty
          (parse_expr "match true with | true -> 1 | false -> false")
      with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: branch type mismatch in match");

    "match_nested_cons" >::
    (fun _ -> t (Ok TInt)
        "match 1 :: 2 :: [] with | [] -> 0 | x :: _ -> x");

    (* ── Match errors ── *)
    "match_unit_pat_on_int_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "match 1 with | () -> 1") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: unit pattern on int scrutinee");

    "match_bool_pat_on_int_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "match 1 with | true -> 1") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: bool pattern on int scrutinee");

    "match_nil_pat_on_int_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "match 1 with | [] -> 1") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: list pattern on int scrutinee");

    "match_cons_pat_bound_several_times_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "match [] with | x :: x -> x") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: x bound several times in cons pattern");

    "match_tuple_pat_bound_several_times_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "match (1,2) with | (a,a) -> a") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: a bound several times in tuple pattern");

    "match_diff_tuple_arity_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "match (1,2,3) with | (a,b) -> a") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: tuple arity mismatch");

    "match_tuple_pat_on_int_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "match 1 with | (a,b) -> a") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: tuple pattern on int scrutinee");

    "match_branch_body_mismatch_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "match 1 with | 0 -> true | x -> x") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: int body but bool expected");

    "match_body_subexpr_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "match 1 with | x -> x + true") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: bool used as int in body");

    "match_second_branch_subexpr_err" >::
    (fun _ ->
      match type_of_expr Env.empty (parse_expr "match 1 with | 0 -> 1 | x -> x + true") with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected error: bool used as int in second branch body");

    (* ── Complex / integration ── *)
    "higher_order_fun" >::
    (fun _ -> t (Ok TInt)
        "let apply (f : int -> int) (x : int) = f x in apply (fun (y : int) -> y + 1) 3");

    "curried_add" >::
    (fun _ -> t (Ok (TFun (TInt, TInt)))
        "let add (x : int) (y : int) = x + y in add 1");

    "let_fun_in_if" >::
    (fun _ -> t (Ok TInt)
        "let f (x : int) = x * 2 in if true then f 3 else f 4");

    "nested_let" >::
    (fun _ -> t (Ok TInt)
        "let x = 1 in let y = x + 1 in let z = y + 1 in z");

  ]


(* ================================================================
   EVAL TESTS
   ================================================================ *)
(* 
let eval_tests =
  let t expected input =
    assert_equal expected (eval_expr Env.empty (parse_expr input))
  in

  "eval tests" >:::
  [

    (* ── Literals ── *)
    "unit" >::
    (fun _ -> t VUnit "()");

    "bool_true" >::
    (fun _ -> t (VBool true) "true");

    "bool_false" >::
    (fun _ -> t (VBool false) "false");

    "int_zero" >::
    (fun _ -> t (VInt 0) "0");

    "int_pos" >::
    (fun _ -> t (VInt 42) "42");

    "nil" >::
    (fun _ -> t (VInt_list []) "[]");

    (* ── Negate ── *)
    "negate_pos" >::
    (fun _ -> t (VInt (-5)) "-(5)");

    "negate_zero" >::
    (fun _ -> t (VInt 0) "-(0)");

    "negate_neg" >::
    (fun _ -> t (VInt 3) "-(-(3))");

    (* ── Assert ── *)
    "assert_true" >::
    (fun _ -> t VUnit "assert true");

    "assert_false_raises" >::
    (fun _ ->
      match eval_expr Env.empty (parse_expr "assert false") with
      | _ -> assert_failure "expected Assert_fail"
      | exception Assert_fail _ -> ());

    (* ── Arithmetic ── *)
    "add" >::
    (fun _ -> t (VInt 5) "2 + 3");

    "sub" >::
    (fun _ -> t (VInt 1) "4 - 3");

    "mul" >::
    (fun _ -> t (VInt 12) "3 * 4");

    "div" >::
    (fun _ -> t (VInt 3) "9 / 3");

    "div_truncates" >::
    (fun _ -> t (VInt 2) "5 / 2");

    "mod" >::
    (fun _ -> t (VInt 1) "7 mod 3");

    "div_by_zero_raises" >::
    (fun _ ->
      match eval_expr Env.empty (parse_expr "1 / 0") with
      | _ -> assert_failure "expected Div_by_zero"
      | exception Div_by_zero _ -> ());

    "mod_by_zero_raises" >::
    (fun _ ->
      match eval_expr Env.empty (parse_expr "5 mod 0") with
      | _ -> assert_failure "expected Div_by_zero"
      | exception Div_by_zero _ -> ());

    (* ── Int comparisons ── *)
    "lt_true" >::
    (fun _ -> t (VBool true) "1 < 2");

    "lt_false" >::
    (fun _ -> t (VBool false) "2 < 1");

    "lte_eq" >::
    (fun _ -> t (VBool true) "2 <= 2");

    "gt_true" >::
    (fun _ -> t (VBool true) "3 > 2");

    "gte_true" >::
    (fun _ -> t (VBool true) "3 >= 3");

    "eq_true" >::
    (fun _ -> t (VBool true) "5 = 5");

    "eq_false" >::
    (fun _ -> t (VBool false) "5 = 6");

    "neq_true" >::
    (fun _ -> t (VBool true) "1 <> 2");

    "neq_false" >::
    (fun _ -> t (VBool false) "3 <> 3");

    (* ── Bool comparisons ── *)
    "eq_bool_tt" >::
    (fun _ -> t (VBool true) "true = true");

    "eq_bool_ff" >::
    (fun _ -> t (VBool true) "false = false");

    "eq_bool_tf" >::
    (fun _ -> t (VBool false) "true = false");

    "neq_bool" >::
    (fun _ -> t (VBool true) "true <> false");

    (* ── Logical (short-circuit) ── *)
    "and_tt" >::
    (fun _ -> t (VBool true) "true && true");

    "and_tf" >::
    (fun _ -> t (VBool false) "true && false");

    "and_short_circuit" >::
    (fun _ -> t (VBool false) "false && (1 / 0 = 0)");

    "or_ff" >::
    (fun _ -> t (VBool false) "false || false");

    "or_tf" >::
    (fun _ -> t (VBool true) "false || true");

    "or_short_circuit" >::
    (fun _ -> t (VBool true) "true || (1 / 0 = 0)");

    (* ── Cons ── *)
    "cons_single" >::
    (fun _ -> t (VInt_list [1]) "1 :: []");

    "cons_multiple" >::
    (fun _ -> t (VInt_list [1; 2; 3]) "1 :: 2 :: 3 :: []");

    (* ── If ── *)
    "if_true_branch" >::
    (fun _ -> t (VInt 1) "if true then 1 else 2");

    "if_false_branch" >::
    (fun _ -> t (VInt 2) "if false then 1 else 2");

    "if_only_evals_taken_branch" >::
    (fun _ -> t (VInt 1) "if true then 1 else (1 / 0)");

    "if_nested" >::
    (fun _ -> t (VInt 3) "if false then 1 else if false then 2 else 3");

    (* ── Tuple ── *)
    "tuple_pair" >::
    (fun _ -> t (VTuple [VInt 1; VBool true]) "(1, true)");

    "tuple_triple" >::
    (fun _ -> t (VTuple [VInt 1; VInt 2; VInt 3]) "(1, 2, 3)");

    "tuple_nested" >::
    (fun _ -> t (VTuple [VTuple [VInt 1; VInt 2]; VBool false]) "((1, 2), false)");

    (* ── Fun / App ── *)
    "fun_identity" >::
    (fun _ -> t (VInt 7) "(fun (x : int) -> x) 7");

    "fun_add_one" >::
    (fun _ -> t (VInt 6) "(fun (x : int) -> x + 1) 5");

    "fun_two_args" >::
    (fun _ -> t (VInt 5) "(fun (x : int) (y : int) -> x + y) 2 3");

    "fun_captures_env" >::
    (fun _ -> t (VInt 2) "let x = 2 in (fun (y : bool) -> x) true");

    "fun_higher_order" >::
    (fun _ -> t (VInt 4)
        "(fun (f : int -> int) -> f 3) (fun (x : int) -> x + 1)");

    "fun_curried" >::
    (fun _ -> t (VInt 7)
        "let add (x : int) (y : int) = x + y in let add3 = add 3 in add3 4");

    (* ── Let ── *)
    "let_simple" >::
    (fun _ -> t (VInt 5) "let x = 5 in x");

    "let_arith" >::
    (fun _ -> t (VInt 7) "let x = 3 in x + 4");

    "let_shadow" >::
    (fun _ -> t (VInt 2) "let x = 1 in let x = 2 in x");

    "let_chain" >::
    (fun _ -> t (VInt 6) "let x = 1 in let y = 2 in let z = 3 in x + y + z");

    "let_fun_no_annot" >::
    (fun _ -> t (VInt 4) "let double (x : int) = x * 2 in double 2");

    "let_fun_annot" >::
    (fun _ -> t (VInt 9) "let square (x : int) : int = x * x in square 3");

    "let_fun_two_args" >::
    (fun _ -> t (VInt 5) "let add (x : int) (y : int) = x + y in add 2 3");

    (* ── Let rec ── *)
    "letrec_factorial_0" >::
    (fun _ -> t (VInt 1)
        "let rec fact (n : int) : int = if n = 0 then 1 else n * fact (n - 1) in fact 0");

    "letrec_factorial_5" >::
    (fun _ -> t (VInt 120)
        "let rec fact (n : int) : int = if n = 0 then 1 else n * fact (n - 1) in fact 5");

    "letrec_sum" >::
    (fun _ -> t (VInt 55)
        "let rec sum (n : int) : int = if n = 0 then 0 else n + sum (n - 1) in sum 10");

    "letrec_fibonacci" >::
    (fun _ -> t (VInt 8)
        "let rec fib (n : int) : int = if n <= 1 then n else fib (n-1) + fib (n-2) in fib 6");

    "letrec_countdown" >::
    (fun _ -> t (VInt 0)
        "let rec go (n : int) : int = if n = 0 then 0 else go (n - 1) in go 100");

    (* ── Match ── *)
    "match_nil" >::
    (fun _ -> t (VInt 0) "match [] with | [] -> 0 | x :: _ -> x");

    "match_cons" >::
    (fun _ -> t (VInt 1) "match 1 :: [] with | [] -> 0 | x :: _ -> x");

    "match_bool_true" >::
    (fun _ -> t (VInt 1) "match true with | true -> 1 | false -> 0");

    "match_bool_false" >::
    (fun _ -> t (VInt 0) "match false with | true -> 1 | false -> 0");

    "match_int_wildcard" >::
    (fun _ -> t (VBool false) "match 5 with | 0 -> true | _ -> false");

    "match_int_exact" >::
    (fun _ -> t (VBool true) "match 0 with | 0 -> true | _ -> false");

    "match_tuple_destruct" >::
    (fun _ -> t (VInt 3) "match (1, 2) with | (x, y) -> x + y");

    "match_nested_cons" >::
    (fun _ -> t (VInt 1)
        "match 1 :: 2 :: [] with | [] -> 0 | x :: _ -> x");

    "match_pvar_bind" >::
    (fun _ -> t (VInt 42)
        "match 42 with | x -> x");

    (* ── Integration ── *)
    "apply_higher_order" >::
    (fun _ -> t (VInt 4)
        "let apply (f : int -> int) (x : int) = f x in
         apply (fun (y : int) -> y + 1) 3");

    "compose_functions" >::
    (fun _ -> t (VInt 7)
        "let add1 (x : int) = x + 1 in
         let double (x : int) = x * 2 in
         add1 (double 3)");

    "letrec_with_accumulator" >::
    (fun _ -> t (VInt 10)
        "let rec loop (n : int) (acc : int) : int =
           if n = 0 then acc else loop (n - 1) (acc + 1)
         in loop 10 0");

    "assert_in_let" >::
    (fun _ -> t (VInt 1)
        "let x = 5 in let _ = assert (x > 0) in 1");

  ] *)


(* ================================================================
   SUITE
   ================================================================ *)

let suite =
  "interp2 test suite" >:::
  [
    type_of_tests;
    (* eval_tests; *)
  ]

let _run = run_test_tt_main suite
