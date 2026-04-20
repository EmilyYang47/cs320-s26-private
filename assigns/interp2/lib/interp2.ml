open Utils
module Error_msg = Error_msg

(* SYNTAX
   ----------------------------------------------------------------------
*)

type ty = Ast.Interp2.ty =
    | TUnit
    | TBool
    | TInt
    | TInt_list
    | TFun of ty * ty
    | TTuple of ty list

let rec pp_ty ppf ty =
  let open Fmt in
  let pp_parens ppf ty =
    match ty with
    | TFun (_, _)
    | TTuple _
    | _ -> pp_ty ppf ty
  in
  match ty with
  | TUnit -> pf ppf "unit"
  | TBool -> pf ppf "bool"
  | TInt -> pf ppf "int"
  | TFun (t1, t2) -> pf ppf "%a -> %a" pp_parens t1 pp_ty t2
  | TTuple ts -> list ~sep:(Fmt.any " * ") pp_ty ppf ts
  | TInt_list -> pf ppf "int list"

type _pattern = Ast.Interp2._pattern =
  | PUnit
  | PBool of bool
  | PInt of int
  | PNil
  | PCons of pattern * pattern
  | PTuple of pattern list
  | PVar of string
and pattern = Ast.Interp2.pattern =
  {
    pos : pos;
    pattern : _pattern;
  }

type bop = Ast.Interp2.bop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or | Cons

type _expr = Ast.Interp2._expr =
  | Unit
  | Bool of bool
  | Int of int
  | Var of string
  | Nil
  | Assert of expr
  | Negate of expr
  | Tuple of expr list
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Fun of (string * ty) list * expr
  | App of expr * expr list
  | Let of
      {
        is_rec : bool;
        name : string;
        args : (string * ty) list;
        annot : ty option;
        binding : expr;
        body : expr;
      }
  | Match of expr * (pattern * expr) list
and expr = Ast.Interp2.expr =
  {
    pos : pos;
    expr : _expr;
  }

type _stmt = Ast.Interp2._stmt =
  | SLet of {
      is_rec : bool;
      name : string;
      args : (string * ty) list;
      annot : ty option;
      binding : expr;
    }
and stmt = Ast.Interp2.stmt =
  {
    pos : pos;
    stmt : _stmt;
  }

type prog = stmt list

module Env = Map.Make(String)

(* TYPE ERRORS
   ----------------------------------------------------------------------
*)

let unknown_var pos x = Error_msg.mk pos (Format.asprintf "Unbound value %s" x)

let exp_ty pos t1 t2 =
  let msg =
    Format.asprintf
      "This expression has type %a but an expression was expected of type %a"
      pp_ty t1 pp_ty t2
  in Error_msg.mk pos msg

let exp_pat pos t1 t2 =
  let msg =
    Format.asprintf
      "This pattern matches values of type %a but a pattern was expected which matches values of type %a"
      pp_ty t1 pp_ty t2
  in Error_msg.mk pos msg

let exp_tuple_pat pos t =
  let msg =
    Format.asprintf
      "This pattern matches values of a tuple type but a pattern was expected which matches values of type %a"
      pp_ty t
  in Error_msg.mk pos msg

let exp_diff_tuple_pat pos ty =
  let msg =
    Format.asprintf
      "This pattern matches values of a tuple type but a pattern was expected which matches values of a different tuple type %a"
      pp_ty ty
  in Error_msg.mk pos msg

let not_func pos ty =
  let msg =
    Format.asprintf
      "This expression has type %a. This is not a function; it cannot be applied"
      pp_ty ty
  in Error_msg.mk pos msg

let too_many_args pos ty =
  let msg =
    Format.asprintf
      "This function has type %a. It is applied to to many arguments"
      pp_ty ty
  in Error_msg.mk pos msg

let missing_rec_annot pos =
  Error_msg.mk pos "Must provide output type annotation for recursive function"

let missing_rec_arg pos =
  Error_msg.mk pos "Must provide argument for recursive function"

let bound_several_times pos x =
  let msg =
    Format.asprintf
      "Variable %s is bound several times in this matching"
      x
  in Error_msg.mk pos msg


(* TYPING
   ----------------------------------------------------------------------
*)

(* Contexts *)

type ctxt = ty Env.t

(* Type Checking *)

let type_of_expr (ctxt : ctxt) (e : expr) : (ty, Error_msg.t) result =
  let rec loop (context : ctxt) (exp : expr) = 
    match exp.expr with 
    | Unit  -> Ok (TUnit : ty)
    | Bool _ -> Ok (TBool : ty)
    | Int _ -> Ok (TInt : ty)
    | Nil   -> Ok (TInt_list : ty) 
    | Var x -> (match Env.find_opt x context with
                | Some t -> Ok t
                | None   -> Error (unknown_var exp.pos x)
              )
    | Let {is_rec; name; args; annot; binding; body} -> if not is_rec && (args = []) 
                                                        (* LET *)
                                                        then (match (loop context binding) with 
                                                          | Ok (t1) -> loop (Env.add name t1 context) body 
                                                          | Error e -> Error e 
                                                        ) 
                                                        else (match annot with 
                                                              (* LETFUNANNOT & LETREC *) 
                                                              | Some out_ty -> let arg_ctxt = List.fold_right (fun (x, t) ctx -> Env.add x t ctx) args context in 
                                                                              let arg_ty = List.fold_right (fun (_, t) acc -> TFun (t, acc)) args out_ty in 
                                                                              let binding_ctxt = if not is_rec then arg_ctxt else Env.add name arg_ty arg_ctxt in 
                                                                              (match loop binding_ctxt binding with 
                                                                              | Ok (t_k_plus_1) -> if t_k_plus_1 <> out_ty then Error (exp_ty binding.pos t_k_plus_1 out_ty) else loop (Env.add name arg_ty context) body
                                                                              | Error e -> Error e)
                                                              (* LETFUN *)
                                                              | None -> if is_rec && args = [] then Error (missing_rec_arg exp.pos)
                                                                        else if is_rec then Error (missing_rec_annot exp.pos)
                                                                        else
                                                                          let arg_ctxt = List.fold_right (fun (x, t) ctx -> Env.add x t ctx) args context in
                                                                          (match loop arg_ctxt binding with
                                                                          | Ok out_ty ->
                                                                              let arg_ty = List.fold_right (fun (_, t) acc -> TFun (t, acc)) args out_ty in
                                                                              loop (Env.add name arg_ty context) body
                                                                          | Error e -> Error e)
                                                              ) 
    | If (e1, e2, e3) -> (match loop context e1 with
                          | Error e -> Error e
                          | Ok TBool ->
                              (match loop context e2, loop context e3 with
                                | Error e, _ -> Error e
                                | _, Error e -> Error e
                                | Ok t2, Ok t3 ->
                                    if t2 = t3 then Ok t2
                                    else Error (exp_ty e3.pos t3 t2))
                          | Ok t -> Error (exp_ty e1.pos t TBool))
    | Fun (args, e) -> let arg_ctxt = List.fold_right (fun (x, t) ctx -> Env.add x t ctx) args context in 
                          (match loop arg_ctxt e with 
                          | Ok (t) -> let out_ty = List.fold_right (fun (_, tk) acc -> TFun (tk, acc)) args t in Ok (out_ty)
                          | Error e -> Error e
                          ) 
    | App (e, e_args) ->
                        (match loop context e with
                        | Error err -> Error err
                        | Ok e_out ->
                            let rec check_arg_type ty args =
                              match ty, args with
                              | t, [] -> Ok t
                              | TFun (t_in, t_out), x :: xs ->
                                  (match loop context x with
                                    | Error err -> Error err
                                    | Ok t_arg ->
                                        if t_arg = t_in then check_arg_type t_out xs
                                        else Error (exp_ty x.pos t_arg t_in))
                              | t, x :: _ -> Error (too_many_args x.pos t)
                            in
                            (match e_out with
                              | TFun _ -> check_arg_type e_out e_args
                              | t -> if e_args = [] then Ok t else Error (not_func e.pos t)))
    | Bop (bop, e1, e2) -> (match loop context e1, loop context e2 with
                            | Error e, _ -> Error e
                            | _, Error e -> Error e
                            | Ok t1, Ok t2 ->
                                            (match bop, t1, t2 with
                                            | (Add | Sub | Mul | Div | Mod), TInt, TInt -> Ok TInt
                                            | (Lt | Lte | Gt | Gte | Eq | Neq), TInt, TInt -> Ok TBool
                                            | (And | Or), TBool, TBool -> Ok TBool
                                            | (Eq | Neq), TBool, TBool -> Ok TBool
                                            | Cons, TInt, TInt_list -> Ok TInt_list
                                            | (Add | Sub | Mul | Div | Mod), t, TInt -> Error (exp_ty e1.pos t TInt)
                                            | (Add | Sub | Mul | Div | Mod), TInt, t -> Error (exp_ty e2.pos t TInt)
                                            | (Add | Sub | Mul | Div | Mod), t, _ -> Error (exp_ty e1.pos t TInt)
                                            | (And | Or), t, TBool -> Error (exp_ty e1.pos t TBool)
                                            | (And | Or), TBool, t -> Error (exp_ty e2.pos t TBool)
                                            | (And | Or), t, _ -> Error (exp_ty e1.pos t TBool)
                                            | Cons, TInt, t -> Error (exp_ty e2.pos t TInt_list)
                                            | Cons, t, _ -> Error (exp_ty e1.pos t TInt)
                                            | (Lt | Lte | Gt | Gte | Eq | Neq), t1, t2 -> if t1 = t2 then Ok TBool else Error (exp_ty e2.pos t2 t1)
                                            )
                            )  
    | Negate (e) -> (match loop context e with 
                    | Ok (TInt) -> Ok (TInt) 
                    | Ok t -> Error (exp_ty e.pos t TInt) 
                    | Error e -> Error e
                    ) 
    | Assert (e) -> (match loop context e with 
                    | Ok (TBool) -> Ok (TUnit) 
                    | Ok t -> Error (exp_ty e.pos t TBool)  
                    | Error e -> Error e 
                    ) 
    | Tuple e_list ->
        let rec out_ty rest = 
          match rest with
          | [] -> Ok []
          | e :: es -> (match loop context e with
                        | Ok t -> (match out_ty es with
                                    | Ok ts -> Ok (t :: ts)
                                    | Error e -> Error e
                                    )
                        | Error e -> Error e
                        )
        in
        (match out_ty e_list with
         | Ok t_list -> Ok (TTuple t_list)
         | Error e -> Error e)
    | Match (e, branches) ->
        (match loop context e with
        | Error err -> Error err
        | Ok t_scrut ->
              let rec bind_pattern (context : ctxt) (pat : pattern) (t : ty) (pos : pos) (bound : string list) : (ctxt * string list, Error_msg.t) result =
                match pat.pattern, t with
                | PUnit, TUnit -> Ok (context, bound)
                | PBool _, TBool -> Ok (context, bound)
                | PInt _, TInt -> Ok (context, bound)
                | PNil, TInt_list -> Ok (context, bound)
                | PVar "_", t -> Ok (Env.add "_" t context, bound)  (* wildcard: skip dup check *)
                | PVar x, t ->
                    if List.mem x bound
                    then Error (bound_several_times pat.pos x)
                    else Ok (Env.add x t context, x :: bound)
                | PCons (p_head, p_tail), TInt_list ->
                    (match bind_pattern context p_head TInt pos bound with
                    | Error err -> Error err
                    | Ok (ctx, bound') -> bind_pattern ctx p_tail TInt_list pos bound')
                | PTuple ps, TTuple ts ->
                    if List.length ps <> List.length ts
                    then Error (exp_diff_tuple_pat pat.pos t)
                    else List.fold_left2
                          (fun acc p ti ->
                            match acc with
                            | Error err -> Error err
                            | Ok (ctx, bound') -> bind_pattern ctx p ti pos bound')
                          (Ok (context, bound)) ps ts
                | PTuple _, t -> Error (exp_tuple_pat pat.pos t)
                | PCons _, t -> Error (exp_pat pat.pos TInt_list t)
                | PUnit, t -> Error (exp_pat pat.pos TUnit t)
                | PBool _, t -> Error (exp_pat pat.pos TBool t)
                | PInt _, t -> Error (exp_pat pat.pos TInt t)
                | PNil, t -> Error (exp_pat pat.pos TInt_list t)
            in
            let rec check_branches = function
              | [] -> assert false
              | [(pat, body)] ->
                  (match bind_pattern context pat t_scrut exp.pos [] with
                  | Error err -> Error err
                  | Ok (pat_ctxt, _) -> loop pat_ctxt body)
              | (pat, body) :: rest ->
                  (match bind_pattern context pat t_scrut exp.pos [] with
                  | Error err -> Error err
                  | Ok (pat_ctxt, _) ->
                      (match loop pat_ctxt body with
                      | Error err -> Error err
                      | Ok t_branch ->
                          (match check_branches rest with
                          | Error err -> Error err
                          | Ok t_rest ->
                              if t_branch = t_rest then Ok t_branch
                              else Error (exp_ty body.pos t_branch t_rest))))
            in
            check_branches branches)
  in loop ctxt e 

let type_of (p : prog) : (ty, Error_msg.t) result =
  let rec go ctxt ty p =
    match p with
    | [] -> Ok (Option.value ~default:TUnit ty)
    | {pos; stmt=SLet {is_rec; name; args; annot; binding}} :: ps -> (
      let body = {pos=dummy_pos; expr=Var name} in
      let e = {pos; expr=Let {is_rec; name; args; annot; binding; body}} in
      match type_of_expr ctxt e with
      | Ok ty ->
        let ctxt = Env.add name ty ctxt in
        go ctxt (Some ty) ps
      | Error err -> Error err
    )
  in go Env.empty None p


(* EVALUATION
   ----------------------------------------------------------------------
*)

(* Values *)

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VTuple of value list
  | VClos of {
      env : value Env.t;
      name : string option;
      args : string list;
      body : expr;
    }
  | VInt_list of int list

(* Dynamic Environments *)

type dyn_env = value Env.t

(* Evaluation *)

exception Div_by_zero of pos
exception Assert_fail of pos
exception Match_fail of pos

let eval_expr (env : dyn_env) (e : expr) : value =
  let rec loop (environment : dyn_env) (exp : expr) = 
    match exp.expr with 
    | Unit  -> VUnit  
    | Bool b -> VBool b 
    | Int n -> VInt n 
    | Nil -> VInt_list [] 
    | Var x -> Env.find x environment (* For this part, I googled "Ocaml, Map.male (String)" and it taught me about the common operations of map module *)
    | Let {is_rec; name; args; binding; body; _} -> if not is_rec && (args = []) 
                                                        (* LETE *)
                                                        then let v1 = loop environment binding in 
                                                            let env2 = Env.add name v1 environment in 
                                                            loop env2 body 
                                                        else
                                                            (* LETRECE *)
                                                            let arg_names = List.map fst args in
                                                            let fval = VClos { env = environment; name = if is_rec then Some name else None; args = arg_names; body = binding } in
                                                            let closure = Env.add name fval environment in
                                                            let fval_out =
                                                              if is_rec then VClos { env = closure; name = Some name; args = arg_names; body = binding }
                                                              else fval
                                                            in
                                                            let env2 = Env.add name fval_out environment in
                                                            loop env2 body
    | If (e1, e2, e3) -> (match loop environment e1 with
                          | VBool true  -> loop environment e2
                          | VBool false -> loop environment e3
                          | _ -> assert false 
                          )  
    (* | Fun _ -> assert false 
    | App _ -> assert false  *)
    | Fun (args, e) -> VClos {env = environment; name = None; args = List.map fst args; body = e}
    (* | App (e1, e_args) -> let v1 = loop environment e1 in 
                          let v_args = List.map (loop environment) e_args in 
                          (match v1 with
                            | VClos {env = env2; name; args; body = e} -> 
                                    let rec get_env3 env args vals = 
                                      match args, vals with 
                                      | [], [] -> env
                                      | ar :: ars, v :: vs -> get_env3 (Env.add ar v env) ars vs
                                      | _ -> assert false
                                    in
                                    let env3 = get_env3 env2 args v_args in
                                    let env3 = match name with
                                      | Some n -> Env.add n v1 env3
                                      | None   -> env3
                                    in
                                    loop env3 e
                            | _ -> assert false)  *)
    (* | App (e1, e_args) -> 
                    let v1 = loop environment e1 in 
                    let v_args = List.map (loop environment) e_args in 
                    (match v1 with
                    | VClos {env = env2; name; args; body = e} ->
                        let rec get_env3 env args vals = 
                          match args, vals with 
                          | [], [] -> env
                          | ar :: ars, v :: vs -> get_env3 (Env.add ar v env) ars vs
                          | _ -> assert false
                        in
                        let n_args = List.length args in
                        let n_vals = List.length v_args in
                        if n_vals < n_args then
                          let applied = List.filteri (fun i _ -> i < n_vals) args in
                          let remaining = List.filteri (fun i _ -> i >= n_vals) args in
                          let env3 = List.fold_left2 (fun env a v -> Env.add a v env) env2 applied v_args in
                          VClos {env = env3; name; args = remaining; body = e}
                        else
                          let env3 = get_env3 env2 args v_args in
                          let env3 = match name with
                            | Some n -> Env.add n v1 env3
                            | None   -> env3
                          in
                          loop env3 e
                    | _ -> assert false) *)
    | App (e1, e_args) -> 
                let v1 = loop environment e1 in 
                let v_args = List.map (loop environment) e_args in 
                let rec apply_closure v vals =
                  match v, vals with
                  | _, [] -> v
                  | VClos {env = env2; name; args; body = e}, _ ->
                      let n_args = List.length args in
                      let n_vals = List.length vals in
                      if n_vals < n_args then
                        (* partial application *)
                        let applied = List.filteri (fun i _ -> i < n_vals) args in
                        let remaining = List.filteri (fun i _ -> i >= n_vals) args in
                        let env3 = List.fold_left2 (fun env a v -> Env.add a v env) env2 applied vals in
                        VClos {env = env3; name; args = remaining; body = e}
                      else
                        (* full or over-application *)
                        let this_vals = List.filteri (fun i _ -> i < n_args) vals in
                        let rest_vals = List.filteri (fun i _ -> i >= n_args) vals in
                        let env3 = List.fold_left2 (fun env a v -> Env.add a v env) env2 args this_vals in
                        let env3 = match name with
                          | Some n -> Env.add n v env3
                          | None -> env3
                        in
                        apply_closure (loop env3 e) rest_vals
                  | _ -> assert false
                in
                apply_closure v1 v_args
    | Bop (bop, e1, e2) -> (match bop with
                            | And -> (match loop environment e1 with
                                      | VBool false -> VBool false
                                      | VBool true  -> loop environment e2
                                      | _ -> assert false)
                            | Or  -> (match loop environment e1 with
                                      | VBool true  -> VBool true
                                      | VBool false -> loop environment e2
                                      | _ -> assert false)
                            | _ -> 
                          (match loop environment e1, loop environment e2 with
                            | VInt (v1), VInt (v2) ->
                                (match bop with
                                  | Add -> VInt (v1 + v2)
                                  | Sub -> VInt (v1 - v2)
                                  | Mul -> VInt (v1 * v2)
                                  | Div -> if v2 = 0 then raise (Div_by_zero exp.pos) else VInt (v1 / v2) 
                                  | Mod -> if v2 = 0 then raise (Div_by_zero exp.pos) else VInt (v1 mod v2) 
                                  | Lt  -> VBool (v1 < v2)
                                  | Lte -> VBool (v1 <= v2)
                                  | Gt  -> VBool (v1 > v2)
                                  | Gte -> VBool (v1 >= v2)
                                  | Eq  -> VBool (v1 = v2)
                                  | Neq -> VBool (v1 <> v2) 
                                  | _ -> assert false) 
                            | VBool (v1), VBool (v2) ->
                                (match bop with
                                  | Eq  -> VBool (v1 = v2)
                                  | Neq -> VBool (v1 <> v2)
                                  | Lt  -> VBool (v1 < v2)
                                  | Lte -> VBool (v1 <= v2)
                                  | Gt  -> VBool (v1 > v2)
                                  | Gte -> VBool (v1 >= v2)
                                  (* | And -> if v1 then VBool(v2) else VBool(v1)
                                  | Or  -> if v1 then VBool(v1) else VBool(v2) *)
                                  | _ -> assert false
                                )
                            | VInt v1, VInt_list vs ->
                                  (match bop with
                                    | Cons -> VInt_list (v1 :: vs)
                                    | _ -> assert false)
                            | v1, v2 -> 
                                  (match bop with 
                                  | Eq  -> VBool (v1 = v2) 
                                  | Neq -> VBool (v1 <> v2) 
                                  | Lt -> VBool (v1 < v2)
                                  | Lte -> VBool (v1 <= v2)
                                  | Gt -> VBool (v1 > v2)
                                  | Gte -> VBool (v1 >= v2)
                                  | _ -> assert false) 
                            )  )
    | Negate (e) -> (match loop environment e with 
                    | VInt (v1) -> VInt (- v1) 
                    | _ -> assert false  
                    ) 
    | Assert (e) -> (match loop environment e with 
                    | VBool (true)  -> VUnit
                    | VBool (false) -> raise (Assert_fail exp.pos)
                    | _ -> assert false  
                    ) 
    | Tuple e_list -> VTuple (List.map (loop environment) e_list)
    | Match (e, branches) ->
                    let rec bind_pat (env : dyn_env) (pat : pattern) (v : value) : dyn_env option =
                      match pat.pattern, v with
                      | PUnit, VUnit -> Some env
                      | PBool b1, VBool b2 -> if b1 = b2 then Some env else None
                      | PInt n1, VInt n2 -> if n1 = n2 then Some env else None
                      | PNil, VInt_list [] -> Some env
                      | PVar "_", _ -> Some env  (* wildcard: match anything, don't bind *)
                      | PVar x, v -> Some (Env.add x v env)
                      | PCons (p_head, p_tail), VInt_list (x :: xs) ->
                          (match bind_pat env p_head (VInt x) with
                          | None -> None
                          | Some env' -> bind_pat env' p_tail (VInt_list xs))
                      | PTuple ps, VTuple vs ->
                          if List.length ps <> List.length vs then None
                          else List.fold_left2
                                (fun acc p v ->
                                  match acc with
                                  | None -> None
                                  | Some env' -> bind_pat env' p v)
                                (Some env) ps vs
                      | _ -> None
                    in
                    let v_scrut = loop environment e in
                    let rec try_branches = function
                      | [] -> raise (Match_fail exp.pos)
                      | (pat, body) :: rest ->
                          (match bind_pat environment pat v_scrut with
                          | None -> try_branches rest
                          | Some env' -> loop env' body)
                    in
                    try_branches branches
  in loop env e 

let eval (p : prog) : value =
  let rec go env v p =
    match p with
    | [] -> Option.value ~default:VUnit v
    | {pos; stmt=SLet {is_rec; name; args; annot; binding}} :: ps ->
      let body = {pos=dummy_pos; expr=Var name} in
      let e = {pos; expr=Let {is_rec; name; args; annot; binding; body}} in
      let v = eval_expr env e in
      go (Env.add name v env) (Some v) ps
  in go Env.empty None p


(* INTERPRETER
   ----------------------------------------------------------------------
*)

let interp ~(filename : string) : (value * ty, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let* prog = Syntax.parse ~filename in
  let* prog = Ast.Interp2.prog_of_prog prog in
  let* ty = type_of prog in
  let* v =
    match eval prog with
    | v -> Ok v
    | exception Assert_fail pos -> Error (Error_msg.mk pos "(Exception) Assert_fail")
    | exception Div_by_zero pos -> Error (Error_msg.mk pos "(Exception) Div_by_zero")
    | exception Match_fail pos -> Error (Error_msg.mk pos "(Exception) Match_fail")
  in
  Ok (v, ty)


(* TESTING STUFF
   ----------------------------------------------------------------------
*)

let parse_expr s =
  let s = "let _ = " ^ s in
  let p = Parser.prog Lexer.read (Lexing.from_string s) in
  match Ast.Interp2.prog_of_prog p with
  | Ok [{pos=_;stmt=SLet {binding=e;_}}] -> e
  | _ -> assert false

let parse_ty s =
  let s = "let _ : " ^ s ^ " = assert false" in
  let p = Parser.prog Lexer.read (Lexing.from_string s) in
  match Ast.Interp2.prog_of_prog p with
  | Ok [{pos=_;stmt=SLet {annot=Some ty;_}}] -> ty
  | _ -> assert false
