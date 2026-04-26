open Utils
module Error_msg = Error_msg
module Ast = Ast

(* SYNTAX
   ----------------------------------------------------------------------
*)

type ty = Ast.Type.t =
  | TUnit
  | TBool
  | TInt
  | TString
  | TTuple of ty list
  | TAdt of ty list * string
  | TFun of ty * ty
  | TParam of string

type _pattern = Ast.Pattern.pattern =
  | PWild
  | PVar of string
  | PUnit
  | PBool of bool
  | PInt of int
  | PString of string
  | PTuple of pattern list
  | PCons of string * pattern option
and pattern = Ast.Pattern.t =
  {
    pos : pos;
    pattern : _pattern;
  }

type bop = Ast.Expr.bop =
  | Add | Sub | Mul
  | Div | Mod
  | And | Or
  | Concat
  | Eq | Neq | Lt | Lte | Gt | Gte

type _expr = Ast.Expr.expr =
  | Unit
  | Bool of bool
  | Int of int
  | String of string
  | Negate of expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Annot of expr * ty
  | Tuple of expr list
  | Assert of expr
  | Var of string
  | Cons of string * expr option
  | Fun of (string * ty option) * expr
  | App of expr * expr
  | Let of
      {
        is_rec : bool;
        name : string;
        binding : expr;
        body : expr;
      }
  | Match of expr * (pattern * expr) list
and expr = Ast.Expr.t =
  {
    pos : pos;
    expr : _expr;
  }

type _stmt = Ast.Stmt.stmt =
  | SLet of
      {
        is_rec : bool;
        name : string;
        binding : expr;
      }

  | SAdt of
      {
        tpars : string list;
        name : string;
        constrs : (string * ty option) list
      }
and stmt = Ast.Stmt.t =
  {
    pos : pos;
    stmt : _stmt;
  }

module Env = Map.Make(String)


(* TYPE ERRORS
   ----------------------------------------------------------------------
*)

let dummy_error = Error_msg.mk dummy_pos "Dummy error"

let unknown_var pos x = Error_msg.mk pos (Format.asprintf "Unbound value %s" x)

let exp_ty pos t1 t2 =
  let msg =
    Format.asprintf
      "This expression has type %a but an expression was expected of type %a"
      Ast.Type.pp t1 Ast.Type.pp t2
  in Error_msg.mk pos msg

let invalid_app pos = Error_msg.mk pos "Invalid application"

let invalid_tuple pos = Error_msg.mk pos "Invalid tuple"

let unknown_cons pos x = Error_msg.mk pos (Format.asprintf "Unbound constructor %s" x)

let cons_exp_no_args pos x =
  Error_msg.mk
    pos
    (Format.asprintf "The constructor %s expects 0 arguments" x)

let cons_exp_args pos x =
  Error_msg.mk
    pos
    (Format.asprintf "The constructor %s expects arguments" x)

let exp_pat pos t1 t2 =
  let msg =
    Format.asprintf
      "This pattern matches values of type %a but a pattern was expected which matches values of type %a"
      Ast.Type.pp t1 Ast.Type.pp t2
  in Error_msg.mk pos msg

let bound_several_times pos x =
  let msg =
    Format.asprintf
      "Variable %s is bound several times in this matching"
      x
  in Error_msg.mk pos msg

let dup_ty_name pos x =
  let msg =
    Format.asprintf
      "Type using name %s is already defined"
      x
  in Error_msg.mk pos msg

let unbound_ty_var pos n =
  Error_msg.mk
    pos
    (Format.asprintf "The type variable %s is unbound in this type declaration" n)

let ty_param_several_times pos =
  Error_msg.mk
    pos
    "A type parameter occurs several times"

(* TYPING
   ----------------------------------------------------------------------
*)

type ty_scheme = string list * ty
type ctxt = ty_scheme Env.t
type constr = ty * ty

let fresh () = TParam (_gensym ()) 

let rec nub l =
  match l with
  | [] -> []
  | x :: xs -> x :: List.filter ((<>) x) (nub xs) 

let free_vars ty =
  let rec go = function
    | TTuple ts | TAdt (ts, _) -> List.concat_map go ts
    | TFun (t1, t2) -> go t1 @ go t2
    | TParam a -> [a]
    | _ -> []
  in nub (go ty)

let type_of_expr (ctxt : ctxt) (e : expr) : (ty_scheme, Error_msg.t) result =
  let rec loop (context : ctxt) (exp : expr) : (ty * constr list, Error_msg.t) result = 
    match exp.expr with 
    | Unit  -> Ok (TUnit, [])  
    | Bool _ -> Ok (TBool, [])  
    | Int _ -> Ok (TInt, []) 
    | String _ -> Ok (TString, [])  
    | Negate (e) -> (match loop context e with 
                    | Ok (ty, c) -> Ok (TInt, (ty, TInt) :: c) 
                    | Error e -> Error e
                    )
    | Var x -> (match Env.find_opt x context with
                | None -> Error (unknown_var exp.pos x)
                | Some (alphas, ty) ->
                  let betas = List.map (fun _ -> fresh ()) alphas in
                  let rec zip acc als bs = 
                    match als, bs with
                    | [], [] -> acc
                    | a::a_rest, b::b_rest -> (a, b) :: zip acc a_rest b_rest 
                    | _ -> assert false 
                  in 
                  let rec lookup al lst =
                    match lst with
                    | [] -> None
                    | (a, b) :: abs -> if a = al then Some (b) else lookup al abs
                  in 
                  let al_b_set = zip [] alphas betas in 
                  let rec subs_a_with_b ty = 
                    match ty with 
                    | TParam a -> ( match lookup a al_b_set with
                                    | Some b -> b
                                    | None -> TParam a 
                                  )
                    | TFun (t1, t2) -> TFun (subs_a_with_b t1, subs_a_with_b t2)
                    | TTuple ts     -> TTuple (List.map subs_a_with_b ts)
                    | TAdt (ts, n)  -> TAdt (List.map subs_a_with_b ts, n)
                    | t -> t
                  in
                  Ok (subs_a_with_b ty, [])) 

    | Bop (bop, e1, e2) -> (match loop context e1, loop context e2 with 
                            | Error e, _ -> Error e 
                            | _, Error e -> Error e 
                            | Ok (t1, c1), Ok (t2, c2) ->
                                (match bop with
                                  | Add | Sub | Mul | Div | Mod -> Ok (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2 )  
                                  | And | Or -> Ok (TBool, (t1, TBool) :: (t2, TBool) :: c1 @ c2 )  
                                  | Concat -> Ok (TString, (t1, TString) :: (t2, TString) :: c1 @ c2 )  
                                  | Eq | Neq | Lt | Lte | Gt | Gte -> Ok (TBool, (t1, t2) :: c1 @ c2 ) 
                                ) 
                          ) 
    | If (e1, e2, e3) -> (match loop context e1, loop context e2, loop context e3 with
                          | Ok (ty1, c1), Ok (ty2, c2), Ok (ty3, c3) -> Ok (ty2, (ty1, TBool) :: (ty3, ty2) :: c1 @ c2 @ c3) 
                          | Error e, _, _ -> Error e 
                          | _, Error e, _ -> Error e 
                          | _, _, Error e -> Error e 
                          )                  
    | Annot (e, t) -> (match loop context e with 
                      | Ok (ty, c) -> Ok (t, (ty, t) :: c) 
                      | Error e -> Error e  
                      ) 
    | Assert e -> (match loop context e with 
                    | Ok (t, c) -> Ok (TUnit, (t, TBool) :: c) 
                    | Error e -> Error e  
                    ) 
    (* | Tuple e_list -> VTuple (List.map (loop environment) e_list) 
    | Cons (name, e_option) -> (match e_option with 
                              | None -> VCons (name, None) 
                              | Some e -> VCons (name, Some (loop environment e)) 
                              ) 
    | Fun ((arg, _), e) -> VClos {env = environment; name = None; arg=arg; body=e} 
    
    | App (e1, e2) -> let v1 = loop environment e1 in
                      let v2 = loop environment e2 in
                      (match v1 with
                      | VClos {env = env2; name; arg = x; body} ->
                          let env3 = Env.add x v2 env2 in
                          let env3 = match name with
                            | Some n -> Env.add n (VClos {env = env2; name; arg=x; body}) env3
                            | None   -> env3
                          in
                          loop env3 body
                      | _ -> assert false) 
    | Let {is_rec; name = x; binding = e1; body = e2} -> if is_rec then 
                                                            (match e1.expr with
                                                            | Fun ((arg, _), e) ->
                                                              let env2 = Env.add x (VClos { env=environment; name = Some x; arg; body = e }) environment in
                                                              loop env2 e2
                                                            | _ ->
                                                              let v1 = loop environment e1 in
                                                              loop (Env.add x v1 environment) e2)
                                                        else let v1 = loop environment e1 in 
                                                              loop (Env.add x v1 environment) e2  
    | Match (e, branches) -> let rec match_patterns (env : dyn_env) (p : pattern) (v : value) : dyn_env option = 
                                match p.pattern, v with 
                                | PWild, _ -> Some env 
                                | PVar x, v -> Some (Env.add x v env) 
                                | PUnit, VUnit -> Some env 
                                | PBool b1, VBool b2 -> if b1 = b2 then Some env else None  
                                | PInt n1, VInt n2 -> if n1 = n2 then Some env else None 
                                | PString s1, VString s2 -> if s1 = s2 then Some env else None 
                                | PTuple ps, VTuple vs ->
                                    if List.length ps <> List.length vs then None 
                                    else let rec make_envr acc ps vs =
                                      match ps, vs with
                                      | [], [] -> Some acc
                                      | p :: ps, v :: vs ->
                                        (match match_patterns acc p v with
                                        | None -> None
                                        | Some envr -> make_envr envr ps vs)
                                      | _ -> None
                                    in make_envr env ps vs 
                                | PCons (x, p), VCons (vx, v) -> 
                                  if x <> vx then None 
                                  else (match p, v with 
                                        | None, None -> Some env  
                                        | Some p1, Some v2 -> match_patterns env p1 v2 
                                        | _ -> None 
                                        ) 
                                | _ -> None 
                              in let v = loop environment e in 
                              let rec check_branches branches = 
                                match branches with 
                                | [] -> raise (Match_fail exp.pos) 
                                | (p, body) :: rest -> 
                                    (match match_patterns environment p v with 
                                    | None -> check_branches rest 
                                    | Some envr -> loop envr body
                                    ) 
                              in check_branches branches 
  in loop env e *) 
    | _ -> assert false 
  in 
  let rec substitute_unification (s : (string * ty) list) (t : ty) : ty = 
    let rec lookup key lst =
      match lst with
      | [] -> None
      | (k, v) :: rest ->
          if k = key then Some v
          else lookup key rest
    in 
    match t with
    | TParam a -> (match lookup a s with
                  | Some v -> substitute_unification s v
                  | None    -> TParam a
                  )
    | TFun (t1, t2) -> TFun (substitute_unification s t1, substitute_unification s t2)
    | TTuple ts     -> TTuple (List.map (substitute_unification s) ts)
    | TAdt (ts, n)  -> TAdt (List.map (substitute_unification s) ts, n)
    | t -> t 
  in 
  let rec apply_substitution (kv : string * ty) (ty : ty) : ty =
    let (a, replacement) = kv in
    match ty with
    | TParam b -> if b = a then replacement else TParam b
    | TFun (t1, t2) -> TFun (apply_substitution kv t1, apply_substitution kv t2)
    | TTuple ts     -> TTuple (List.map (apply_substitution kv) ts)
    | TAdt (ts, n)  -> TAdt (List.map (apply_substitution kv) ts, n)
    | t -> t
  in 
  let unification (constraints : constr list) : ((string * ty) list, Error_msg.t) result = 
    let rec loop acc constraint_set = 
      match constraint_set with 
      | [] -> Ok (acc)  
      | (t1, t2) :: cs -> if t1 = t2 then loop acc cs 
                          else (match (t1, t2) with 
                                  | TFun (s1, t1), TFun (s2, t2) -> loop acc ((s1, s2) :: (t1, t2) :: cs) 
                                  | TParam a, t | t, TParam a -> if not (List.mem a (free_vars t)) then 
                                                                  let rest = List.map (fun (l, r) -> (apply_substitution (a, t) l, apply_substitution (a, t) r)) cs in
                                                                  loop ((a, t) :: acc) rest 
                                                                  else Error dummy_error 
                                  | TTuple a, TTuple b -> if List.length a = List.length b 
                                                          then let rec zip acc tp1 tp2 = 
                                                            match tp1, tp2 with
                                                            | [], [] -> acc
                                                            | a::a_rest, b::b_rest -> (a, b) :: zip acc a_rest b_rest 
                                                            | _ -> assert false 
                                                          in let combined = zip [] a b 
                                                          in loop acc (combined @ cs)   
                                                          else Error dummy_error                    
                                  | TAdt (ts1, n1), TAdt (ts2, n2) -> if n1 = n2  
                                                                      then let rec zip acc ts1 ts2 = 
                                                                        match ts1, ts2 with
                                                                        | [], [] -> acc
                                                                        | a::a_rest, b::b_rest -> (a, b) :: zip acc a_rest b_rest 
                                                                        | _ -> assert false 
                                                                      in let combined = zip [] ts1 ts2 
                                                                      in loop acc (combined @ cs)    
                                                                      else Error dummy_error           
                                  | _ -> Error dummy_error 
                          ) 
    in loop [] constraints   
  in 
  match loop ctxt e with 
  | Ok ((ty, constrs)) -> (match unification constrs with 
                          | Ok (set) -> let substituted_ty = substitute_unification set ty in 
                                          let free_variables = free_vars substituted_ty in 
                                          Ok (free_variables, substituted_ty) 
                          | Error e -> Error e 
                          ) 
  | Error e -> Error e 
      
    
    (* in loop ctxt e  *)









let well_typed (p : stmt list) : (unit, Error_msg.t) result =
  let rec go (used_ty_names : string list) (ctxt : ctxt) p =
    match p with
    | [] -> Ok ()
    | {pos; stmt=SLet {is_rec;name;binding}} :: ps ->
      let body = Ast.Expr.var dummy_pos name in
      let e = Ast.Expr.let_ pos is_rec name [] None binding body in
      begin
        match type_of_expr ctxt e with
        | Ok ty -> go used_ty_names (Env.add name ty ctxt) ps
        | Error e -> Error e
      end
    | {pos; stmt=SAdt {tpars; name; constrs}} :: ps ->
      if nub tpars = tpars
      then
        if List.mem name used_ty_names
        then Error (dup_ty_name pos name)
        else
          let rec process ctxt cs =
            match cs with
            | [] -> Ok ctxt
            | (cons_name, None) :: cs ->
              let tparams = List.map (fun x -> TParam x) tpars in
              process (Env.add cons_name (tpars, TAdt(tparams, name)) ctxt) cs
            | (cons_name, Some ty) :: cs ->
              begin
                match List.(find_opt (fun x -> not (mem x tpars)) (free_vars ty)) with
                | None ->
                  let tparams = List.map (fun x -> TParam x) tpars in
                  let ctxt = Env.add cons_name (tpars, TFun (ty, TAdt(tparams, name))) ctxt in
                  process ctxt cs
                | Some a -> Error (unbound_ty_var pos a)
              end
          in
          match process ctxt constrs with
          | Ok ctxt -> go (name :: used_ty_names) ctxt ps
          | Error e-> Error e
      else Error (ty_param_several_times pos)
  in
  let ctxt =
    Env.(
      empty
      |> add "print_endline" ([], TFun (TString, TUnit))
      |> add "Nil" (["a"], TAdt ([TParam "a"], "list"))
      |> add "Cons" (["a"], TFun (TTuple [TParam "a"; TAdt ([TParam "a"], "list")], TAdt ([TParam "a"], "list")))
    )
  in go [] ctxt p

(* EVALUATION
   ----------------------------------------------------------------------
*)

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VString of string
  | VCons of string * value option
  | VTuple of value list
  | VClos of {
      env : value Env.t;
      name : string option;
      arg : string;
      body : expr;
    }

type dyn_env = value Env.t

exception Div_by_zero of pos
exception Assert_fail of pos
exception Match_fail of pos
exception Compare_fun_val of pos

let eval_expr (env : dyn_env) (e : Ast.Expr.t) : value =
  let rec loop (environment : dyn_env) (exp : expr) = 
    match exp.expr with 
    | Unit  -> VUnit  
    | Bool b -> VBool b 
    | Int n -> VInt n 
    | String s -> VString s 
    | Negate e -> (match loop environment e with 
                    | VInt (v1) -> VInt (- v1) 
                    | _ -> assert false  
                    ) 
    | Var x -> Env.find x environment
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
                            | VString v1, VString v2 ->
                                  (match bop with
                                    | Concat -> VString (v1 ^ v2)
                                    | Eq  -> VBool (v1 = v2) 
                                    | Neq -> VBool (v1 <> v2) 
                                    | Lt -> VBool (v1 < v2)
                                    | Lte -> VBool (v1 <= v2)
                                    | Gt -> VBool (v1 > v2)
                                    | Gte -> VBool (v1 >= v2)
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
    | If (e1, e2, e3) -> (match loop environment e1 with
                          | VBool true  -> loop environment e2
                          | VBool false -> loop environment e3
                          | _ -> assert false 
                          )      
    | Annot (e, _) -> loop environment e 
    | Assert e -> (match loop environment e with 
                    | VBool (true)  -> VUnit
                    | VBool (false) -> raise (Assert_fail exp.pos)
                    | _ -> assert false  
                    ) 
    | Tuple e_list -> VTuple (List.map (loop environment) e_list) 
    | Cons (name, e_option) -> (match e_option with 
                              | None -> VCons (name, None) 
                              | Some e -> VCons (name, Some (loop environment e)) 
                              ) 
    | Fun ((arg, _), e) -> VClos {env = environment; name = None; arg=arg; body=e} 
    
    | App (e1, e2) -> let v1 = loop environment e1 in
                      let v2 = loop environment e2 in
                      (match v1 with
                      | VClos {env = env2; name; arg = x; body} ->
                          let env3 = Env.add x v2 env2 in
                          let env3 = match name with
                            | Some n -> Env.add n (VClos {env = env2; name; arg=x; body}) env3
                            | None   -> env3
                          in
                          loop env3 body
                      | _ -> assert false) 
    | Let {is_rec; name = x; binding = e1; body = e2} -> if is_rec then 
                                                            (match e1.expr with
                                                            | Fun ((arg, _), e) ->
                                                              let env2 = Env.add x (VClos { env=environment; name = Some x; arg; body = e }) environment in
                                                              loop env2 e2
                                                            | _ ->
                                                              let v1 = loop environment e1 in
                                                              loop (Env.add x v1 environment) e2)
                                                        else let v1 = loop environment e1 in 
                                                              loop (Env.add x v1 environment) e2  
    | Match (e, branches) -> let rec match_patterns (env : dyn_env) (p : pattern) (v : value) : dyn_env option = 
                                match p.pattern, v with 
                                | PWild, _ -> Some env 
                                | PVar x, v -> Some (Env.add x v env) 
                                | PUnit, VUnit -> Some env 
                                | PBool b1, VBool b2 -> if b1 = b2 then Some env else None  
                                | PInt n1, VInt n2 -> if n1 = n2 then Some env else None 
                                | PString s1, VString s2 -> if s1 = s2 then Some env else None 
                                | PTuple ps, VTuple vs ->
                                    if List.length ps <> List.length vs then None 
                                    else let rec make_envr acc ps vs =
                                      match ps, vs with
                                      | [], [] -> Some acc
                                      | p :: ps, v :: vs ->
                                        (match match_patterns acc p v with
                                        | None -> None
                                        | Some envr -> make_envr envr ps vs)
                                      | _ -> None
                                    in make_envr env ps vs 
                                | PCons (x, p), VCons (vx, v) -> 
                                  if x <> vx then None 
                                  else (match p, v with 
                                        | None, None -> Some env  
                                        | Some p1, Some v2 -> match_patterns env p1 v2 
                                        | _ -> None 
                                        ) 
                                | _ -> None 
                              in let v = loop environment e in 
                              let rec check_branches branches = 
                                match branches with 
                                | [] -> raise (Match_fail exp.pos) 
                                | (p, body) :: rest -> 
                                    (match match_patterns environment p v with 
                                    | None -> check_branches rest 
                                    | Some envr -> loop envr body
                                    ) 
                              in check_branches branches 
    in loop env e

let eval (p : stmt list) : value =
  let rec go env v p =
    match p with
    | [] -> Option.value ~default:VUnit v
    | {pos; stmt=SLet {is_rec; name; binding}} :: ps ->
      let body = {pos=dummy_pos; expr=Var name} in
      let e = Ast.Expr.let_ pos is_rec name [] None binding body in
      let v = eval_expr env e in
      go (Env.add name v env) (Some v) ps
    | _ :: ps -> go env v ps
  in
  let env =
    Env.(
      empty
      |> add "print_endline"
        (VClos
           {
             env = empty;
             name = None;
             arg = "$print_endline";
             body = Ast.Expr.mk dummy_pos Unit;
           })
    )
  in go env None p


(* INTERPRETER
   ----------------------------------------------------------------------
*)

let interp ~(filename : string) : (value, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let* prog = Syntax.parse ~filename in
  let* () = well_typed prog in
  let* v =
    match eval prog with
    | v -> Ok v
    | exception Assert_fail pos -> Error (Error_msg.mk pos "(Exception) Assert_fail")
    | exception Div_by_zero pos -> Error (Error_msg.mk pos "(Exception) Div_by_zero")
    | exception Match_fail pos -> Error (Error_msg.mk pos "(Exception) Match_fail")
    | exception Compare_fun_val pos -> Error (Error_msg.mk pos "(Exception) Compare_fun_val")
  in
  Ok v

(* TESTING STUFF
   ----------------------------------------------------------------------
*)

let parse_expr s =
  let s = "let _x = " ^ s in
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | [{pos=_;stmt=SLet {binding=e;_}}] -> e
  | _ -> assert false
