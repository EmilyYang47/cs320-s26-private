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

let type_of_expr (ctxt : ctxt) (e : expr) : (ty_scheme, Error_msg.t) result =
  ignore (ctxt, e); assert false

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
    | Match (_, _) -> assert false 
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
