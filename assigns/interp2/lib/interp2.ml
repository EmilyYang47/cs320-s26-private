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
  | TTuple ts -> list ~sep:(Fmt.any "*@ ") pp_ty ppf ts
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
      "This pattern matches values of a tuple type but a pattern was expected which matches values of different tuple type %a"
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
                | None   -> assert false
              )
    | Let {is_rec; name; args; annot; binding; body} -> if not is_rec && (args = []) 
                                                        (* LET *)
                                                        then (match (loop context binding) with 
                                                          | Ok (t1) -> loop (Env.add name t1 context) body 
                                                          | _ -> assert false 
                                                        ) 
                                                        else (match annot with 
                                                              (* LETFUNANNOT & LETREC *) 
                                                              | Some out_ty -> let arg_ctxt = List.fold_right (fun (x, t) ctx -> Env.add x t ctx) args context in 
                                                                              let arg_ty = List.fold_right (fun (_, t) acc -> TFun (t, acc)) args out_ty in 
                                                                              let binding_ctxt = if not is_rec then arg_ctxt else Env.add name arg_ty arg_ctxt in 
                                                                              (match loop binding_ctxt binding with 
                                                                              | Ok (t_k_plus_1) -> if t_k_plus_1 <> out_ty then assert false else loop (Env.add name arg_ty context) body
                                                                              | _ -> assert false ) 
                                                              (* LETFUN *)
                                                              | None -> let arg_ctxt = List.fold_right (fun (x, t) ctx -> Env.add x t ctx) args context in 
                                                                        let binding_ctxt = if not is_rec then arg_ctxt else assert false in 
                                                                        let out_ty = (match loop binding_ctxt binding with 
                                                                                      | Ok (t_k_plus_1) -> t_k_plus_1 
                                                                                      | _ -> assert false ) 
                                                                                      in 
                                                                        let arg_ty = List.fold_right (fun (_, t) acc -> TFun (t, acc)) args out_ty in 
                                                                        loop (Env.add name arg_ty context) body
                                                              ) 
    | If (e1, e2, e3) -> (match loop context e1, loop context e2, loop context e3 with
                          | Ok (TBool : ty), Ok (t2), Ok (t3) -> if t2 = t3 then Ok (t2) else assert false
                          | _ -> assert false)  
    | Fun (args, e) -> let arg_ctxt = List.fold_right (fun (x, t) ctx -> Env.add x t ctx) args context in 
                          (match loop arg_ctxt e with 
                          | Ok (t) -> let out_ty = List.fold_right (fun (_, tk) acc -> TFun (tk, acc)) args t in Ok (out_ty)
                          | _ -> assert false 
                          ) 
    | App (e, e_args) -> (match loop context e with
                      | Ok (e_out) ->
                          let rec check_arg_type ty args =
                            (match ty, args with
                            | t, [] -> Ok t
                            | TFun (t_in, t_out), x :: xs -> (match loop context x with
                                                              | Ok t_arg -> if t_arg = t_in then check_arg_type t_out xs else assert false
                                                              | _ -> assert false
                                                              )
                            | _ -> assert false
                            )  
                          in
                          check_arg_type e_out e_args
                      | Error _ -> assert false)
    | Bop (bop, e1, e2) -> (match loop context e1, loop context e2 with
                            | Ok (TInt : ty), Ok (TInt : ty) -> (match bop with
                                                                | Add | Sub | Mul | Div | Mod -> Ok (TInt  : ty)
                                                                | Lt  | Lte | Gt  | Gte | Eq  | Neq -> Ok (TBool : ty)
                                                                | _ -> assert false
                                                                )
                            | Ok (TBool : ty), Ok (TBool : ty) -> (match bop with
                                                                  | And | Or | Lt | Lte | Gt | Gte | Eq | Neq -> Ok (TBool : ty)
                                                                  | _ -> assert false
                                                                  )
                            | Ok TInt, Ok TInt_list -> (match bop with
                                                        | Cons -> Ok (TInt_list)
                                                        | _ -> assert false
                                                        )
                            | Ok t1, Ok t2 -> if t1 = t2 then (match bop with
                                                              | Lt | Lte | Gt | Gte | Eq | Neq -> Ok (TBool : ty)
                                                              | _ -> assert false
                                                              )
                                              else assert false 
                            | _ -> assert false
                            )  
    | Negate (e) -> (match loop context e with 
                    | Ok (TInt) -> Ok (TInt) 
                    | _ -> assert false 
                    ) 
    | Assert (e) -> (match loop context e with 
                    | Ok (TBool) -> Ok (TUnit) 
                    | _ -> assert false 
                    ) 
    | Tuple e_list ->
        let rec out_ty rest = 
          match rest with
          | [] -> Ok []
          | e :: es -> (match loop context e with
                        | Ok t -> (match out_ty es with
                                    | Ok ts -> Ok (t :: ts)
                                    | _ -> assert false 
                                    )
                        | Error _ -> assert false
                        )
        in
        (match out_ty e_list with
         | Ok t_list -> Ok (TTuple t_list)
         | Error _ -> assert false)
    | Match _ -> assert false 
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
    | Fun (args, body) ->
        VClos {
          env = environment;
          name = None;
          args = List.map fst args;
          body;
        }
    | App (e1, e_args) ->
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
    | Match _ -> assert false 
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
