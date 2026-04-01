
(* Syntax *)

type ty = Ast.Interp1.ty =
  | Unit
  | Bool
  | Int
  | Fun of ty * ty

type bop = Ast.Interp1.bop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

type expr = Ast.Interp1.expr =
  | Unit
  | Bool of bool
  | Int of int
  | Var of string
  | Let of string * expr * expr
  | LetRec of {
      name : string;
      arg : string;
      arg_ty : ty;
      out_ty : ty;
      binding : expr;
      body : expr;
    }
  | If of expr * expr * expr
  | Fun of string * ty * expr
  | App of expr * expr
  | Bop of bop * expr * expr
  | Negate of expr
  | Assert of expr

(* Environments *)

module Env = Map.Make (String)

(* Values *)

type value =
  | Unit
  | Bool of bool
  | Int of int
  | Clos of value Env.t * string option * expr

(* Contexts *)

type ctxt = ty Env.t

(* Dynamic Environments *)

type dyn_env = value Env.t

(* Type Checking *)

let type_of (ctxt : ctxt) (e : expr) : ty option =
  let rec loop (context : ctxt) (exp : expr) = 
    match exp with 
    | Unit  -> Some (Unit : ty)
    | Bool _ -> Some (Bool : ty)
    | Int _ -> Some (Int : ty)
    | Var x -> Env.find_opt x context (* For this part, I googled "Ocaml, Map.male (String)" and it taught me about the common operations of map module *)
    | Let (x, e1, e2) -> (match (loop context e1) with 
                          | Some (t1) -> loop (Env.add x t1 context) e2 
                          | _ -> None
                        )   
    | LetRec {name; arg; arg_ty; out_ty; binding; body} -> let t1 : ty = Fun (arg_ty, out_ty) in
                                                            let ctxt1 = Env.add name t1 (Env.add arg arg_ty context) in
                                                            (match loop ctxt1 binding with 
                                                            | Some (t2) -> if t2 <> out_ty then None
                                                                            else loop (Env.add name t1 ctxt) body
                                                            | _ -> None ) 
    | If (e1, e2, e3) -> if (loop context e1) = Some (Bool) && (loop context e2) = (loop context e3) then (loop context e2) else None 
    | Fun (var, t1, e) -> (match loop (Env.add var t1 context) e with 
                          | None -> None 
                          | Some (t2) -> Some (Fun (t1, t2)) 
                          ) 
    | App (e1, e2) -> (match (loop context e1) with 
                      | Some (Fun (t2, t)) -> if (loop context e2) = Some (t2) then Some (t) else None 
                      | _ -> None 
                      )
    | Bop (bop, e1, e2) -> (match loop context e1, loop context e2 with
                            | Some (Int), Some (Int) ->
                                (match bop with
                                  | Add | Sub | Mul | Div | Mod -> Some (Int)
                                  | Lt  | Lte | Gt  | Gte | Eq  | Neq -> Some (Bool) 
                                  | _ -> None)
                            | Some (Bool), Some (Bool) ->
                                (match bop with
                                  | And | Or | Eq  | Neq -> Some (Bool)
                                  | _         -> None)
                            | Some (t1), Some (t2) -> if t1 = t2 then (if ((bop = Eq) || (bop = Neq)) then Some (Bool) else None) else None 
                            | _ -> None 
                            )  
    | Negate (e) -> (match loop context e with 
                    | Some (Int) -> Some (Int) 
                    | _ -> None 
                    ) 
    | Assert (e) -> (match loop context e with 
                    | Some (Bool) -> Some (Unit) 
                    | _ -> None 
                    )
  in loop ctxt e

(* Evaluation *)

exception Div_by_zero
exception Assert_fail

let eval (env : dyn_env) (e : expr) : value =
  ignore (env, e); assert false (* TODO *)

(* Interpretation *)

let interp ~(filename : string) : value option =
  let e_ty =
    match Syntax.parse ~filename with
    | Ok p -> Ast.Interp1.expr_of_prog p
    | Error e -> Error e
  in
  match e_ty with
  | Ok e -> (
      match type_of Env.empty e with
      | Some _ -> Some (eval Env.empty e)
      | _ ->
        let _type_error_msg = print_endline "Type error"
        in None
    )
  | Error e ->
    let _parse_error_msg =
      In_channel.with_open_text filename
        (fun ic ->
           let text = In_channel.input_all ic in
           let msg = Error_msg.to_string ~filename ~text e in
           Format.eprintf "%s" msg)
    in None

