
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
                                                                            else loop (Env.add name t1 context) body
                                                            | _ -> None ) 
    | If (e1, e2, e3) -> (match loop context e1, loop context e2, loop context e3 with
                          | Some (Bool : ty), Some (t2), Some (t3) ->
                              if t2 = t3 then Some (t2) else None
                          | _ -> None)  
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
                                  | _ -> None) 
                            | Some (t1), Some (t2) -> if t1 = t2 then (match bop with
                                                                        | Eq | Neq -> Some (Bool : ty)
                                                                        | _        -> None
                                                                      ) 
                                                                  else None 
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
  let rec loop (environment : dyn_env) (exp : expr) = 
    match exp with 
    | Unit  -> Unit  
    | Bool b -> Bool b 
    | Int n -> Int n 
    | Var x -> Env.find x environment (* For this part, I googled "Ocaml, Map.male (String)" and it taught me about the common operations of map module *)
    | Let (x, e1, e2) -> let v1 = loop environment e1 in 
                          loop (Env.add x v1 environment) e2  
    | LetRec {name; arg; arg_ty; binding; body; _} -> let env2 = (Env.add name (Clos (environment, Some name, Fun (arg, arg_ty, binding))) environment) in loop env2 body (* I used gen-ai to help me debug this line; my original code put the "_" in the middle of the pattern *)
    | If (e1, e2, e3) -> (match loop environment e1 with
                          | Bool true  -> loop environment e2
                          | Bool false -> loop environment e3
                          | _ -> assert false 
                          )  
    | Fun (x, t, e) -> Clos (environment, None , Fun (x, t, e))    
    | App (e1, e2) -> let v1   = loop environment e1 in
                      let v2 = loop environment e2 in
                      (match v1 with
                      | Clos (env2, name, Fun (x, _, e)) ->
                          let env3 = Env.add x v2 env2 in
                          let env3 = match name with
                            | Some n -> Env.add n v1 env3
                            | None   -> env3
                          in
                          loop env3 e
                      | _ -> assert false) 
    | Bop (bop, e1, e2) -> (match loop environment e1, loop environment e2 with
                            | Int (v1), Int (v2) ->
                                (match bop with
                                  | Add -> Int (v1 + v2)
                                  | Sub -> Int (v1 - v2)
                                  | Mul -> Int (v1 * v2)
                                  | Div -> if v2 = 0 then raise Div_by_zero else Int (v1 / v2) 
                                  | Mod -> if v2 = 0 then raise Div_by_zero else Int (v1 mod v2) 
                                  | Lt  -> Bool (v1 < v2)
                                  | Lte -> Bool (v1 <= v2)
                                  | Gt  -> Bool (v1 > v2)
                                  | Gte -> Bool (v1 >= v2)
                                  | Eq  -> Bool (v1 = v2)
                                  | Neq -> Bool (v1 <> v2) 
                                  | _ -> assert false) 
                            | Bool (v1), Bool (v2) ->
                                (match bop with
                                  | And -> Bool (v1 && v2) 
                                  | Or -> Bool (v1 || v2) 
                                  | Eq  -> Bool (v1 = v2) 
                                  | Neq -> Bool (v1 <> v2) 
                                  | _ -> assert false) 
                            | v1, v2 -> 
                                  (match bop with 
                                  | Eq  -> Bool (v1 = v2) 
                                  | Neq -> Bool (v1 <> v2) 
                                  | _ -> assert false) 
                            )  
    | Negate (e) -> (match loop environment e with 
                    | Int (v1) -> Int (- v1) 
                    | _ -> assert false  
                    ) 
    | Assert (e) -> (match loop environment e with 
                    | Bool (true)  -> Unit
                    | Bool (false) -> raise Assert_fail
                    | _ -> assert false  
                    )
  in loop env e 

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

