
let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

type token = Lpar | Rpar | Word of string

let tokens_of_string (s : string) : token list =
  let rec go acc i =
    if i >= String.length s
    then acc
    else
      match s.[i] with
      | '(' -> go (Lpar :: acc) (i + 1)
      | ')' -> go (Rpar :: acc) (i + 1)
      | c ->
        if is_ws c
        then go acc (i + 1)
        else
          let rec go' j =
            if i + j >= String.length s
            then Word (String.sub s i j) :: acc
            else
              let c = s.[i + j] in
              if List.mem c ['('; ')'] || is_ws c
              then go (Word (String.sub s i j) :: acc) (i + j)
              else go' (j + 1)
          in go' 1
  in List.rev (go [] 0)

type sexpr =
  | Atom of string
  | List of sexpr list

let sexpr_of_tokens_opt (ts : token list) : sexpr option =
  let rec go (ts : token list) : (sexpr * token list) option =
    match ts with
    | [] -> None
    | t :: ts -> (
        match t with
        | Word a -> Some (Atom a, ts)
        | Lpar -> (
            match go' ts with
            | es, Rpar :: ts -> Some (List es, ts)
            | _ -> None
          )
        | Rpar -> None
      )
  and go' (ts : token list) : sexpr list * token list =
    match go ts with
    | Some (e, ts) ->
      let (es, ts) = go' ts in
      e :: es, ts
    | None -> [], ts
  in
  match go ts with
  | Some (e, []) -> Some e
  | _ -> None

let sexpr_of_string_opt (s : string) : sexpr option =
  sexpr_of_tokens_opt (tokens_of_string s)

let rec string_of_sexpr (e : sexpr) : string =
  match e with
  | Atom s -> s
  | List ss -> "(" ^ String.concat " " (List.map string_of_sexpr ss) ^ ")"

type op = Add | Mul | Eq

let string_of_op (op : op) : string =
  match op with
  | Add -> "+"
  | Mul -> "*"
  | Eq -> "="

let op_of_sexpr_opt (s : sexpr) : op option =
  match s with
  | Atom "+" -> Some Add
  | Atom "*" -> Some Mul
  | Atom "=" -> Some Eq
  | _ -> None

type expr =
  | Int of int
  | Bop of op * expr * expr
  | If of expr * expr * expr

let expr_of_sexpr_opt (e : sexpr) : expr option =
  let rec loop e = 
    match e with 
    | Atom n -> Some (Int (int_of_string n))
    | List [op; e1; e2] ->  (let sexpr_op = op_of_sexpr_opt (op) in 
                              let sexpr_e1 = loop (e1) in 
                              let sexpr_e2 = loop (e2) in 
                              match (sexpr_op, sexpr_e1, sexpr_e2) with 
                              |  (Some sop, Some se1, Some se2) -> Some (Bop (sop, se1, se2)) 
                              | _ -> None 
                            )
    | List [Atom "if"; cond; e1; e2] -> (let sexpr_cond = loop (cond) in 
                                        let sexpr_e1 = loop (e1) in 
                                        let sexpr_e2 = loop (e2) in 
                                        match (sexpr_cond, sexpr_e1, sexpr_e2) with 
                                        |  (Some c, Some se1, Some se2) -> Some (If (c, se1, se2)) 
                                        | _ -> None 
                                        )
    | _ -> None 

  in loop e


let expr_of_string_opt (e : string) : expr option =
  match sexpr_of_string_opt e with 
  | Some sexpr -> expr_of_sexpr_opt sexpr 
  | None -> None 

let sexpr_of_expr (e : expr) : sexpr =
  let rec loop e = 
    match e with 
    | Int n -> Atom (string_of_int n)
    | Bop (op, e1, e2) ->  List [Atom (string_of_op op); loop e1; loop e2] 
    | If (cond, e1, e2) ->  List [Atom "if"; loop cond; loop e1; loop e2]  

  in loop e


let string_of_expr (e : expr) : string =
  string_of_sexpr (sexpr_of_expr e)

type ty = BoolT | IntT

let ty_of_sexpr_opt (e : sexpr) : ty option =
  match e with
  | Atom s -> (
      match s with
      | "bool" -> Some BoolT
      | "int" -> Some IntT
      | _ -> None
    )
  | _ -> None

let string_of_ty (ty : ty) : string =
  match ty with
  | BoolT -> "bool"
  | IntT -> "int"

type ty_jmt =
  {
    expr : expr;
    ty : ty;
  }

let string_of_ty_jmt (j : ty_jmt) : string =
  string_of_expr j.expr ^ " : " ^ string_of_ty j.ty

type ty_rule =
  | Int_lit
  | Add_int
  | Mul_int
  | Eq_rule
  | If_rule

let string_of_ty_rule (r : ty_rule) =
  match r with
  | Int_lit -> "intLit"
  | Add_int -> "addInt"
  | Mul_int -> "mulInt"
  | Eq_rule -> "eq"
  | If_rule -> "if"

let ty_rule_of_sexpr_opt (e : sexpr) : ty_rule option =
  match e with
  | Atom s -> (
      match s with
      | "INTLIT" -> Some Int_lit
      | "ADDINT" -> Some Add_int
      | "MULINT" -> Some Mul_int
      | "EQ" -> Some Eq_rule
      | "IF" -> Some If_rule
      | _ -> None
    )
  | _ -> None

type ty_deriv =
  | Rule_app of {
      prem_derivs : ty_deriv list;
      concl : ty_jmt;
      rname : ty_rule;
    }
  | Hole

let ty_deriv_of_sexpr_opt (e : sexpr) : ty_deriv option = 
  let rec handle_premises p = 
    match p with 
    | [] -> Some []
    | x :: xs -> match (loop x, handle_premises xs) with 
                | Some current, Some rest -> Some (current :: rest) 
                | _ -> None 
  and loop e = 
    match e with 
    | Atom "???" -> Some (Hole) 
    | List (expr :: ty :: rname :: derivs) ->  
                            (let sexpr_expr = expr_of_sexpr_opt expr in 
                              let sexpr_ty = ty_of_sexpr_opt  ty in 
                              let sexpr_rname = ty_rule_of_sexpr_opt rname in 
                              match (sexpr_expr, sexpr_ty, sexpr_rname) with 
                              |  (Some sexpr, Some sty, Some srname) -> (let previous = (handle_premises derivs) 
                                                                        in match previous with 
                                                                          | Some ps -> let concl = {expr = sexpr; ty = sty} in Some (Rule_app {prem_derivs = ps; concl = concl; rname = srname}) 
                                                                          | _ -> None
                                                                        )
                              | _ -> None 
                            ) 
    | _ -> None 

  in loop e 

let ty_deriv_of_string_opt (e : string) : ty_deriv option =
  match sexpr_of_string_opt e with
  | Some sexpr -> ty_deriv_of_sexpr_opt sexpr
  | None -> None

let string_of_ty_deriv (d : ty_deriv) : string =
  let rec go d =
    match d with
    | Hole -> [("???", "hole")]
    | Rule_app d ->
      (string_of_ty_jmt d.concl, string_of_ty_rule d.rname) :: go' [] d.prem_derivs
  and go' has_line ds =
    let lines =
      List.fold_left
        (fun acc b -> (if b then "│  " else "   ") ^ acc)
        ""
        has_line
    in
    match ds with
    | [] -> []
    | [Hole] ->
      [lines ^ "└──???" , "hole"]
    | [Rule_app d] ->
      let next_line =
        ( lines ^ "└──" ^ string_of_ty_jmt d.concl
        , string_of_ty_rule d.rname
        )
      in next_line :: go' (false :: has_line) d.prem_derivs
    | Hole :: ds -> (lines ^ "├──???" , "hole") :: go' has_line ds
    | Rule_app d :: ds ->
      let next_line =
        ( lines ^ "├──" ^ string_of_ty_jmt d.concl
        , string_of_ty_rule d.rname
        )
      in
      next_line
      :: go' (true :: has_line) d.prem_derivs
      @ go' has_line ds
  in
  let lines = go d in
  let length = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun x _ -> x + 1) 0 in
  let width =
    List.fold_left
      (fun acc (line, _) -> max acc (length line))
      0
      lines
  in
  let lines =
    List.map
      (fun (line, rname) ->
         String.concat ""
           [
             line;
             String.init (width - length line + 2) (fun _ -> ' ');
             "("; rname; ")";
           ])
      lines
  in
  String.concat "\n" lines

let check_rule (rname : ty_rule) (premises : ty_jmt option list) (concl : ty_jmt) : bool =
  match rname with 
  | Int_lit -> premises = [] && (match concl.expr with
                                | Int _ -> concl.ty = IntT
                                | _ -> false)
  | Add_int -> (match premises with
                | [e1; e2] -> (match concl with
                                        | {expr = Bop (Add, c1, c2); ty = ty} -> (match e1 with
                                                                                | None -> true
                                                                                | Some e ->
                                                                                    e.expr = c1 &&
                                                                                    e.ty = IntT
                                                                                )
                                                                                && 
                                                                                (match e2 with
                                                                                | None -> true
                                                                                | Some e ->
                                                                                    e.expr = c2 &&
                                                                                    e.ty = IntT
                                                                                )
                                                                                &&
                                                                                ty = IntT
                                                                                
                                        | _ -> false)
                | _ -> false)
  | Mul_int -> (match premises with
                | [Some e1; Some e2] -> (match concl with
                                        | {expr = Bop (Mul, c1, c2); ty = ty} -> e1.expr = c1 && 
                                                                                e2.expr = c2 && 
                                                                                e1.ty = IntT && 
                                                                                e2.ty = IntT && 
                                                                                ty = IntT 
                                        | _ -> false)
                | _ -> false)
  | Eq_rule -> (match premises with
                | [Some e1; Some e2] -> (match concl with
                                        | {expr = Bop (Eq, c1, c2); ty = ty} -> e1.expr = c1 && 
                                                                                e2.expr = c2 && 
                                                                                e1.ty = e2.ty && 
                                                                                ty = BoolT 
                                        | _ -> false)
                | _ -> false)
  | If_rule -> (match premises with
                | [Some e1; Some e2; Some e3] -> (match concl with
                                        | {expr = If (c1, c2, c3); ty = ty} -> e1.expr = c1 && 
                                                                                e2.expr = c2 && 
                                                                                e3.expr = c3 && 
                                                                                e2.ty = e3.ty && 
                                                                                ty = e2.ty && 
                                                                                ty = e3.ty && 
                                                                                e1.ty = BoolT 
                                        | _ -> false)
                | _ -> false)

type status =
  | Complete
  | Invalid
  | Partial

let check_deriv (_ : ty_deriv) : status =
  assert false (* TODO *)

type value = BoolV of bool | IntV of int

let string_of_value (v : value) : string =
  match v with
  | BoolV b -> string_of_bool b
  | IntV n -> string_of_int n

let value_of_expr (_ : expr) : value =
  assert false (* TODO *)

type error = Parse_error | Invalid_deriv of ty_deriv

let interp (s : string) : (ty_deriv * value option, error) result  =
  match ty_deriv_of_string_opt s with
  | Some deriv -> (
    match check_deriv deriv with
    | Complete -> (
      match deriv with
      | Rule_app d -> Ok (deriv, Some (value_of_expr d.concl.expr))
      | _ -> assert false
    )
    | Partial -> Ok (deriv, None)
    | Invalid -> Error (Invalid_deriv deriv)
  )
  | None -> Error Parse_error

let example_deriv : string = "" (* TODO *)
