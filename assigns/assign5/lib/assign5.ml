module Tensor = Tensor
type tensor = Tensor.t

type 'a sexpr = 'a Sexpr.t =
  | Atom of 'a
  | List of 'a sexpr list

type op = Syntax.op = Add | Mul

type expr = Syntax.expr =
  | Ident of string * string list
  | Map of op * expr * expr
  | Fold of op * string * expr

type stmt = Syntax.stmt =
  | Init of string * int list * float sexpr
  | Set of string * string list * expr

let rec get_t env name = 
  match env with 
    | [] -> None 
    | (n, t) :: rest_env -> if n = name then Some t else get_t rest_env name

let rec find_size label lst =
  match lst with
  | [] -> None
  | (l, s) :: rest -> if l = label then Some s else find_size label rest 

let get_union a b = 
  let sorted = List.sort (fun (x,_) (y,_) -> String.compare x y) (a @ b)
  in 
  let rec loop lst acc =
      (match lst with
      | [] -> Some (List.rev acc)  
      | (l, s) :: rest -> ( match acc with
                            | [] -> loop rest [(l, s)] 
                            | (prev_l, prev_s) :: _ -> if l = prev_l then
                                                          if s = prev_s then loop rest acc 
                                                          else None
                                                        else loop rest ((l, s) :: acc) 
                          )
      )
  in
  loop sorted [] 

let rec remove_axis label lst =
  match lst with
  | [] -> []
  | (l, s) :: rest -> if l = label then remove_axis label rest else (l, s) :: remove_axis label rest 

let rec contains l labels = 
  match labels with 
  | [] -> false 
  | x :: xs -> if l = x then true else contains l xs 


let rec dim_check (env : (string * tensor) list) (e : expr) : ((string * int) list) option =
  (* TODO *)
  match e with 
  | Ident (name, labels) -> (match get_t env name with 
                            | None -> None 
                            | Some t -> if List.length (Tensor.shape t) <> List.length labels then None 
                                        else if let rec check_not_distinct label_lst =
                                                  (match label_lst with
                                                  | [] -> false
                                                  | x :: xs -> if contains x xs then true else check_not_distinct xs
                                                  )
                                                in 
                                                check_not_distinct labels then None 
                                            else Some (Tensor.relabel_assoc (Tensor.idx_space t) labels)
                              )
  | Map (_, e1, e2) -> (match (dim_check env e1, dim_check env e2) with 
                        | (Some a, Some b) -> get_union a b 
                        | (_, _) -> None 
                        )
  | Fold (_, i, e) -> match dim_check env e with 
                        | None -> None 
                        | Some a -> (match find_size i a with 
                                    | None -> None 
                                    | Some _ -> Some (remove_axis i a)
                                    ) 


let rec eval (env : (string * tensor) list) (e : expr) : tensor =
  (* TODO *) 
  match e with 
  | Ident (name, labels) -> (match get_t env name with
                            | Some t -> Tensor.relabel_axes t labels
                            | None -> assert false
                            )

  | Map (op, e1, e2) -> let a = eval env e1 in 
                        let b = eval env e2 in 
                        let space = match get_union (Tensor.idx_space a) (Tensor.idx_space b) with 
                                    | None -> assert false 
                                    | Some x -> x
                        in 
                        let rec filter labels idx =
                          (match idx with
                          | [] -> []
                          | (l, v) :: rest -> (if contains l labels then [(l, v)] else []) @ filter labels rest 
                          )
                        in 
                        Tensor.init space (fun idx -> let idx1 = filter (Tensor.axis_labels a) idx
                                                      in
                                                      let idx2 = filter (Tensor.axis_labels b) idx
                                                      in
                                                      let v1 = Tensor.get a idx1 
                                                      in
                                                      let v2 = Tensor.get b idx2 
                                                      in 
                                                      match op with 
                                                      | Add -> v1 +. v2 
                                                      | Mul -> v1 *. v2 
                                          )
  | Fold (op, i, e) -> let a = eval env e in 
                      let space = Tensor.idx_space a 
                      in 
                      let size = match find_size i space with
                                | Some s -> s
                                | None -> assert false
                      in 
                      let new_sp = remove_axis i space 
                      in 
                      Tensor.init new_sp (fun idx -> let rec loop j acc =
                                                      if j = size then acc
                                                      else
                                                        let current =
                                                          if j = 0 then Tensor.get a ((i,j)::idx)
                                                          else match op with 
                                                            | Add -> acc +. Tensor.get a ((i,j)::idx) 
                                                            | Mul -> acc *. Tensor.get a ((i,j)::idx) 
                                                        in loop (j+1) current
                                                    in 
                                                    loop 0 0.0
                                          )

type error =
  | Parse_error
  | Dim_error
  | Init_error

let interp (env : (string * tensor) list) (s : string) : ((string * tensor) list, error) result =
  let interp_stmt env stmt =
    match Syntax.stmt_of_sexpr_opt stmt with
    | Some (Init (a, shape, expr)) -> (
        match Tensor.of_sexpr_opt shape expr with
        | Some t -> Ok ((a, t) :: env)
        | _ -> Error Init_error
      )
    | Some (Set (a, idx, e)) -> (
        let sort  = List.sort String.compare in
        match dim_check env e with
        | Some idx_space when sort idx = sort (List.map fst idx_space) ->
          let idx_space = List.map (fun x -> (x, List.assoc x idx_space)) idx in
          let t = Tensor.init idx_space (Tensor.get (eval env e)) in
          Ok ((a, t) :: env)
        | _ -> Error Dim_error
      )
    | _ -> Error Parse_error
  in
  let rec interp_prog env = function
    | [] -> Ok env
    | e :: es -> (
        match interp_stmt env e with
        | Ok env -> interp_prog env es
        | Error e -> Error e
      )
  in
  match Sexpr.list_of_string_opt s with
  | Some ss -> interp_prog env ss
  | None -> Error Parse_error
