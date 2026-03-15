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

let rec dim_check (env : (string * tensor) list) (e : expr) : ((string * int) list) option =
  (* TODO *)
  match e with 
  | Ident (name, labels) -> (match get_t env name with 
                            | None -> None 
                            | Some t -> if List.length (Tensor.shape t) <> List.length labels then None 
                                        else if let rec contains l rest_labels = 
                                                  (match rest_labels with 
                                                  | [] -> false 
                                                  | x :: xs -> if l = x then true else contains l xs 
                                                  )
                                                in 
                                                let rec check_not_distinct label_lst =
                                                  (match label_lst with
                                                  | [] -> false
                                                  | x :: xs -> if contains x xs then true else check_not_distinct xs
                                                  )
                                                in 
                                                check_not_distinct labels then None 
                                        else let rec generate_lst label_lst size_lst = 
                                                (match (label_lst, size_lst) with 
                                                | ([], []) -> [] 
                                                | (x :: xs, y :: ys) -> (x, y) :: generate_lst xs ys 
                                                | (_, _) -> [] 
                                                )
                                              in 
                                              Some (generate_lst labels (Tensor.shape t))
                              )
  | Map (_, e1, e2) -> (match (dim_check env e1, dim_check env e2) with 
                        | (Some a, Some b) -> let sorted = List.sort (fun (x,_) (y,_) -> String.compare x y) (a @ b)
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
                        | (_, _) -> None 
                        )
  | Fold (_, i, e) -> match dim_check env e with 
                        | None -> None 
                        | Some a -> let rec find_size label lst =
                                      (match lst with
                                      | [] -> None
                                      | (l, s) :: rest -> if l = label then Some s else find_size label rest 
                                      )
                                    in 
                                    (match find_size i a with 
                                    | None -> None 
                                    | Some _ -> let rec remove_axis label lst =
                                                  match lst with
                                                  | [] -> []
                                                  | (l, s) :: rest -> if l = label then remove_axis label rest else (l, s) :: remove_axis label rest 
                                                in Some (remove_axis i a)
                                    )
                                             



let eval (env : (string * tensor) list) (e : expr) : tensor =
  ignore (env, e); assert false (* TODO *)

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
