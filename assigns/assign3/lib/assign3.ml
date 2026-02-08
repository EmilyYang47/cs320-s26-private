
let is_ws c =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let is_upper c =
  let c = int_of_char c in
  65 <= c && c <= 90

let all_upper x =
  let rec loop i =
    if i >= String.length x
    then true
    else if not (is_upper x.[i])
    then false
    else loop (i + 1)
  in loop 0

let lex (s : string) : string list =
  let rec go acc i =
    let rec go_digits acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_digit (String.get s (i + j))
      then go_digits acc i (j + 1)
      else go (String.sub s i j :: acc) (i + j)
    in
    let rec go_variable acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_upper (String.get s (i + j))
      then go_variable acc i (j + 1)
      else go (String.sub s i j :: acc) (i + j)
    in
    if i >= String.length s
    then List.rev acc
    else
      match String.get s i with
      | '=' -> go ("=" :: acc) (i + 1)
      | '+' -> go ("+" :: acc) (i + 1)
      | '-' -> go ("-" :: acc) (i + 1)
      | '*' -> go ("*" :: acc) (i + 1)
      | '/' -> go ("/" :: acc) (i + 1)
      | '(' -> go ("(" :: acc) (i + 1)
      | ')' -> go (")" :: acc) (i + 1)
      | c ->
        if is_digit c
        then go_digits acc i 1
        else if is_ws c
        then go acc (i + 1)
        else if is_upper c 
        then go_variable acc i 1
        else assert false
  in go [] 0 





let rec drop_last l =
  match l with
  | x :: y :: rest -> x :: drop_last (y :: rest)
  | _ -> []

let split_on_sum_sub e =
  let rec go d acc e =
    match d, e with
    | _, [] -> (List.rev acc, [], [])
    | d, "(" :: xs -> go (d + 1) ("(" :: acc) xs
    | d, ")" :: xs -> go (d - 1) (")" :: acc) xs
    | 0, "-" :: xs -> (List.rev acc, ["-"], xs)
    | 0, "+" :: xs -> (List.rev acc, ["+"], xs)
    | d, x :: xs -> go d (x :: acc) xs
  in go 0 [] e 

let split_on_mul_div e =
  let rec go d acc e =
    match d, e with
    | _, [] -> (List.rev acc, [], [])
    | d, "(" :: xs -> go (d + 1) ("(" :: acc) xs
    | d, ")" :: xs -> go (d - 1) (")" :: acc) xs
    | 0, "*" :: xs -> (List.rev acc, ["*"], xs)
    | 0, "/" :: xs -> (List.rev acc, ["/"], xs)
    | d, x :: xs -> go d (x :: acc) xs
  in go 0 [] e         







let rec eval (env : (string * int) list) (expr : string list) : int = 
  let rec loop acc prev_sign exp =
    match exp with
    | [] -> acc
    | _ ->
        let (first_part, sign, second_part) = split_on_sum_sub exp in
        match prev_sign with
        | [] -> loop acc sign second_part       
        | "+" :: _ -> loop (acc + eval_mul_div env first_part) sign second_part
        | "-" :: _ -> loop (acc - eval_mul_div env first_part) sign second_part
        | _ -> assert false
  in let (first_part, sign, second_part) = split_on_sum_sub expr in
  loop (eval_mul_div env first_part) sign second_part 



and eval_mul_div env expr = 
  let rec loop acc prev_sign exp =
    match exp with
    | [] -> acc
    | _ ->
        let (first_part, sign, second_part) = split_on_mul_div exp in
        match prev_sign with
        | [] -> loop acc sign second_part       
        | "*" :: _ -> loop (acc * eval_num_paren env first_part) sign second_part
        | "/" :: _ -> loop (acc / eval_num_paren env first_part) sign second_part
        | _ -> assert false
  in let (first_part, sign, second_part) = split_on_mul_div expr in
  loop (eval_num_paren env first_part) sign second_part 


and eval_num_paren env expr =
  let rec find_variable_value variable_list key = 
    match variable_list with 
    | (l_key, l_value) :: l -> 
      if l_key = key
      then l_value
      else find_variable_value l key 
    | [] -> assert false 
  in 
  match expr with
  | [n] -> if is_digit (String.get n 0) 
          then int_of_string n  
          else find_variable_value env n               (* number *)
  | "(" :: rest -> eval env (drop_last rest) (* parened expr *)
  | _ -> assert false                    (* undefined *) 




let insert_uniq (k : 'k) (v : 'v) (r : ('k * 'v) list) : ('k * 'v) list =
  let rec loop variable_list acc found = 
    match variable_list with 
    | [] -> if found 
            then acc 
            else (k, v) :: acc 
    | (l_key, l_value) :: l -> 
      if l_key = k
      then loop l ( (k, v) :: acc ) true 
      else loop l ( (l_key, l_value) :: acc ) found 
  in  loop r [] false

let interp (input : string) (env : (string * int) list) : int * (string * int) list =
  match lex input with
  | var :: "=" :: expr -> (
    match eval env expr with
    | ouput -> ouput, insert_uniq var ouput env
    | exception _ -> failwith "whoops!"
  )
  | _
  | exception _ -> failwith "whoops!"
