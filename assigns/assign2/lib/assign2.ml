
let is_ws c =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let lex s =
  let rec go acc i =
    let rec go_digits acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_digit (String.get s (i + j))
      then go_digits acc i (j + 1)
      else go (String.sub s i j :: acc) (i + j)
    in
    if i >= String.length s
    then List.rev acc
    else
      match String.get s i with
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
        else assert false
  in go [] 0

(* let eval _e = assert false TODO  *)

let rec drop_last l =
  match l with
  | x :: y :: rest -> x :: drop_last (y :: rest)
  | _ -> []

let split_on_minus e =
  let rec go d acc e =
    match d, e with
    | _, [] -> (List.rev acc, [])
    | d, "(" :: xs -> go (d + 1) ("(" :: acc) xs
    | d, ")" :: xs -> go (d - 1) (")" :: acc) xs
    | 0, "-" :: xs -> (List.rev acc, xs)
    | d, x :: xs -> go d (x :: acc) xs
  in go 0 [] e

let split_on_plus e =
  let rec go d acc e =
    match d, e with
    | _, [] -> (List.rev acc, [])
    | d, "(" :: xs -> go (d + 1) ("(" :: acc) xs
    | d, ")" :: xs -> go (d - 1) (")" :: acc) xs
    | 0, "+" :: xs -> (List.rev acc, xs)
    | d, x :: xs -> go d (x :: acc) xs
  in go 0 [] e

let split_on_mul e =
  let rec go d acc e =
    match d, e with
    | _, [] -> (List.rev acc, [])
    | d, "(" :: xs -> go (d + 1) ("(" :: acc) xs
    | d, ")" :: xs -> go (d - 1) (")" :: acc) xs
    | 0, "*" :: xs -> (List.rev acc, xs)
    | d, x :: xs -> go d (x :: acc) xs
  in go 0 [] e

let split_on_div e =
  let rec go d acc e =
    match d, e with
    | _, [] -> (List.rev acc, [])
    | d, "(" :: xs -> go (d + 1) ("(" :: acc) xs
    | d, ")" :: xs -> go (d - 1) (")" :: acc) xs
    | 0, "/" :: xs -> (List.rev acc, xs)
    | d, x :: xs -> go d (x :: acc) xs
  in go 0 [] e

let rec eval expr =
  let rec loop acc exp = 
    match exp with 
    | [] -> acc 
    | _ -> let (first_part, second_part) = split_on_minus exp in
          loop (acc - eval_summation first_part) second_part   
    in let (first_part, second_part) = split_on_minus expr in
          loop (eval_summation first_part) second_part  
          
and eval_summation expr =
  let rec loop acc exp =
    match exp with
    | [] -> acc
    | _ ->
        let (first_part, second_part) = split_on_plus exp in
        loop (acc + eval_mul first_part) second_part
    in let (first_part, second_part) = split_on_plus expr in
    loop (eval_mul first_part) second_part 

and eval_mul expr =
  let rec loop acc exp =
    match exp with
    | [] -> acc
    | _ ->
        let (first_part, second_part) = split_on_mul exp in
        loop (acc * eval_div first_part) second_part
    in let (first_part, second_part) = split_on_mul expr in
    loop (eval_div first_part) second_part 

and eval_div expr =
  let rec loop acc exp =
    match exp with
    | [] -> acc
    | _ ->
        let (first_part, second_part) = split_on_div exp in
        loop (acc / eval_num_paren first_part) second_part
    in let (first_part, second_part) = split_on_div expr in
    loop (eval_num_paren first_part) second_part 

and eval_num_paren expr =
  match expr with
  | [n] -> int_of_string n               (* number *)
  | "(" :: rest -> eval (drop_last rest) (* parened expr *)
  | _ -> assert false                    (* undefined *)

let interp (input : string) : int =
  match eval (lex input) with
  | output -> output
  | exception _ -> failwith "whoops!"
