
let sqrt (n : int) : int = 
  let rec loop k = 
    if n <= k * k 
    then k 
    else loop (k + 1) 
  in loop 0 

let pow (n : int) (k : int) : int = 
  (* first deal with the edge cases *)
  if n = 1 
  then n 
  else if k = 0 
  then 1
  else if n = 0
  then 0
  (* now handle the general cases *)
  else let rec loop result p = 
    if p = 0 
    then result 
    else loop (result * n) (p-1) 
  in loop 1 k

let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let string_of_char (c : char) : string =
  String.init 1 (fun _ -> c)

let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then List.rev acc
    else loop (String.get s i :: acc) (i + 1)
  in loop [] 0

let implode (cs : char list) : string =
  String.init
    (List.length cs)
    (fun i -> List.nth cs i)

let implode_all (css : char list list) : string list =
  let rec loop acc css =
    match css with
    | [] -> List.rev acc
    | cs :: rest -> loop (implode cs :: acc) rest
  in loop [] css

let split_on_ws_helper (_cs : char list) : char list list =
  assert false

let split_on_ws (s : string) : string list =
  implode_all (split_on_ws_helper (explode s))

let eval (_stack : int list) (_prog : string list) : int list =
  assert false

let interp (input : string) : int =
  match eval [] (split_on_ws input) with
  | [output] -> output
  | _ -> failwith "whoops!"
