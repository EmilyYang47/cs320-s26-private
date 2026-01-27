
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
  else if k < 0 then assert false 
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


let locate_next_ws (s : string) (i : int) : int = 
  (* input s: the original string 
     input i: the starting index 
     output: the index of next white space*) 
  let rec loop j = 
    (* base case1: there is no next whitespace *)
    if String.length s = j 
    then -1 
    (* base case2: next whitespace foud *) 
    else if String.get s j = ' ' || String.get s j = '\n' 
    then j 
    (* otherwise: proceed to the next index *)
    else loop (j + 1) 
  in loop i 


let split_on_ws (s : string) : string list =
  let rec loop i words = 
    (* skip all white spaces and reach next non-white space character *)
    let rec skip_ws j = 
      if j >= String.length s 
      then j 
      else if String.get s j = ' ' || String.get s j = '\n' 
      then skip_ws (j + 1) 
      else j 
    in 
    (* set this non-white space character as new start *)
    let i = skip_ws i in 
    (* base case: if already reach the last index *)
    if i >= String.length s 
    then List.rev words 
    (* otherwise: find the next white space, and add the word into the list *)
    else let j = locate_next_ws s i in 
      if j = -1 
      then let word = String.sub s i (String.length s - i) in List.rev (word :: words) 
      else let word = String.sub s i (j - i) in loop (j + 1) (word :: words) 
  in loop 0 [] 


let eval (stack : int list) (prog : string list) : int list =
  let rec loop result i = 
    if i >= (List.length prog)   
    (* base case: already implemented all progs *) 
    then result 
    (* otherwise: implement the progs *) 
    else match List.nth prog i with 
    | "*" -> (match result with 
            | x :: y :: rest -> loop ((y * x) :: rest) (i + 1) 
            | _ -> assert false) 
    | "+" -> (match result with 
            | x :: y :: rest -> loop ((y + x) :: rest) (i + 1) 
            | _ -> assert false) 
    | "-" -> (match result with 
            | x :: y :: rest -> loop ((y - x) :: rest) (i + 1) 
            | _ -> assert false) 
    | "/" -> (match result with 
            | x :: y :: rest -> loop ((y / x) :: rest) (i + 1) 
            | _ -> assert false) 
    | "mod" -> (match result with 
            | x :: y :: rest -> loop ((y mod x) :: rest) (i + 1) 
            | _ -> assert false) 
    | "sqrt" -> (match result with 
            | x :: rest -> loop ((sqrt x) :: rest) (i + 1) 
            | _ -> assert false) 
    | "^" -> (match result with 
            | x :: y :: rest -> loop ((pow y x) :: rest) (i + 1) 
            | _ -> assert false) 
    | x -> loop (int_of_string x :: result) (i + 1) 
  in loop stack 0 


let interp (input : string) : int =
  match eval [] (split_on_ws input) with
  | [output] -> output
  | _ -> failwith "whoops!"
