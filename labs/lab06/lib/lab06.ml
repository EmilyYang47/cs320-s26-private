
let rec map (f : 'a -> 'b) (l : 'a list) : 'a list =
  match l with 
  | [] -> []
  | x :: xs -> f x :: map f xs

let rec filter (f : 'a -> bool) (l : 'a list) : 'a list =
  math l with 
  | [] -> [] 
  (* | x :: xs when f x -> x :: filter f xs 
  | _ :: xs -> filter f xs  *)
  | x :: xs -> if f x then x :: filter f xs 
  else filter f xs 

let fold_left
    (f : 'acc -> 'a -> 'acc)
    (base : 'acc)
    (l : 'a list) : 'acc =
  match l with 
  | [] -> base 
  | x :: xs -> fold_lect f (f base x) xs 

let fold_right
    (f : 'a -> 'acc -> 'acc)
    (l : 'a list)
    (base : 'acc) : 'acc =
  match l with 

  
module Matrix = struct
  type 'a t = 'a list list

  let init (dim : int * int) (f : int -> int -> 'a) : 'a t =
    ignore (dim, f); assert false

  let map (f : 'a -> 'b) (m : 'a t) : 'b t =
    ignore (f, m); assert false

  let fold_left
      (f : 'acc -> 'a -> 'acc)
      (base : 'acc)
      (m : 'a t) : 'acc =
    ignore (f, base, m); assert false
end
