open Core.Std
       
type inttree = Empty | Node of int * inttree * inttree

(* use this function in fromList *)
let rec insert t i =
  match t with
      Empty -> Node(i,Empty,Empty)
    | Node(j,l,r) -> 
      if i=j 
      then t 
      else if i < j 
      then Node(j,insert l i,r)
      else Node(j,l,insert r i)

(* no need for this function; it is just an example *)
let rec member t i =
  match t with
      Empty -> false
    | Node(j,l,r) -> i=j || (i < j && member l i) || member r i

(* put fromList, sum1, prod1, avg1, map1 and negateAll here *)
let fromList (l : int list) : inttree =
  List.fold l ~init:Empty ~f:insert

let rec sum1 = function 
  | Empty -> 0
  | Node (j, l, r) -> j + (sum1 l) + (sum1 r)

let rec prod1 = function
  | Empty -> 1
  | Node (j, l, r) -> j * (prod1 l) * (prod1 r)

let rec avg1 t =
  let rec avg1_helper t = match t with
    | Empty -> (0, 0)
    | Node (j, l, r) ->
      let ((lsum, lcnt), (rsum, rcnt)) = (avg1_helper l, avg1_helper r)
      in (j + lsum + rsum, 1 + lcnt + rcnt)
  in let (sum, cnt) = avg1_helper t
  in sum / cnt

let rec fold f a t =
  match t with
  | Empty -> a
  | Node(j,l,r) -> fold f (fold f (f a j) l) r

let rec map f t = match t with
  | Empty -> Empty
  | Node (j, l, r) -> Node (f j, map f l, map f r)

let negateAll t = map (fun x -> -x) t

let sum2 t = fold (fun a b -> a + b) 0 t

let prod2 t = fold (fun a b -> a * b) 1 t

let avg2 t =
  let (sum, cnt) = fold (fun (s,c) i -> (s+i, c+1)) (0,0) t
  in sum / cnt

(* put sum2, prod2, and avg2 here *)

type 'a iterator = Nomore | More of 'a * (unit -> 'a iterator)

let rec iter t =
  let rec f t k =
    match t with
	Empty -> k ()
      | Node(j,l,r) -> More(j, fun () -> f l (fun () -> f r k))
  in f t (fun () -> Nomore)

(* extra credit: put optionToException, exceptionToOption, etc. here *)

(* a little testing -- commented out since the functions do not exist yet *)

(*
let tr = fromList [0;1;2;3;4;5;6;7;8;9;9;9;1] (* repeats get removed *)
let print_ans f t = print_string (string_of_int (f t)); print_string "\n"
let _ = print_ans sum1 tr
let _ = print_ans prod1 tr
let _ = print_ans avg1 tr
let _ = print_ans sum2 tr
let _ = print_ans prod2 tr
let _ = print_ans avg2 tr
let _ = print_ans sum3 tr
let _ = print_ans prod3 tr
let _ = print_ans avg3 tr
*)
