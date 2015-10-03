open Core.Std

type id = string

type binop = Plus
           | Minus
           | Times
           | Div

type stm = CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm of exp list

 and exp = IdExp of id
         | NumExp of int
         | OpExp of exp * binop * exp
         | EseqExp of stm * exp


let prog = CompoundStm(
               AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
               CompoundStm(
                   AssignStm("b",
                             EseqExp(PrintStm [IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)],
                                     OpExp(NumExp 10, Times, IdExp "a"))),
                   PrintStm [IdExp "b"]))

let rec maxargs s =
  match s with
  | CompoundStm (s1, s2) -> max (maxargs s1) (maxargs s2)
  | AssignStm (_, e1)    -> maxargs_exp e1
  | PrintStm l           -> List.length l
and maxargs_exp e =
  match e with
  | OpExp (e1, _, e2) -> max (maxargs_exp e1) (maxargs_exp e2)
  | EseqExp (s1, e2)  -> max (maxargs s1) (maxargs_exp e2)
  | _ -> 0

type table = (id * int) list

let test_table = [("a", 5); ("b", 10)]

let lookup x env =
  match List.Assoc.find env x with
  | Some x -> x
  | None -> 0

let update x v (env : table) : table =
  List.Assoc.add env x v

let interpBinOp (op : binop) (v1 : int) (v2 : int) : int =
  match op with
  | Plus -> v1 + v2
  | Minus -> v1 - v2
  | Times -> v1 * v2
  | Div -> v1 / v2

let rec interpStm (s : stm) (env : table) : table =
  match s with
  | CompoundStm (s1, s2) -> interpStm s2 (interpStm s1 env)
  | AssignStm (x, e1)    -> let (v1, env') = (interpExp e1 env)
                            in update x v1 env'
  | PrintStm l           -> let (vs, env') = threadInterpExp l env in
                            List.iter ~f:(fun v -> printf "%d " v) vs;
                            printf "\n";
                            env'

and interpExp (e : exp) (env : table) : int * table =
  match e with
  | OpExp (e1, op, e2) -> let (v1, env') = interpExp e1 env in
                          let (v2, env'') = interpExp e2 env'
                          in (interpBinOp op v1 v2, env'')
  | EseqExp (s1, e2) -> interpExp e2 (interpStm s1 env)
  | IdExp x -> (lookup x env, env)
  | NumExp v -> (v, env)

and threadInterpExp (es : exp list) (env : table) : int list * table =
  match es with
  | [] -> ([], env)
  | hd :: tl -> let (v, env') = interpExp hd env in
                let (vs, env'') = threadInterpExp tl env' in
                (v :: vs, env'')

let expList = [NumExp 2;
               EseqExp(AssignStm("a", NumExp 5), IdExp "a");
               EseqExp(AssignStm("b", OpExp(IdExp "a", Plus, NumExp 1)), IdExp "b");
               IdExp "a"]
