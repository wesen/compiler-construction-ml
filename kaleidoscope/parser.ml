let binop_precedence : (char, int) Hashtbl.t = Hashtbl.create 10

let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1

let rec parse_primary =
  parser
(* numberexpr ::= number *)
| [< 'Token.Number n >] -> Ast.Number n
(* parenexpr ::= '(' expression ')' *)
| [< 'Token.Kwd '('; e=parse_expr; 'Token.Kwd ')' ?? "expected ')'" >] -> e
  (* identifierexpr ::= identifier
     :: = identifier '(' argumentexpr ')' *)
| [< 'Token.Ident id; stream >] ->
  let rec parse_args accumulator =
    parser
  | [< e=parse_expr; stream >] ->
    begin parser
        | [< 'Token.Kwd ','; e=parse_args (e :: accumulator) >] -> e
        | [< >] -> e :: accumulator
    end stream
  | [< >] -> accumulator
  in
  let rec parse_ident id =
    parser
  (* Call *)
  | [< 'Token.Kwd '(';
       args=parse_args [];
       'Token.Kwd ')' ?? "expected ')'">] ->
    Ast.Call (id, Array.of_list (List.rev args))

(* Simple variable ref. *)
  | [< >] -> Ast.Variable id
  in
  parse_ident id stream
| [< >] -> raise (Stream.Error "Unknown token when expecting an expression.")

and parse_bin_rhs expr_prec lhs stream =
  (* binoprhs ::= ('+' primary)* *)
  match Stream.peek stream with
  (* If this is a binop, find its precedence *)
  | Some (Token.Kwd c) when Hashtbl.mem binop_precedence c ->
    let token_prec = precedence c in

    (* If this is a binop that binds at least as tightly as the current binop,
     * consume it, otherwise we are done. *)
    if token_prec < expr_prec then lhs else begin
      (* Eat the binop *)
      Stream.junk stream;

      (* Parse the primary expression after the binary operator. *)
      let rhs = parse_primary stream in

      (* We now know it is a binop *)
      let rhs =
        match Stream.peek stream with
        | Some (Token.Kwd c2) ->
          (* If BinOp binds less tightly with rhs than the operator after
           * rhs, let the pending operator take rhs as its lh. *)
          let next_prec = precedence c2 in
          if token_prec < next_prec
          then parse_bin_rhs (token_prec + 1) rhs stream
          else rhs
        | _ -> rhs
      in
      let lhs = Ast.Binary (c, lhs, rhs) in
      parse_bin_rhs expr_prec lhs stream
    end
  | _ -> lhs

and parse_expr =
  parser
    (* expression
     * ::= primary binoprhs *)
| [< lhs=parse_primary; stream >] -> parse_bin_rhs 0 lhs stream

let parse_prototype =
  let rec parse_args accumulator =
    parser
  | [< 'Token.Ident id; e=parse_args (id :: accumulator) >] -> e
  | [< >] -> accumulator
  in

  parser
| [< 'Token.Ident id;
     'Token.Kwd '(' ?? "expected '(' in prototype";
     args = parse_args [];
     'Token.Kwd ')' ?? "expected ')' in prototype" >] ->
  (* success. *)
  Ast.Prototype (id, Array.of_list (List.rev args))

| [< >] ->
  raise (Stream.Error "expected function name in prototype")

let parse_definition =
  parser
| [< 'Token.Def; p=parse_prototype; e=parse_expr >] ->
  Ast.Function (p, e)

let parse_extern =
  parser
| [< 'Token.Extern; e=parse_prototype >] -> e

let parse_toplevel =
  parser
| [< e=parse_expr >] ->
  (* Make an anonymous proto. *)
  Ast.Function (Ast.Prototype ("", [||]), e)
