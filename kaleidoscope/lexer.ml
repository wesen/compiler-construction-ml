let rec lex =
  parser
  (* whitespace *)
  | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lex stream

  (* identifier *)
  | [< ' ('A' .. 'Z' | 'a' .. 'z' as c); stream >] ->
     let buffer = Buffer.create 1 in
     Buffer.add_char buffer c;
     lex_ident buffer stream

  (* number *)
  | [< ' ('0' .. '9' as c); stream >] ->
     let buffer = Buffer.create 1 in
     Buffer.add_char buffer c;
     lex_number buffer stream

  (* comment until EOL *)
  | [< ' ('#'); stream >] ->
     lex_comment stream

  (* single char *)
  | [< 'c_; stream >] ->
     [< 'Token.Kwd c_; lex stream >]

  (* end of stream *)
  | [< >] -> [< >]

and lex_number buffer =
  parser
  | [< ' ('0' .. '9' | '.' as c); stream >] ->
     Buffer.add_char buffer c;
     lex_number buffer stream
  | [< stream=lex >] ->
     [< 'Token.Number (float_of_string (Buffer.contents buffer)); stream >]

and lex_ident buffer =
  parser
  | [< ' ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' as c); stream >] ->
     Buffer.add_char buffer c;
     lex_ident buffer stream
  | [< stream=lex >] ->
     match Buffer.contents buffer with
     | "def" -> [< 'Token.Def; stream >]
     | "extern" -> [< 'Token.Extern; stream >]
     | id -> [< 'Token.Ident id; stream >]

and lex_comment =
  parser
  | [< ' ('\n'); stream=lex >] -> stream
  | [< '_; e=lex_comment >] -> e
  | [< >] -> [< >]
