open Easy_format

let list =
  { list with
    list_style = Some "list";
    opening_style = Some "op";
    body_style = Some "body";
    separator_style = Some "sep";
    closing_style = Some "cl"
  }

let atom = { atom_style = Some "atom" }
let label = { label with label_style = Some "label" }

let tuple_param =
  { list with
    space_after_opening = false;
    space_before_closing = false;
    align_closing = false
  }

let operator_param =
  { list with
    space_after_opening = false;
    space_before_closing = false;
    separators_stick_left = false;
    space_before_separator = true;
    space_after_separator = true;
    align_closing = true
  }

let html_escape_string s =
  let buf = Buffer.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
      '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let html_escape = `Escape_string html_escape_string
let html_style = [
  "atom", { tag_open = "<a>"; tag_close = "</a>" };
  "body", { tag_open = "<lb>"; tag_close = "</lb>" };
  "list", { tag_open = "<l>"; tag_close = "</l>" };
  "op", { tag_open = "<op>"; tag_close = "</op>" };
  "cl", { tag_open = "<cl>"; tag_close = "</cl>" };
  "sep", { tag_open = "<sep>"; tag_close = "</sep>" };
  "label", { tag_open = "<la>"; tag_close = "</la>" };
]

let format_tuple f l =
  List (("(", ",", ")", tuple_param), List.map f l)

let format_int x =
  Atom (string_of_int x, atom)

let format_float x =
  Atom (Printf.sprintf "%.5f" x, atom)

let format_sum ?(wrap = `Wrap_atoms) l =
  List (("(", "+", ")", { operator_param with wrap_body = wrap }),
        List.map format_int l)

let format_array ~align_closing ~wrap f a =
  let l = Array.to_list (Array.map f a) in
  List (("[|", ";", "|]",
         { list with
           align_closing = align_closing;
           wrap_body = wrap }),
        l)

let format_matrix
    ?(align_closing1 = true)
    ?(align_closing2 = true)
    ?(wrap1 = `Wrap_atoms)
    ?(wrap2 = `Wrap_atoms)
    m =
  format_array ~align_closing: align_closing1 ~wrap: wrap1
    (format_array ~align_closing: align_closing2 ~wrap: wrap2 format_float) m

let format_record f l0 =
  let l = List.map (fun (s, x) -> Label ((Atom (s ^ ":", atom), label), f x)) l0
  in List (("{", ";", "}", list), l)

let begin_style =
  { label with indent_after_label = 0 },
  ("begin", ";", "end",
   { list with stick_to_label = false })

let curly_style = label, ("{", ";", "}", list)

let format_function_definition (body_label, body_param) name param body =
  Label (
    (
      Label (
        (Atom ("function " ^ name, atom), label),
        List (("(", ",", ")", tuple_param),
              List.map (fun s -> Atom (s, atom)) param)
      ),
      body_label
    ),
    List (body_param, List.map (fun s -> Atom (s, atom)) body)
  )

let print_margin fmt () =
  let margin = Format.pp_get_margin fmt () in
  for i = 1 to margin do
    print_char '+'
  done;
  print_newline ()

let with_margin ?(html = false) margin f x =
  let fmt = Format.formatter_of_out_channel stdout in
  Format.pp_set_margin fmt margin;
  if html then
    Pretty.define_styles fmt html_escape html_style;
  print_margin fmt ();
  f fmt x;
  Format.pp_print_flush fmt ();
  print_newline ()

let print s =
  Printf.printf "\n*** %s ***\n%!" s

let print_tuple fmt l =
  Pretty.to_formatter fmt (format_tuple format_int l)

let print_sum ?wrap fmt l =
  Pretty.to_formatter fmt (format_sum ?wrap l)

let print_matrix ?align_closing1 ?align_closing2 ?wrap1 ?wrap2 m fmt () =
  Pretty.to_formatter fmt
    (format_matrix ?align_closing1 ?align_closing2 ?wrap1 ?wrap2 m)

let print_function_definition style name param fmt body =
  Pretty.to_formatter fmt (format_function_definition style name param body)
