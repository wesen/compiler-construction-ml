open Llvm

exception Error of string

let context = global_context ()
    (* LLVM construct that contains all of the functions and global variables
     * in a chunk of code *)
let the_module = create_module context "my cool jit"
(* helper object to generate LLVM values *)
let builder = builder context
(* symbol table for the code. The only thing that can be referenced
 * in this form of Kaleidoscope are function parameters. *)
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let rec codegen_expr = function
  | Ast.Number n -> const_float double_type n
  | Ast.Variable name -> (try Hashtbl.find named_values name with
      | Not_found -> raise (Error "unknown variable name"))
  | Ast.Binary (op, lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin
      match op with
      | '+' -> build_fadd lhs_val rhs_val "addtmp" builder
      | '-' -> build_fsub lhs_val rhs_val "subtmp" builder
      | '*' -> build_fmul lhs_val rhs_val "multmp" builder
      | '<' ->
        (* convert bool to 0/1 to double 0.0 or 1.0 *)
        let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
        build_uitofp i double_type "booltmp" builder
      | _ -> raise (Error "invalid binary operator")
    end
  | _ -> raise (Error "foobar")
