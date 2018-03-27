let lexbuf = Lexing.from_channel !channel in
let ast = Parser.program Scanner.token lexbuf in match !action with
Ast -> print_string (Ast.string_of_program ast)
| _ -> let sast = Semant.check ast in (* Semantic checking *)
match !action with
Ast -> () (* Included for exhaustiveness *)
  | Sast    -> print_string (Sast.string_of_sprogram sast)
    (* Generate LLVM (even if it’s wrong) *)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule
                              (Codegen.translate sast))
    (* Generate and verify LLVM *)
  | Compile -> let m = Codegen.translate sast in
  Llvm_analysis.assert_valid_module m; (* useful built-in *) print_string
  (Llvm.string_of_llmodule m)
