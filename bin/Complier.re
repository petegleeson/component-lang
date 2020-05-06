open Component_lang_core;

switch (Sys.argv) {
| [|_, filename|] =>
  Result.(
    bind(Lexer.lex(filename), Parser.parse(filename))
    |> fold(~ok=Ast.show_program, ~error=Error.error_message)
    |> Console.log
  )
| _ => print_endline("need to provide a source file")
};