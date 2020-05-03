switch (Sys.argv) {
| [|_, filename|] =>
  let parse_result = Component_lang_core.Parser.parse(filename);
  switch (parse_result) {
  | Success(program) =>
    Console.log(Component_lang_core.Ast.show_program(program))
  | Failure(err) =>
    err |> Component_lang_core.Error.error_message |> Console.log
  };
| _ => print_endline("need to provide a source file")
};