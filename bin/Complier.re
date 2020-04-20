switch (Sys.argv) {
| [|_, filename|] =>
  filename
  |> Component_lang_core.Parser.parse
  |> Component_lang_core.Ast.show
  |> Console.log
| _ => print_endline("need to provide a source file")
};