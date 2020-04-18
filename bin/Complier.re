switch (Sys.argv) {
| [|_, filename|] =>
  filename |> Component_lang_core.Parser.parse |> Console.log
| _ => print_endline("need to provide a source file")
};