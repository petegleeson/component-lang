switch (Sys.argv) {
| [|_, filename|] =>
  filename
  |> open_in
  |> Stream.of_channel
  |> Component_lang_core.Parser.parse
  |> Console.log
| _ => print_endline("need to provide a source file")
};