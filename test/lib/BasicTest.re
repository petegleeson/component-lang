/* MyFirstTest.re */
open TestFramework;
open Component_lang_core;

/*
switch (Sys.argv) {
| [|_, filename|] =>
  Result.(
    bind(Lexer.lex(filename), Parser.parse(filename))
    |> (
      result =>
        bind(result, TypeCheck.type_check(filename))
        |> fold(
             ~ok=CodeGen.gen_program(filename),
             ~error=Error.error_message,
           )
        // |> fold(~ok=Ast.show_program, ~error=Error.error_message)
        |> Console.log
    )
  )
| _ => print_endline("need to provide a source file")
};
*/

let gen_ir = filename => Result.(
  bind(Lexer.lex(filename), Parser.parse(filename))
    |> (
      result =>
        bind(result, TypeCheck.type_check(filename))
        |> fold(
             ~ok=CodeGen.gen_program(filename),
             ~error=Error.error_message,
           )
    )
)

describe("basic tests", ({test, _}) => {
  test("function that returns an int", ({expect, _}) => {
    expect.string(gen_ir("test/lib/input/int-fn")).toMatchSnapshot();
  });
});
