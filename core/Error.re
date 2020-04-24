type stage =
  | Lexing
  | Parsing;

type t = {
  detail: string,
  filename: string,
  hint: option(string),
  loc: Source.location,
  stage,
  title: string,
};

module FCP =
  FileContextPrinter.Make({
    let config =
      FileContextPrinter.Config.initialize({linesBefore: 3, linesAfter: 3});
  });

let error_message = ({detail, filename, hint, loc, stage, title}) => {
  let ((line, col), _) = loc;
  <Pastel>
    <Pastel color=Red>
      {title ++ " of "}
      <Pastel underline=true> filename </Pastel>
    </Pastel>
    "\n\n"
    {switch (FCP.printFile(filename, loc)) {
     | Some(lines) => lines ++ "\n\n"
     | _ => ""
     }}
    <Pastel color=Red>
      detail
      " on line "
      <Pastel bold=true> {string_of_int(line)} </Pastel>
      " and position "
      <Pastel bold=true> {string_of_int(col)} </Pastel>
      " stopped compilation."
    </Pastel>
    {switch (hint) {
     | Some(msg) => "\n\n" ++ msg
     | _ => ""
     }}
  </Pastel>;
};