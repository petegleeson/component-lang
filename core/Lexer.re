open Source;

exception
  LexerError({
    raw: string,
    loc: location,
    hint: option(string),
  });

let chars_from_filename = filename => {
  let in_channel = open_in(filename);
  let chars = ref([]);
  while (switch (input_char(in_channel)) {
         | exception End_of_file => false
         | c =>
           chars := [c, ...chars^];
           true;
         }) {
    ();
  };
  List.rev(chars^);
};

let token_from_char =
  Token.(
    (char, position) => {
      let (line, col) = position;
      let loc = (position, (line, col + 1));
      (
        switch (char) {
        | ('+' | '-' | '*' | '/') as op =>
          Some(
            Operator(
              loc,
              switch (op) {
              | '+' => Plus
              | '-' => Minus
              | '*' => Multiply
              | '/' => Divide
              | x =>
                raise(LexerError({loc, raw: Char.escaped(x), hint: None}))
              },
            ),
          )
        | ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') as num =>
          Some(Number(loc, Char.escaped(num)))
        | (
            'a' | 'A' | 'b' | 'B' | 'c' | 'C' | 'd' | 'D' | 'e' | 'E' | 'f' |
            'F' |
            'g' |
            'G' |
            'h' |
            'H' |
            'i' |
            'I' |
            'j' |
            'J' |
            'k' |
            'K' |
            'l' |
            'L' |
            'm' |
            'M' |
            'n' |
            'N' |
            'o' |
            'O' |
            'p' |
            'P' |
            'q' |
            'Q' |
            'r' |
            'R' |
            's' |
            'S' |
            't' |
            'T' |
            'u' |
            'U' |
            'v' |
            'V' |
            'w' |
            'W' |
            'x' |
            'X' |
            'y' |
            'Y' |
            'z' |
            'Z'
          ) as c =>
          Some(Identifier(loc, Char.escaped(c)))
        | ',' => Some(Comma(loc))
        | '=' => Some(Equals(loc))
        | ';' => Some(Semicolon(loc))
        | '{' => Some(LCurly(loc))
        | '}' => Some(RCurly(loc))
        | '(' => Some(LParen(loc))
        | ')' => Some(RParen(loc))
        | ' '
        | '\n' => None
        | x => raise(LexerError({loc, raw: Char.escaped(x), hint: None}))
        },
        switch (char) {
        | '\n' => (line + 1, 1)
        | _ => (line, col + 1)
        },
      );
    }
  );

let tokenise =
  Token.(
    chars => {
      chars
      |> List.fold_left(
           (((pending, tokens), position), char) => {
             let (token, next_position) = token_from_char(char, position);
             (
               switch (token, pending) {
               | (
                   Some(Number((_, finish), x)),
                   Some(Number((start, _), y)),
                 ) => (
                   Some(Number((start, finish), y ++ x)),
                   tokens,
                 )
               | (
                   Some(Identifier((_, finish), x)),
                   Some(Identifier((start, _), y)),
                 ) => (
                   Some(
                     switch (y ++ x) {
                     | "let" => Let((start, finish))
                     | _ => Identifier((start, finish), y ++ x)
                     },
                   ),
                   tokens,
                 )
               | (
                   Some(Identifier((_, finish), x)),
                   Some(Let((start, _))),
                 ) => (
                   Some(Identifier((start, finish), "let" ++ x)),
                   tokens,
                 )
               | (Some(t), Some(pt)) => (Some(t), [pt, ...tokens])
               | (Some(t), None) => (Some(t), tokens)
               | (None, Some(pt)) => (None, [pt, ...tokens])
               | (None, None) => (None, tokens)
               },
               next_position,
             );
           },
           ((None, []), (1, 1)),
         )
      |> (
        (((pt, tokens), _)) =>
          (
            switch (pt) {
            | Some(t) => [t, ...tokens]
            | _ => tokens
            }
          )
          |> List.rev
      );
    }
  );

let lex = filename => {
  switch (filename |> chars_from_filename |> tokenise) {
  | tokens => Ok(tokens)
  | exception (LexerError({hint, raw, loc})) =>
    Error(Error.{
      title: "An unexpected character prevented compilation",
      detail: "Character " ++ raw,
      stage: Lexing,
      filename,
      hint,
      loc,
    })
  };
};

