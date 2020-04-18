open Source;
open Ast;

let is_number = char => {
  let v = int_of_char(char);
  47 < v && v < 58;
};

let is_operator = char => {
  let v = int_of_char(char);
  47 < v && v < 58;
};

let isCharacter = char => {
  let v = int_of_char(char);
  64 < v && v < 91 || 96 < v && v < 123;
};

exception LexerError(string);

let parse = (filename: string) => {
  let in_channel = open_in(filename);
  let position: ref(point) = ref((1, 1));
  let char_stream =
    Stream.from(_ =>
      try({
        let (line, column) = position.contents;
        let char = input_char(in_channel);
        let result = Some(((line, column), char));
        position.contents = (
          switch (char) {
          | '\n' => (line + 1, 1)
          | _ => (line, column + 1)
          }
        );
        result;
      }) {
      | End_of_file =>
        close_in(in_channel);
        None;
      }
    );
  let token_stream =
    Stream.from(_ => {
      let rec tokenise = ((point, current), next) => {
        let (line, col) = point;
        if (is_number(current)) {
          Some(
            switch (next) {
            | Some(((next_line, next_col), lookahead))
                when is_number(lookahead) =>
              switch (
                tokenise(Stream.next(char_stream), Stream.peek(char_stream))
              ) {
              | Some(Token.Number((_, fin), value)) =>
                Number((point, fin), Char.escaped(current) ++ value)
              | _ =>
                raise(
                  LexerError(
                    Printf.sprintf(
                      "expected number on line %i and column %i",
                      next_line,
                      next_col,
                    ),
                  ),
                )
              }
            | _ => Number((point, (line, col + 1)), Char.escaped(current))
            },
          );
        } else if (is_operator(current)) {
          let loc = (point, (line, col + 1));
          Some(
            switch (current) {
            | '+' => Token.Operator(loc, Plus)
            | '-' => Token.Operator(loc, Minus)
            | '*' => Token.Operator(loc, Multiply)
            | '/' => Token.Operator(loc, Divide)
            | _ => Token.Invalid(loc, Char.escaped(current))
            },
          );
        } else {
          None;
        };
      };

      tokenise(Stream.next(char_stream), Stream.peek(char_stream));
    });

  let rec generate_ast = (current, lookahead) => {
    switch (current, lookahead) {
    | (token, Some(Token.Operator(loc, operator))) =>
      let left = generate_ast(token, None);
      Stream.junk(token_stream);
      let right =
        generate_ast(Stream.next(token_stream), Stream.peek(token_stream));
      BinaryOperator({loc: get_location(left), operator, left, right});
    | (Token.Number(loc, raw), _) => Int({loc, raw})
    | _ => Unknown({loc: Token.get_location(current)})
    };
  };

  let rec generate_program = () => {
    switch (Stream.peek(token_stream)) {
    | None => []
    | Some(token) => [
        generate_ast(Stream.next(token_stream), Stream.peek(token_stream)),
        ...generate_program(),
      ]
    };
  };

  Program({loc: ((1, 1), position.contents), body: generate_program()});
};