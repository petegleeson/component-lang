open Source;
open Ast;

let is_number = char => {
  let v = int_of_char(char);
  47 < v && v < 58;
};

let is_operator = char => {
  let v = int_of_char(char);
  41 < v && v < 48;
};

let is_character = char => {
  let v = int_of_char(char);
  64 < v && v < 91 || 96 < v && v < 123;
};

let is_whitespace = char => {
  char === ' ' || char === '\n';
};

exception
  LexerError({
    raw: string,
    loc: location,
    hint: option(string),
  });
exception
  ParserError({
    raw: string,
    loc: location,
    hint: option(string),
  });

type result('a) =
  | Success('a)
  | Failure(Error.t);

let rec generate_ast = tokens => {
  switch (tokens) {
  | [token, Token.Operator(loc, operator), ...rest] =>
    let (left, _) = generate_ast([token]);
    let (right, remaining) = generate_ast(rest);
    (
      switch (right) {
      | BinaryOperator(right_node)
          when
            Token.precendence(right_node.operator)
            < Token.precendence(operator) =>
        BinaryOperator({
          loc: (
            Source.get_start(get_location(left)),
            Source.get_end(get_location(right_node.right)),
          ),
          kind: Int,
          operator: right_node.operator,
          left:
            BinaryOperator({
              loc: (
                Source.get_start(get_location(left)),
                Source.get_end(get_location(right_node.left)),
              ),
              kind: Int,
              operator,
              left,
              right: right_node.left,
            }),
          right: right_node.right,
        })
      | _ =>
        BinaryOperator({
          loc: (
            Source.get_start(get_location(left)),
            Source.get_end(get_location(right)),
          ),
          kind: Int,
          operator,
          left,
          right,
        })
      },
      remaining,
    );
  | [Token.Number(loc, raw), ...rest] => (Int({loc, kind: Int, raw}), rest)
  | [token] =>
    raise(
      ParserError({loc: Token.get_location(token), raw: "", hint: None}),
    )
  | [token, ...rest] =>
    raise(
      ParserError({loc: Token.get_location(token), raw: "", hint: None}),
    )
  | _ => raise(ParserError({loc: ((1, 1), (1, 1)), raw: "", hint: None}))
  };
};

let rec generate_asts = tokens => {
  switch (generate_ast(tokens)) {
  | (ast, []) => [ast]
  | (ast, remaining) => List.cons(ast, generate_asts(remaining))
  };
};

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

let parse = filename => {
  let chars = chars_from_filename(filename);

  let lexer_result =
    switch (tokenise(chars)) {
    | tokens => Success(tokens)
    | exception (LexerError({hint, raw, loc})) =>
      Failure({
        title: "An unexpected character prevented compilation",
        detail: "Character " ++ raw,
        stage: Lexing,
        filename,
        hint,
        loc,
      })
    };

  switch (lexer_result) {
  | Success(tokens) =>
    switch (generate_asts(tokens)) {
    | body => Success(Program({loc: ((1, 1), (1, 1)), kind: Void, body}))
    | exception (ParserError({hint, raw, loc})) =>
      Failure({
        title: "A syntax error prevented compilation",
        detail: "Character",
        stage: Parsing,
        filename,
        hint,
        loc,
      })
    }
  | Failure(x) => Failure(x)
  };
};