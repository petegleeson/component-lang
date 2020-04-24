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
  | _ => raise(ParserError({loc: ((1, 1), (1, 1)), raw: "", hint: None}))
  };
};

let rec generate_asts = tokens => {
  switch (generate_ast(tokens)) {
  | (ast, []) => [ast]
  | (ast, remaining) => List.cons(ast, generate_asts(remaining))
  };
};

let parse = filename => {
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

  let lexer_result =
    try({
      let result = ref([]);
      Stream.iter(
        item => {
          let rec tokenise = ((point, current), next) => {
            let (line, col) = point;
            if (is_number(current)) {
              Some(
                switch (next) {
                | Some((lookaheadPoint, lookahead)) when is_number(lookahead) =>
                  switch (
                    tokenise(
                      Stream.next(char_stream),
                      Stream.peek(char_stream),
                    )
                  ) {
                  | Some(Token.Number((_, fin), value)) =>
                    Number((point, fin), Char.escaped(current) ++ value)
                  | _ =>
                    raise(
                      LexerError({
                        loc: (point, lookaheadPoint),
                        raw: Char.escaped(lookahead),
                        hint: Some("Expected a number"),
                      }),
                    )
                  }
                | _ =>
                  Token.Number(
                    (point, (line, col + 1)),
                    Char.escaped(current),
                  )
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
                | _ =>
                  raise(
                    LexerError({
                      loc,
                      raw: Char.escaped(current),
                      hint: None,
                    }),
                  )
                },
              );
            } else if (is_whitespace(current)) {
              None;
            } else {
              let loc = (point, (line, col + 1));
              raise(
                LexerError({loc, raw: Char.escaped(current), hint: None}),
              );
            };
          };
          switch (tokenise(item, Stream.peek(char_stream))) {
          | Some(t) => result := [t, ...result^]
          | None => ()
          };
        },
        char_stream,
      );
      Success(List.rev(result.contents));
    }) {
    | LexerError({hint, raw, loc}) =>
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
    | body =>
      Success(
        Program({loc: ((1, 1), position.contents), kind: Void, body}),
      )
    | exception (ParserError({hint, raw, loc})) =>
      Failure({
        title: "An unexpected character prevented compilation",
        detail: "Character " ++ raw,
        stage: Parsing,
        filename,
        hint,
        loc,
      })
    }
  | Failure(x) => Failure(x)
  };
};