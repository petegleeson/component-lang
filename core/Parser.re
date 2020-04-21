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

exception LexerError(string);
exception ParserError(string);

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
  | _ => raise(ParserError("unexpected token"))
  };
};

let rec generate_asts = tokens => {
  switch (generate_ast(tokens)) {
  | (ast, []) => [ast]
  | (ast, remaining) => List.cons(ast, generate_asts(remaining))
  };
};

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

  let tokens = {
    let result = ref([]);
    Stream.iter(
      item => {
        let rec tokenise = ((point, current), next) => {
          let (line, col) = point;
          if (is_number(current)) {
            Some(
              switch (next) {
              | Some(((next_line, next_col), lookahead))
                  when is_number(lookahead) =>
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
                    LexerError(
                      Printf.sprintf(
                        "expected number on line %i and column %i",
                        next_line,
                        next_col,
                      ),
                    ),
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
              | _ => Token.Invalid(loc, Char.escaped(current))
              },
            );
          } else {
            None;
          };
        };
        switch (tokenise(item, Stream.peek(char_stream))) {
        | Some(t) => result := [t, ...result^]
        | None => ()
        };
      },
      char_stream,
    );
    List.rev(result.contents);
  };

  Program({
    loc: ((1, 1), position.contents),
    kind: Void,
    body: generate_asts(tokens),
  });
};