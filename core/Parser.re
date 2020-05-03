open Source;

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

type result('a) =
  | Success('a)
  | Failure(Error.t);

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

exception
  ParserError({
    loc: location,
    hint: option(string),
  });

let match_semicolon = env => switch(Env.peek(env)) {
  | Semicolon(_) => Env.eat(env)
  | _ => raise(
          ParserError({
            loc: Env.location(env),
            hint: Some("Expected a semi-colon"),
          }),
        )
}

let rec match_expression =
  Ast.Expression.(
    env => {
      let exp =
        switch (Env.peek(env)) {
        | Number(loc, raw) =>
          Env.eat(env);
          Int({loc, raw, kind: Int});
        | Identifier(loc, name) => switch(Env.lookahead_one(env)) {
          | Some(LParen(_)) => match_apply(env)
          | Some(_)
          | None => Identifier(match_identifier(env))
          }
        | LCurly(_) => Block(match_block(env))
        | LParen((start, _)) =>
          Env.eat(env);
          let rec match_params = () =>
            switch (Env.peek(env)) {
            | RParen(_) =>
              Env.eat(env);
              [];
            | Comma(_) => 
              Env.eat(env);
              let param = match_identifier(env);
              [param, ...match_params()];
            | _ => 
              let param = match_identifier(env);
              [param, ...match_params()];
            };
          let params = match_params();
          let body = match_block(env);
          Function({
            loc: (start, Source.get_end(body.loc)),
            kind: Var,
            params,
            body,
          });
        | _ =>
          raise(
            ParserError({
              loc: Env.location(env),
              hint: Some("Unexpected start of expression"),
            }),
          )
        };
      switch (Env.peek(env)) {
      | Operator(loc, operator) =>
        Env.eat(env);
        let left = exp;
        let right = match_expression(env);
        switch (right) {
        | BinaryOperator(right_node)
            when
              Token.precendence(right_node.operator)
              < Token.precendence(operator) =>
          BinaryOperator({
            loc: (
              get_start(Ast.get_location(left)),
              get_end(Ast.get_location(right_node.right)),
            ),
            kind: Int,
            operator: right_node.operator,
            left:
              BinaryOperator({
                loc: (
                  get_start(Ast.get_location(left)),
                  get_end(Ast.get_location(right_node.left)),
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
              get_start(Ast.get_location(left)),
              get_end(Ast.get_location(right)),
            ),
            kind: Int,
            operator,
            left,
            right,
          })
        };
      | _ => exp;
      };
    }
  )
and match_block = env => {
  let (start, _) = Env.location(env);
  Env.eat(env);
  let rec match_statements = () => {
    let exp = match_statement(env);
    switch (Env.peek(env)) {
    | RCurly(_) => [exp]
    | _ => [exp, ...match_statements()]
    };
  };
  let expressions = match_statements();
  switch (Env.peek(env)) {
  | RCurly((_, finish)) =>
    Env.eat(env);
    Ast.Block.{loc: (start, finish), expressions, kind: Var};
  | _ =>
    raise(
      ParserError({
        loc: Env.location(env),
        hint: Some("Expected \"}\" at end of block expression"),
      }),
    )
  };
}
and match_identifier = env => {
  switch (Env.peek(env)) {
  | Identifier(loc, name) =>
    Env.eat(env);
    Ast.Identifier.{loc, name, kind: Var};
  | _ =>
    raise(
      ParserError({
        loc: Env.location(env),
        hint: Some("Expected an identifier"),
      }),
    )
  };
}
and match_declaration = env => {
  switch (Env.peek(env)) {
  | Let((start, _)) =>
    Env.eat(env);
    let id = match_identifier(env);
    Env.eat(env); // @Improve expect "="
    let value = match_expression(env);
    Ast.Declaration.{
      loc: (start, Source.get_end(Ast.get_location(value))),
      kind: Void,
      id,
      value,
    };
  | _ =>
    raise(
      ParserError({
        loc: Env.location(env),
        hint: Some("Declarations must start with the \"let\" keyword"),
      }),
    )
  };
} and match_statement = Ast.Statement.(env => {
  let stmt = switch(Env.peek(env)){
  | Let(_) => Declaration(match_declaration(env))
  | _ => Expression(match_expression(env))
  }
  match_semicolon(env);
  stmt;
}) and match_apply = Ast.Apply.(env => {
  let func = match_identifier(env);
  Env.eat(env); // @Improve expect "("
  let rec match_args = () =>
    switch (Env.peek(env)) {
    | RParen((_, finish)) =>
      Env.eat(env);
      ([], finish);
    | Comma(_) => 
      Env.eat(env);
      let param = match_expression(env);
      let (rest, _) = match_args();
      ([param, ...rest], Source.get_end(Ast.get_location(param)))
    | _ => 
      let param = match_expression(env);
      let (rest, _) = match_args();
      ([param, ...rest], Source.get_end(Ast.get_location(param)))
    };
  let (args, finish) = match_args();
  Apply({
    loc: (Source.get_start(func.loc), finish),
    kind: Var,
    args,
    func,
  });

})

let match_program = env => {
  // @Note making this function tail call optimised causes an infinite loop
  let rec match_statements = () =>
    if (Env.has_more_tokens(env)) {
      let stmt = match_statement(env);
      [stmt, ...match_statements()];
    } else {
      [];
    };
  let (_, finish) = Env.location(env);
  Ast.Program.{loc: ((1, 1), finish), body: match_statements(), kind: Void};
};

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
    switch (match_program(Env.init(tokens))) {
    | program => Success(program)
    | exception (ParserError({hint, loc})) =>
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
