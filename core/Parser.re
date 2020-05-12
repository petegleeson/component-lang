open Source;

exception
  ParserError({
    loc: location,
    hint: option(string),
  });

module Env = {
  type t = {
    tokens: ref(list(Token.case)),
    scopes: ref(list(Ast.Scope.t(Ast.Identifier.t))),
    current_id: ref(int),
  };

  let init = tokens => {
    {
      tokens: ref(tokens),
      scopes: ref([Ast.Scope.empty]),
      current_id: ref(0),
    };
  };

  exception EnvError(string);

  let peek = ({tokens}) => {
    switch (tokens^) {
    | [t, ...rest] => t
    | _ => raise(EnvError("nothing left to peek"))
    };
  };

  let has_more_tokens = ({tokens}) => {
    switch (tokens^) {
    | [] => false
    | _ => true
    };
  };

  let eat = ({tokens}) => {
    tokens :=
      (
        switch (tokens^) {
        | [_, ...rest] => rest
        | _ => raise(EnvError("nothing left to eat"))
        }
      );
  };

  let location = env => env |> peek |> Token.get_location;

  let current_scope = ({scopes}) => List.hd(scopes^);

  let push_scope = ({scopes}) => {
    scopes := List.cons(Ast.Scope.empty, scopes^);
  };

  let pop_scope = ({scopes}) => {
    switch (scopes^) {
    | [] => raise(EnvError("no scope to pop"))
    | [s, ...rest] =>
      scopes := rest;
      s;
    };
  };

  let add_binding = (k, v, env) => {
    env.scopes :=
      (
        switch (env.scopes^) {
        | [] => raise(EnvError("no scopes to add binding to"))
        | [_, ...rest] => [
            Ast.Scope.(
              switch (mem(k, current_scope(env))) {
              | false => add(k, v, current_scope(env))
              | true =>
                raise(
                  ParserError({
                    loc: v.loc,
                    hint:
                      Some(
                        Printf.sprintf(
                          "Cannot declare variable \"%s\" multiple times in the same scope",
                          k,
                        ),
                      ),
                  }),
                )
              }
            ),
            ...rest,
          ]
        }
      );
  };

  let get_binding = (k, env) => {
    let rec find = scopes =>
      switch (scopes) {
      | [] => None
      | [curr, ...rest] =>
        switch (Ast.Scope.find_opt(k, curr)) {
        | None => find(rest)
        | some_id => some_id
        }
      };
    find(env.scopes^);
  };

  let expect = (fn, env) => {
    switch (fn(peek(env))) {
    | Ok(res) =>
      eat(env);
      res;
    | Error(msg) => raise(ParserError({loc: location(env), hint: msg}))
    };
  };

  let lookahead_one = ({tokens}) =>
    switch (tokens^) {
    | [] => None
    | [_] => None
    | [_, lh, ...rest] => Some(lh)
    };

  let next_id = ({current_id}) => {
    current_id := current_id^ + 1;
    current_id^;
  };
};

let match_semicolon = env =>
  switch (Env.peek(env)) {
  | Semicolon(_) => Env.eat(env)
  | _ =>
    raise(
      ParserError({
        loc: Env.location(env),
        hint: Some("Expected a semi-colon"),
      }),
    )
  };

let rec match_expression =
  Ast.Expression.(
    env => {
      let exp =
        switch (Env.peek(env)) {
        | Number(loc, raw) =>
          Env.eat(env);
          Int({loc, raw, kind: Int});
        | Identifier(loc, name) =>
          switch (Env.lookahead_one(env)) {
          | Some(LParen(_)) => match_apply(env)
          | Some(_)
          | None => Identifier(match_identifier(env))
          }
        | LCurly(_) => Block(match_block(env))
        | LParen((start, _)) => Function(match_function(env))
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
      | _ => exp
      };
    }
  )
and match_block = env => {
  let (start, _) = Env.location(env);
  Env.push_scope(env);
  Env.expect(
    fun
    | Token.LCurly(_) => Ok()
    | _ => Error(Some("Blocks must start with a \"{\"")),
    env,
  );
  let rec match_statements = () => {
    let stmt = match_statement(env);
    switch (Env.peek(env)) {
    | RCurly(_) => ([stmt], TypeCheck.kind_of_statement(stmt))
    | _ =>
      let (stmts, kind) = match_statements();
      ([stmt, ...stmts], kind);
    };
  };
  let (expressions, kind) = match_statements();
  switch (Env.peek(env)) {
  | RCurly((_, finish)) =>
    Env.eat(env);
    Ast.Block.{
      loc: (start, finish),
      expressions,
      kind,
      scope: Env.pop_scope(env),
    };
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
  Env.expect(
    fun
    | Identifier(loc, name) => {
        switch (Env.get_binding(name, env)) {
        | Some({kind}) =>
          let id = Ast.Identifier.{loc, name, kind};
          Ok(id);
        | None =>
          raise(
            ParserError({
              loc,
              hint:
                Some(
                  Printf.sprintf(
                    "Expected variable \"%s\" to be declared before use",
                    name,
                  ),
                ),
            }),
          )
        };
      }
    | _ => Error(Some("Expected a variable")),
    env,
  );
}
and match_new_identifier = env => {
  Env.expect(
    fun
    | Identifier(loc, name) => {
        let id = Ast.Identifier.{loc, name, kind: Var(Env.next_id(env))};
        Env.add_binding(id.name, id, env);
        Ok(id);
      }
    | _ => Error(Some("Expected a variable")),
    env,
  );
}
and match_declaration = env => {
  switch (Env.peek(env)) {
  | Let((start, _)) =>
    Env.eat(env);

    let id = match_new_identifier(env);

    Env.expect(
      fun
      | Token.Equals(_) => Ok()
      | _ => Error(Some("Expected \"=\" after the variable name")),
      env,
    );

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
}
and match_function =
  Ast.Function.(
    env => {
      let (start, _) = Env.location(env);
      Env.expect(
        fun
        | Token.LParen(_) => Ok()
        | _ =>
          Error(Some("Expected function definition to start with \"(\"")),
        env,
      );
      Env.push_scope(env);
      let rec match_params = () =>
        switch (Env.peek(env)) {
        | RParen(_) =>
          Env.eat(env);
          [];
        | Comma(_) =>
          Env.eat(env);
          let param = match_new_identifier(env);
          [param, ...match_params()];
        | _ =>
          let param = match_new_identifier(env);
          [param, ...match_params()];
        };
      let params = match_params();
      let body = match_block(env);
      Ast.Function.{
        loc: (start, Source.get_end(body.loc)),
        kind:
          Func(
            List.map(Ast.Identifier.(({kind}) => kind), params),
            body.kind,
          ),
        params,
        body,
        scope: Env.pop_scope(env),
      };
    }
  )
and match_statement =
  Ast.Statement.(
    env => {
      let stmt =
        switch (Env.peek(env)) {
        | Let(_) => Declaration(match_declaration(env))
        | _ => Expression(match_expression(env))
        };
      Env.expect(
        fun
        | Token.Semicolon(_) => Ok()
        | _ => Error(Some("Expected a \";\" after statement")),
        env,
      );
      stmt;
    }
  )
and match_apply =
  Ast.Apply.(
    env => {
      let func = match_identifier(env);
      Env.expect(
        fun
        | Token.LParen(_) => Ok()
        | _ => Error(Some("Expected \"(\" after function variable")),
        env,
      );
      let rec match_args = () =>
        switch (Env.peek(env)) {
        | RParen((_, finish)) =>
          Env.eat(env);
          ([], finish);
        | Comma(_) =>
          Env.eat(env);
          let param = match_expression(env);
          let (rest, _) = match_args();
          ([param, ...rest], Source.get_end(Ast.get_location(param)));
        | _ =>
          let param = match_expression(env);
          let (rest, _) = match_args();
          ([param, ...rest], Source.get_end(Ast.get_location(param)));
        };
      let (args, finish) = match_args();
      Apply({
        loc: (Source.get_start(func.loc), finish),
        kind: Var(Env.next_id(env)),
        args,
        func,
      });
    }
  );

let match_program = env => {
  // @Note making this function tail call optimised causes an infinite loop
  let rec match_statements = () =>
    if (Env.has_more_tokens(env)) {
      let stmt = match_statement(env);
      [stmt, ...match_statements()];
    } else {
      [];
    };
  // @Bug this gives the location of the first token
  let (_, finish) = Env.location(env);
  let body = match_statements();
  Ast.Program.{
    loc: ((1, 1), finish),
    body,
    kind: Void,
    scope: Env.current_scope(env),
  };
};

let parse = (filename, tokens) => {
  switch (match_program(Env.init(tokens))) {
  | program => Ok(program)
  | exception (ParserError({hint, loc})) =>
    Error(
      Error.{
        title: "A syntax error prevented compilation",
        detail: "Character",
        stage: Parsing,
        filename,
        hint,
        loc,
      },
    )
  };
};