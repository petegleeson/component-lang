exception
  TypeCheckError({
    loc: Source.location,
    hint: option(string),
  });

let rec kind_of_expression =
  Ast.Expression.(
    expr =>
      switch (expr) {
      | Apply({kind})
      | Function({kind})
      | Block({kind})
      | Int({kind})
      | BinaryOperator({kind})
      | Identifier({kind}) => kind
      }
  )
and kind_of_statement =
  Ast.Statement.(
    stmt =>
      switch (stmt) {
      | Expression(expr) => kind_of_expression(expr)
      | Declaration(decl) => decl.kind
      }
  );

let set_kind =
  Ast.Expression.(
    (kind, expr) =>
      switch (expr) {
      | Apply({loc, func, args}) => Apply({loc, kind, func, args})
      | BinaryOperator({loc, operator, left, right}) =>
        BinaryOperator({loc, kind, operator, left, right})
      | Block({loc, expressions, scope}) =>
        Block({loc, kind, expressions, scope})
      | Identifier({loc, name}) => Identifier({loc, kind, name})
      | Function({loc, params, body, scope}) =>
        Function({loc, kind, params, body, scope})
      | Int({loc, raw}) => Int({loc, kind, raw})
      }
  );

module Scope = {
  exception ScopeError(string);

  type scope;
  type stack = list(scope);

  let pop = stack => List.hd(stack);

  let push = (scope, stack) => [scope, ...stack];

  let rec lookup = (name, stack) =>
    switch (stack) {
    | [] => raise(ScopeError(Printf.sprintf("%s not found in scope", name)))
    | [current, ...rest] =>
      switch (Ast.Scope.find_opt(name, current)) {
      | Some(id) => id
      | None => lookup(name, rest)
      }
    };
};

module Subst = Map.Make(Int);

let rec apply_subst = (subst, kind: Ast.kind) =>
  switch (kind) {
  | Var(id) =>
    switch (Subst.find_opt(id, subst)) {
    | Some(k) => k
    | None => Ast.Var(id)
    }
  | Func(params, ret) =>
    Func(List.map(apply_subst(subst), params), apply_subst(subst, ret))
  | k => k
  };

let apply_subst_to_scope = (subst, scope) =>
  Ast.Scope.map(
    Ast.Identifier.(
      ({loc, name, kind}) => {loc, name, kind: apply_subst(subst, kind)}
    ),
    scope,
  );

exception UnifyError(string);

let compose_subst = (s, s') =>
  Subst.union(
    (_, a, _) =>
      raise(
        UnifyError(
          Printf.sprintf("Var already in subst %s", Ast.show_kind(a)),
        ),
      ),
    s,
    s',
  );

let rec unify = (a: Ast.kind, b: Ast.kind) =>
  switch (a, b) {
  | (Int, Int) => Subst.empty
  | (Void, Void) => Subst.empty
  | (Var(id), x)
  | (x, Var(id)) => Subst.add(id, x, Subst.empty)
  | (Func(params, return), Func(params', return')) =>
    List.fold_left2(
      (subst, p, p') => compose_subst(unify(p, p'), subst),
      Subst.empty,
      [return, ...params],
      [return', ...params'],
    )
  | (a, b) =>
    raise(
      UnifyError(
        Printf.sprintf(
          "Types do not unify: %s and %s",
          Ast.show_kind(a),
          Ast.show_kind(b),
        ),
      ),
    )
  };

let type_expression =
  Ast.Expression.(
    expr =>
      switch (expr) {
      | Apply({loc, kind, func, args}) => Apply({loc, kind, func, args})
      | e => e
      }
  );

let type_identifier = Ast.Identifier.(id => id);

let type_declaration =
  Ast.Declaration.(
    ({loc, id, value, kind}) => {
      let typed_id = type_identifier(id);
      let typed_value = type_expression(value);

      let subst = unify(typed_id.kind, kind_of_expression(typed_value));

      (
        {
          loc,
          id: {
            loc: typed_id.loc,
            name: typed_id.name,
            kind: apply_subst(subst, typed_id.kind),
          },
          value: typed_value,
          kind,
        },
        subst,
      );
    }
  );

let type_statement =
  Ast.Statement.(
    stmt =>
      switch (stmt) {
      | Expression(expr) => (
          Expression(type_expression(expr)),
          Subst.empty,
        )
      | Declaration(decl) =>
        let (typed_decl, subst) = type_declaration(decl);
        (Declaration(typed_decl), subst);
      }
  );

let type_program =
  Ast.Program.(
    ({loc, body, kind, scope}) => {
      let (typed_body, subst) =
        List.fold_left(
          ((stmts, subst), stmt) => {
            let (typed_stmt, stmt_subst) = type_statement(stmt);
            ([typed_stmt, ...stmts], compose_subst(subst, stmt_subst));
          },
          ([], Subst.empty),
          body,
        );
      {
        loc,
        body: List.rev(typed_body),
        kind,
        scope: apply_subst_to_scope(subst, scope),
      };
    }
  );

let type_check = (filename, program) =>
  switch (type_program(program)) {
  | typed_program => Ok(typed_program)
  | exception (TypeCheckError({loc, hint})) =>
    Error(
      Error.{
        title: "A type error prevented compilation",
        detail: "?",
        stage: TypeCheck,
        filename,
        hint,
        loc,
      },
    )
  };

/*
 let rec get_kind =
   Ast.(
     ast =>
       switch (ast) {
       | Int({kind})
       | BinaryOperator({kind})
       | Program({kind})
       | Unknown({kind}) => kind
       }
   );

 let unify =
   Ast.(
     (a: kind, b: kind) => (
       switch (a, b) {
       | (x, Var) => x
       | (Var, x) => x
       | (Int, Int) => Int
       | _ => Error
       }: kind
     )
   ) */
// let rec type_check =
//   Ast.(
//     ast =>
//       switch (ast) {
//       | Int({loc, raw}) => Int({loc, raw, kind: Int})
//       | BinaryOperator({loc, operator, left, right}) =>
//         BinaryOperator({
//           loc,
//           operator,
//           kind: unify(unify(Int, left.kind), unify(Int, right.kind)),
//           left,
//           right,
//         })
//       | Program({loc, kind, body}) =>
//         Program({loc, kind, body: List.map(type_check, [])})
//       | Unknown({loc}) => Unknown({loc, kind: Var})
/*       */