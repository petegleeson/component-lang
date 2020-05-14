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

let show_subst = s =>
  Subst.fold(
    (k, v, str) => Printf.sprintf("%s %i: %s", str, k, Ast.show_kind(v)),
    s,
    "",
  );

let rec apply_subst = (subst, kind: Ast.kind) =>
  switch (kind) {
  | Var(id) =>
    switch (Subst.find_opt(id, subst)) {
    | Some(k) => apply_subst(subst, k)
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

let apply_subst_to_scopes = (subst, scopes) =>
  List.map(apply_subst_to_scope(subst), scopes);

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

// @Notsure - think about how to represent the "expected" type
let rec unify = (a: Ast.kind, b: Ast.kind) =>
  switch (a, b) {
  | (Int, Int) => Subst.empty
  | (Void, Void) => Subst.empty
  | (Func(params, return), Func(params', return')) =>
    List.fold_left2(
      (subst, p, p') => compose_subst(unify(p, p'), subst),
      Subst.empty,
      [return, ...params],
      [return', ...params'],
    )
  | (Var(id), Var(id')) when id === id' => Subst.empty
  | (Var(id), x)
  | (x, Var(id)) => Subst.add(id, x, Subst.empty)
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

let type_identifier =
  Ast.Identifier.(
    ({loc, name, kind}, scope) => {
      let subst = unify(kind, Scope.lookup(name, scope).kind);
      ({loc, name, kind: apply_subst(subst, kind)}, subst);
    }
  );

let rec type_expression =
  Ast.Expression.(
    (expr, scopes) =>
      switch (expr) {
      | Apply({loc, kind, func, args}) =>
        let (typed_func, func_subst) = type_identifier(func, scopes);
        let (typed_args, args_subst) =
          List.fold_left(
            ((exprs, subst), arg) => {
              let (typed_expr, expr_subst) =
                type_expression(arg, apply_subst_to_scopes(subst, scopes));
              ([typed_expr, ...exprs], compose_subst(subst, expr_subst));
            },
            ([], func_subst),
            args,
          );

        let subst =
          Ast.(
            unify(
              Func(List.map(e => kind_of_expression(e), typed_args), kind),
              typed_func.kind,
            )
          );

        (
          Apply({
            loc,
            kind: apply_subst(subst, kind),
            func: typed_func,
            args: typed_args,
          }),
          // @Incomplete should return subst - need to instanciate typed_func.kind
          Subst.empty,
        );
      | BinaryOperator({loc, kind, operator, left, right}) =>
        let (typed_left, left_subst) = type_expression(left, scopes);
        let (typed_right, right_subst) =
          type_expression(right, apply_subst_to_scopes(left_subst, scopes));

        let kind_left = kind_of_expression(typed_left);
        let kind_right = kind_of_expression(typed_right);

        let subst_binop_left = unify(kind_left, Ast.Int);
        let subst_binop_right = unify(kind_right, Ast.Int);

        (
          BinaryOperator({
            loc,
            kind,
            operator,
            left:
              kind_left
              |> apply_subst(subst_binop_left)
              |> (s => set_kind(s, typed_left)),
            right:
              kind_right
              |> apply_subst(subst_binop_right)
              |> (s => set_kind(s, typed_right)),
          }),
          compose_subst(subst_binop_left, subst_binop_right),
        );
      | Block(block) =>
        let (typed_block, subst) = type_block(block, scopes);
        (Block(typed_block), subst);
      | Function({loc, kind, params, body, scope}) =>
        let (typed_params, params_subst) =
          List.fold_left(
            ((exprs, subst), param) => {
              let (typed_param, param_subst) =
                type_identifier(
                  param,
                  apply_subst_to_scopes(subst, [scope, ...scopes]),
                );
              ([typed_param, ...exprs], compose_subst(subst, param_subst));
            },
            ([], Subst.empty),
            params,
          );
        let (typed_body, body_subst) = type_block(body, [scope, ...scopes]);
        let func_subst =
          unify(
            kind,
            Ast.Func(
              List.map((id: Ast.Identifier.t) => id.kind, typed_params),
              typed_body.kind,
            ),
          );

        let all_subst = compose_subst(body_subst, func_subst);
        (
          Function({
            loc,
            kind: apply_subst(all_subst, kind),
            params:
              List.map(
                Ast.Identifier.(
                  ({loc, name, kind}) => {
                    loc,
                    name,
                    kind: apply_subst(all_subst, kind),
                  }
                ),
                typed_params,
              ),
            body: typed_body,
            scope: apply_subst_to_scope(all_subst, scope),
          }),
          all_subst,
        );
      | Identifier(id) =>
        let (typed_id, subst) = type_identifier(id, scopes);
        (Identifier(typed_id), subst);
      | Int(x) => (Int(x), Subst.empty)
      }
  )
and type_block =
  Ast.Block.(
    ({loc, kind, expressions, scope}, scopes) => {
      let (typed_stmts, subst) =
        List.fold_left(
          ((stmts, subst), stmt) => {
            let (typed_stmt, stmt_subst) =
              type_statement(
                stmt,
                apply_subst_to_scopes(subst, [scope, ...scopes]),
              );
            ([typed_stmt, ...stmts], compose_subst(subst, stmt_subst));
          },
          ([], Subst.empty),
          expressions,
        );
      (
        {
          loc,
          kind: apply_subst(subst, kind),
          expressions: typed_stmts,
          scope: apply_subst_to_scope(subst, scope),
        },
        subst,
      );
    }
  )
and type_statement =
  Ast.Statement.(
    (stmt, scope) =>
      switch (stmt) {
      | Expression(expr) =>
        let (typed_expr, subst) = type_expression(expr, scope);
        (Expression(typed_expr), subst);
      | Declaration(decl) =>
        let (typed_decl, subst) = type_declaration(decl, scope);
        (Declaration(typed_decl), subst);
      }
  )
and type_declaration =
  Ast.Declaration.(
    ({loc, id, value, kind}, scope) => {
      let (typed_id, subst_id) = type_identifier(id, scope);
      let (typed_value, expr_subst) =
        type_expression(value, apply_subst_to_scopes(subst_id, scope));

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
        compose_subst(subst, expr_subst),
      );
    }
  );

let type_program =
  Ast.Program.(
    ({loc, body, kind, scope}) => {
      let (typed_body, subst) =
        List.fold_left(
          ((stmts, subst), stmt) => {
            let (typed_stmt, stmt_subst) =
              type_statement(stmt, [apply_subst_to_scope(subst, scope)]);
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