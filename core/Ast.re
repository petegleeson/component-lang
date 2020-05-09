open Source;

[@deriving show]
type kind =
  | Int
  | Error
  | Var(int)
  | Void;

module Scope = Map.Make(String);

module rec Apply: {
  type t = {
    loc: location,
    kind,
    func: Identifier.t,
    args: list(Expression.t),
  };
} = Apply
and BinaryOperator: {
  type t = {
    loc: location,
    operator: Token.operator,
    left: Expression.t,
    right: Expression.t,
    kind,
  };
} = BinaryOperator
and Block: {
  type t = {
    loc: location,
    expressions: list(Statement.t),
    kind,
    scope: Scope.t(Identifier.t),
  };
} = Block
and Declaration: {
  type t = {
    loc: location,
    id: Identifier.t,
    value: Expression.t,
    kind,
  };
} = Declaration
and Identifier: {
  type t = {
    loc: location,
    name: string,
    kind,
  };
} = Identifier
and Expression: {
  type t =
    | Apply(Apply.t)
    | BinaryOperator(BinaryOperator.t)
    | Block(Block.t)
    | Identifier(Identifier.t)
    | Function(Function.t)
    | Int(Int.t);
} = Expression
and Function: {
  type t = {
    loc: location,
    params: list(Identifier.t),
    body: Block.t,
    kind,
    scope: Scope.t(Identifier.t),
  };
} = Function
and Int: {
  type t = {
    loc: location,
    raw: string,
    kind,
  };
} = Int
and Statement: {
  type t =
    | Expression(Expression.t)
    | Declaration(Declaration.t);
} = Statement
and Program: {
  type t = {
    loc: location,
    body: list(Statement.t),
    kind,
    scope: Scope.t(Identifier.t),
  };
} = Program;

let rec show_expression =
  Expression.(
    exp =>
      switch (exp) {
      | Apply({loc, kind, func, args}) =>
        Format.sprintf(
          "@[<2>Apply.{@ loc: %s@ kind: %s@ func: %s@ args: [@ %s ]@ }@]@.",
          Source.show_location(loc),
          show_kind(kind),
          show_identifier(func),
          List.fold_left(
            (res, exp) => res ++ show_expression(exp) ++ ", ",
            "",
            args,
          ),
        )
      | Int({loc, kind, raw}) =>
        Format.sprintf(
          "@[<2>Int.{@ loc: %s@ kind: %s@ raw: \"%s\"@ }@]@.",
          Source.show_location(loc),
          show_kind(kind),
          raw,
        )
      | Identifier(id) => show_identifier(id)
      | BinaryOperator({loc, kind, operator, left, right}) =>
        Format.sprintf(
          "@[<2>BinOp.{@ loc: %s@ kind: %s@ operator: %s@ left: %s@ right: %s@ }@]@.",
          Source.show_location(loc),
          show_kind(kind),
          Token.show_operator(operator),
          show_expression(left),
          show_expression(right),
        )
      | Block({loc, kind, expressions}) =>
        Format.sprintf(
          "@[<2>Block.{@ loc: %s@ kind: %s@ expressions: [@ %s ]@ }@]@.",
          Source.show_location(loc),
          show_kind(kind),
          List.fold_left(
            (res, exp) => res ++ show_statement(exp) ++ ", ",
            "",
            expressions,
          ),
        )
      | Function({loc, kind, params, body}) =>
        Format.sprintf(
          "@[<2>Function.{@ loc: %s@ kind: %s@ params: [@ %s ]@ body: %s@ }@]@.",
          Source.show_location(loc),
          show_kind(kind),
          List.fold_left(
            (res, exp) => res ++ show_identifier(exp) ++ ", ",
            "",
            params,
          ),
          show_expression(Block(body)),
        )
      }
  )
and show_declaration =
  Declaration.(
    ({loc, kind, id, value}) =>
      Format.sprintf(
        "@[<2>Declaration.{@ loc: %s@ kind: %s@ id: %s@ value: %s@ }@]@.",
        Source.show_location(loc),
        show_kind(kind),
        show_expression(Identifier(id)),
        show_expression(value),
      )
  )
and show_statement =
  Statement.(
    stmt =>
      switch (stmt) {
      | Expression(expr) => show_expression(expr)
      | Declaration(decl) => show_declaration(decl)
      }
  )
and show_identifier =
  Identifier.(
    ({loc, kind, name}) =>
      Format.sprintf(
        "@[<2>Identifier.{@ loc: %s@ kind: %s@ name: \"%s\"@ }@]@.",
        Source.show_location(loc),
        show_kind(kind),
        name,
      )
  );

let show_program =
  Program.(
    ({loc, kind, body}) =>
      Format.sprintf(
        "@[<2>Program.{@ loc: %s@ kind: %s@ statements: [@ %s ]@ }@]@.",
        Source.show_location(loc),
        show_kind(kind),
        List.fold_left(
          (res, stmt) => res ++ show_statement(stmt) ++ ", ",
          "",
          body,
        ),
      )
  );

let get_location =
  Expression.(
    expression =>
      switch (expression) {
      | Apply({loc})
      | Int({loc})
      | Block({loc})
      | Function({loc})
      | Identifier({loc})
      | BinaryOperator({loc}) => loc
      }
  );
