open Source;

[@deriving show]
type kind =
  | Int
  | Error
  | Var
  | Void;

module rec BinaryOperator: {
  type t = {
    loc: location,
    operator: Token.operator,
    left: Expression.t,
    right: Expression.t,
    kind,
  };
} = BinaryOperator
and Expression: {
  type t =
    | BinaryOperator(BinaryOperator.t)
    | Int(Int.t);
} = Expression
and Int: {
  type t = {
    loc: location,
    raw: string,
    kind,
  };
} = Int
and Program: {
  type t = {
    loc: location,
    body: list(Expression.t),
    kind,
  };
} = Program;

let get_location =
  Expression.(
    expression =>
      switch (expression) {
      | Int({loc}) => loc
      | BinaryOperator({loc}) => loc
      }
  );