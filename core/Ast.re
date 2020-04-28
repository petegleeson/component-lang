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
and Block: {
  type t = {
    loc: location,
    expressions: list(Expression.t),
    kind,
  };
} = Block
and Expression: {
  type t =
    | BinaryOperator(BinaryOperator.t)
    | Int(Int.t)
    | Block(Block.t);
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

let rec show_expression =
  Expression.(
    exp =>
      switch (exp) {
      | Int({loc, kind, raw}) =>
        Format.sprintf(
          "@[<2>Int.{@ loc: %s@ kind: %s@ raw: \"%s\"@ }@]@.",
          Source.show_location(loc),
          show_kind(kind),
          raw,
        )
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
            (res, exp) => res ++ show_expression(exp) ++ ", ",
            "",
            expressions,
          ),
        )
      }
  );

let show_program =
  Program.(
    ({loc, kind, body}) =>
      Format.sprintf(
        "@[<2>Program.{@ loc: %s@ kind: %s@ expressions: [@ %s ]@ }@]@.",
        Source.show_location(loc),
        show_kind(kind),
        List.fold_left(
          (res, exp) => res ++ show_expression(exp) ++ ", ",
          "",
          body,
        ),
      )
  );

// let loc = ((1, 1), (1, 1));

// let myInt = Expression.Int({loc, raw: "1", kind: Int});

// print_endline(
//   show_expression(
//     Expression.Block({
//       loc,
//       kind: Var,
//       expressions: [
//         Expression.BinaryOperator({
//           loc,
//           kind: Int,
//           operator: Plus,
//           left: myInt,
//           right: myInt,
//         }),
//         Expression.BinaryOperator({
//           loc,
//           kind: Int,
//           operator: Plus,
//           left: myInt,
//           right: myInt,
//         }),
//       ],
//     }),
//   ),
// );

let get_location =
  Expression.(
    expression =>
      switch (expression) {
      | Int({loc}) => loc
      | Block({loc}) => loc
      | BinaryOperator({loc}) => loc
      }
  );