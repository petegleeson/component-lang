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
  );

let rec type_check =
  Ast.(
    ast =>
      switch (ast) {
      | Int({loc, raw}) => Int({loc, raw, kind: Int})
      | BinaryOperator({loc, operator, left, right}) =>
        BinaryOperator({
          loc,
          operator,
          kind:
            unify(
              unify(Int, get_kind(left)),
              unify(Int, get_kind(right)),
            ),
          left,
          right,
        })
      | Program({loc, kind, body}) =>
        Program({loc, kind, body: List.map(type_check, body)})
      | Unknown({loc}) => Unknown({loc, kind: Var})
      }
  );