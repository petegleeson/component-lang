exception
  TypeCheckError({
    loc: Source.location,
    hint: option(string),
  });

let kind =
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

let unify = (a, b) =>
  switch (kind(a), kind(b)) {
  | (Var, x) => (set_kind(x, a), b)
  // @Incomplete should not default to doing nothing
  | (_, _) => (a, b)
  };

let type_expression = Ast.Expression.(expr => expr);

let type_identifier = Ast.Identifier.(id => id);

let type_declaration =
  Ast.Declaration.(
    ({loc, id, value, kind}) => {
      let typed_id = type_identifier(id);

      let typed_value = type_expression(value);

      let (unified_id, unified_value) =
        unify(Ast.Expression.Identifier(typed_id), typed_value);

      // @Incomplete unify return initial type?
      {loc, id: switch(unified_id) {
        | Identifier(id) => id
        | _ => raise(TypeCheckError({ loc: typed_id.loc, hint: Some("Identifier not unified properly")}))
      }, value: unified_value, kind};
    }
  );

let type_statement =
  Ast.Statement.(
    stmt =>
      switch (stmt) {
      | Expression(expr) => Expression(type_expression(expr))
      | Declaration(decl) => Declaration(type_declaration(decl))
      }
  );

let type_program =
  Ast.Program.(
    ({loc, body, kind, scope}) => {
      loc,
      body: List.map(type_statement, body),
      kind,
      scope,
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
