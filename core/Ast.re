open Source;

type case =
  | BinaryOperator({
      loc: location,
      operator: Token.operator,
      left: case,
      right: case,
    })
  | Int({
      loc: location,
      raw: string,
    })
  | Program({
      loc: location,
      body: list(case),
    })
  | Unknown({loc: location});

let get_location = case =>
  switch (case) {
  | BinaryOperator({loc})
  | Int({loc})
  | Program({loc})
  | Unknown({loc}) => loc
  };