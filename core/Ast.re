open Source;

module BinaryOperator = {
  [@deriving show]
  type t('n) = {
    loc: location,
    operator: Token.operator,
    left: 'n,
    right: 'n,
  };
};

module Int = {
  [@deriving show]
  type t = {
    loc: location,
    raw: string,
  };
};

module Program = {
  [@deriving show]
  type t('n) = {
    loc: location,
    body: list('n),
  };
};

module Unknown = {
  [@deriving show]
  type t = {loc: location};
};

[@deriving show]
type t =
  | BinaryOperator(BinaryOperator.t(t))
  | Int(Int.t)
  | Program(Program.t(t))
  | Unknown(Unknown.t);

let get_location = case =>
  switch (case) {
  | BinaryOperator({loc})
  | Int({loc})
  | Program({loc})
  | Unknown({loc}) => loc
  };