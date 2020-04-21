open Source;

[@deriving show]
type kind =
  | Int
  | Error
  | Var
  | Void;

module BinaryOperator = {
  [@deriving show]
  type t('n) = {
    loc: location,
    operator: Token.operator,
    left: 'n,
    right: 'n,
    kind,
  };
};

module Int = {
  [@deriving show]
  type t = {
    loc: location,
    raw: string,
    kind,
  };
};

module Program = {
  [@deriving show]
  type t('n) = {
    loc: location,
    body: list('n),
    kind,
  };
};

module Unknown = {
  [@deriving show]
  type t = {
    loc: location,
    kind,
  };
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