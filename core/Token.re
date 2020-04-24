open Source;

[@deriving show]
type raw = string;

[@deriving show]
type operator =
  | Plus
  | Minus
  | Multiply
  | Divide;

let precendence = op =>
  switch (op) {
  | Plus
  | Minus => 0
  | Multiply
  | Divide => 100
  };

[@deriving show]
type case =
  | Number(location, raw)
  | Operator(location, operator);

let get_location = case =>
  switch (case) {
  | Number(loc, _)
  | Operator(loc, _) => loc
  };