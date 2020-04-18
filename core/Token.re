open Source;

type raw = string;

type operator =
  | Plus
  | Minus
  | Multiply
  | Divide;

type case =
  | Number(location, raw)
  | Operator(location, operator)
  | Invalid(location, raw);

let get_location = case =>
  switch (case) {
  | Number(loc, _)
  | Operator(loc, _)
  | Invalid(loc, _) => loc
  };