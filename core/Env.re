type t = {tokens: ref(list(Token.case))};

let init = tokens => {
  {tokens: ref(tokens)};
};

exception EnvError(string);

let peek = ({tokens}) => {
  switch (tokens^) {
  | [t, ...rest] => t
  | _ => raise(EnvError("nothing left to peek"))
  };
};

let has_more_tokens = ({tokens}) => {
  switch (tokens^) {
  | [] => false
  | _ => true
  };
};

let rec peek_until = ({tokens}, fn) =>
  switch (tokens^) {
  | [] => []
  | [t, ...rest] =>
    if (fn(t)) {
      [t];
    } else {
      [t, ...peek_until({tokens: ref(rest)}, fn)];
    }
  };

let eat = ({tokens}) => {
  tokens :=
    (
      switch (tokens^) {
      | [_, ...rest] => rest
      | _ => raise(EnvError("nothing left to eat"))
      }
    );
};

let expect = ({tokens}, fn) => {
  fn(peek({tokens: tokens})) ? eat({tokens: tokens}) : ();
};

let location = env => env |> peek |> Token.get_location;