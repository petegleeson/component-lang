[@deriving show]
type line = int;
[@deriving show]
type column = int;
[@deriving show]
type point = (line, column);
[@deriving show]
type location = (point, point);

let get_start = ((s, _): location) => s;
let get_end = ((_, e): location) => e;