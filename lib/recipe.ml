open Yojson

type ingredient = {
  item : string;
  amount : float;
  unit : string;
}

type recipe = {
  name : string;
  flavor : string;
  restricts : string list;
  cook_time : int;
  ingredients : (string * ingredient list) list;
  directions : string list;
}
