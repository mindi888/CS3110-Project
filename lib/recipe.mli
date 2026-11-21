(**Parses json files into recipes and recipe types*)

type recipe
(**Type representation of a recipe *)

type ingredient = {
  item : string;
  amount : float;
  unit : string;
}

val create : Yojson.Basic.t -> recipe
(**[create s] parses one recipie that is written on a json file and returns a
   recipe object*)

val get_name : recipe -> string
(** [get_name r] returns the name of the recipe [r]*)

val get_flavor : recipe -> string
(** [get_flavor r] returns the flavor of the recipe [r], sweet or savory*)

val get_restrict : recipe -> string list
(** [get_restrict r] returns a list ofs the dietary restrictions of the recipe
    [r] *)

val cook_time : recipe -> int
(** [cook_time r] returns the cooking time in minutes of the recipe [r]*)

val servings : recipe -> int
(** [servings r] returns the number of servings in the recipie [r]*)
