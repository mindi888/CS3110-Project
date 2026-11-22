(**Parses json files into recipes and recipe types*)

type recipe
(**Type representation of a recipe *)

type ingredient = {
  item : string;  (**Name of ingredient*)
  amount : float;  (**Amount of ingredient required as a float value*)
  unit : string;
      (**Measurment unit of ingredient required, empty string if no unit*)
}
(**Type representation of an ingredient required for the recipe*)

val create : Yojson.Basic.t -> recipe
(**[create s] parses one recipie that is written on a json file and returns a
   recipe object*)

val load_all : string -> recipe list
(** [load_all dir] returns a list of recpie objects that represent all recipes
    (json files) in the directory [dir]*)

val get_name : recipe -> string
(** [get_name r] returns the name of the recipe [r]*)

val get_flavor : recipe -> string
(** [get_flavor r] returns the flavor of the recipe [r], sweet or savory*)

val get_allergens : recipe -> string list
(** [get_allergens r] returns a list of food allergens that are contianed in the recipe
    [r] *)

val get_restrict : recipe -> string list
(** [get_restrict r] returns a list of religious/deitary restrictions rules it meets in recipe
    [r] *)

val cook_time : recipe -> int
(** [cook_time r] returns the cooking time in minutes of the recipe [r]*)

val servings : recipe -> int
(** [servings r] returns the number of servings in the recipie [r]*)

val get_ing : recipe -> (string * ingredient list) list
(** [get_ing r] returns a list of ingredients required for the recipie seperated
    by catagory. Such as wet and dry or cheese sauce and crust*)

val get_directions : recipe -> string list
(** [get_directions r] returns a string list of all the instructions in
    chronological order*)
