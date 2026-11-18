(**Parses json files into recipes and recipie types*)

type recipe
(**Type representation of a recipe *)

val create : Yojson.Basic.t -> recipe
(**[create s] parses a json file and returns a recipe object*)
(**val get_name: recipe -> string 
val get_flavor: recipe -> string
val get_restrict: recipe -> string 
val cook_time: recipe -> int
val servings: recipe -> int*)