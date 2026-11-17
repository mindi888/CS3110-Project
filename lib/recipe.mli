(**loads and tags each recipie to be suggested*)

type recipe
(** representation of the recipe *)

val create: Yojson.Basic.t -> recipe
(** [create j] will parse a json file of a certain recipe and return a recipe*)