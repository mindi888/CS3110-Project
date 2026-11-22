type flavor =
  | Sweet
  | Savory
  | Either

(* type restriction = | Gluten | Dairy | Meat | Poultry *)

val flavor_options : (string * flavor) list
val restrict_options : (string * string list) list
val time_options : (string * int) list
val serving_options : (string * int) list

type answers

val make :
  flavor:flavor -> restrict:string list -> time:int -> serving:int -> answers
(** [f rs t s] is the answers with flavor preference [f], recipe tags to avoid
    [rs], maximum cooking time [t] in minutes, and [s] servings. Raises:
    [Invalid_argument] if [t < 1] or [s < 1]. *)

val get_flavor : answers -> flavor
(** [get_flavor a] is the flavor preference in [a]. *)

val get_restrict : answers -> string list
(** [get_restrict a] is the recipe tags to avoid in [a], e.g.
    [["gluten"; "meat"]]; [[]] if there are none. *)

val get_time : answers -> int
(** [get_time a] is the maximum cooking time in minutes in [a]; [max_int] if the
    user has no limit. *)

val get_serving : answers -> int
(** [get_serving a] is the number of servings in [a]. *)

(* val restriction_to_string : restriction -> string
 * Matches the tags in the recipe JSON files, e.g. [Gluten] is "gluten". *)
