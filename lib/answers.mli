type flavor =
  | Sweet
  | Savory
  | Either

(* type restriction = | Gluten | Dairy | Meat | Poultry *)

val flavor_options : (string * flavor) list
val allergens_options : (string * string list) list
val restrict_options : (string * string list) list
val time_options : (string * int) list
val serving_options : (string * int) list

type answers

val make :
  flavor:flavor ->
  allergens:string list ->
  restrict:string list ->
  time:int ->
  serving:int ->
  answers
(** [make ~flavor ~allergens ~restrict ~time ~serving] is the answers with
    flavor preference [flavor], allergen tags to avoid [allergens], dietary
    restriction tags [restrict], maximum cooking time [time] in minutes, and
    [serving] servings.
    Raises: [Invalid_argument] if [time < 1] or [serving < 1]. *)

val get_flavor : answers -> flavor
(** [get_flavor a] is the flavor preference in [a]. *)

val get_allergens : answers -> string list
(** [get_allergens a] is the allergen tags to avoid in [a], e.g.
    [["gluten"; "nuts"]]; [[]] if there are none. *)

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
