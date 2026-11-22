type flavor =
  | Sweet
  | Savory
  | Either

type restriction =
  | Gluten
  | Dairy
  | Meat
  | Poultry

type time =
  | Under30
  | Under60
  | Under120
  | Anytime

type answer
(** A user's completed answers. Invariant: [serving > 0]. *)

val make : flavor -> restriction list -> time -> int -> answer
(** [f rs t s] is the answer, where [f] is flavor, [rs] is restrictions, 
  [t] is time, and [s] is serving size.
    Raises: [Invalid_argument] if [serving < 1]. *)

val flavor : answer -> flavor
val restrictions : answer -> restriction list
(** [] if the user selected none *)
val time : answer -> time
val serving : answer -> int

val restriction_to_string : restriction -> string
(** Matches the tags in the recipe JSON files, e.g. [Gluten] is "gluten". *)

val time_to_minutes : time -> int
(** [Anytime] is [max_int]. *)

val change_flavor : answer -> flavor -> answer
val change_restrictions : answer -> restriction list-> answer
val change_time : answer -> time -> answer
val change_serving : answer -> int -> answer