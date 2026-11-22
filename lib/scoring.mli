(** Filters and scores lists of recipes based on user's criteria *)

(* TO-DO: Synch answer & recipe types with Mindi and Jennifer *)
(* TO-DO: MS3 - Implement post-ranking adjustments, vary weighting for more important attributes (ex: preferring faster prep time) *)
(* Questionable design choice (finalize for MS3): When user inputs additional preferences/restrictions after initial ranking, clients to scoring are responsible for calling the right methods to re-compute everything? *)

type scored_recipe
(** Type representation of a scored recipe, which contains a recipe, its score, and an explanation for its score. *)

val filter :  Answers.answers -> Recipe.recipe list -> Recipe.recipe list
(** [filter responses recipes] removes all recipes in [recipes] that contain one or more of the restrictions in the provided [responses] *)

val rank :  Answers.answers -> Recipe.recipe list -> scored_recipe list
(** [rank responses recipes] ranks the recipes in [recipes] based on how well they fit the user's preferences in [responses]. Requires that all inputted recipes do not violate the user's dietary restrictions. Returns the recipes ranked by their scores from highest to lowest. *)

val get_recipe : scored_recipe -> Recipe.recipe
(** [get_recipe scored] returns the recipe in [scored] *)

val get_score : scored_recipe -> int
(** [get_score scored] returns the score of the [scored] *)

val explain_score : scored_recipe -> string
(** [explain_score scored] returns the reason for the recipe's score *)