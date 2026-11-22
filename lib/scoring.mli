(** Filters and scores lists of recipes based on user's criteria *)

(* TO-DO: Synch answer & recipe types with Mindi and Jennifer *)
(* TO-DO: MS3 - Implement post-ranking adjustments, vary weighting for more important attributes (ex: preferring faster prep time) *)
(* Another question: could we get away with just passing in restrictions to filter instead of all of the answers? <-- May depend on how answers representation is abstracted *)
(* Questionable design choice (finalize for MS3): When user inputs additional preferences/restrictions after initial ranking, clients to scoring are responsible for calling the right methods to re-compute everything? *)
(* Note: I'm drawing a distinction between restrictions, which I'm taking to mean religious/life-threatening concerns like allergies that must be met (used in filtering), and preferences, which are things like cook time and flavor preference (used in ranking). We can re-organize which tags qualify as what later on. *)

(* NOTE: This type will be abstracted later on, so try to use the helper functions get_recipe, get_score, and explain_score *)
type scored_recipe = {
  recipe : Recipe.recipe;
  score : int;
  reason : string;
}
(** Type representation of a scored recipe, which contains a recipe, its score, and an explanation for its score. *)

val filter : Recipe.recipe list -> answers -> Recipe.recipe list
(** [filter recipes responses] removes all recipes in [recipes] that contain one or more of the restrictions in the provided [responses] *)

val rank : Recipe.recipe list -> answers -> scored_recipe list
(** [rank recipes responses] ranks the recipes in [recipes] based on how well they fit the user's preferences in [responses]. Requires that all inputted recipes do not violate the user's dietary restrictions. Returns the recipes ranked by their scores from highest to lowest. *)

val get_recipe : scored_recipe -> Recipe.recipe
(** [get_recipe scored_recipe] returns the recipe in [scored_recipe] *)

val get_score : scored_recipe -> int
(** [get_score scored_recipe] returns the score of the [scored_recipe] *)

val explain_score : scored_recipe -> string
(** [explain_score scored_recipe] returns the reason for the recipe's score *)