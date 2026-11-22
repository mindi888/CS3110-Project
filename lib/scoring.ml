(** Filters and scores lists of recipes based on user's criteria *)

type scored_recipe = {
  recipe : Recipe.recipe;
  score : int;
  reason : string;
}

let rec meets_criteria rec_restrict = function
| [] -> true
| restrict :: rest -> (List.mem restrict rec_restrict) && (meets_criteria rec_restrict rest)
(* Tries to find each of the user's restrictions in the recipe's restrictions. If no match is found, the recipe does not meet the restriction, and the function returns false. There may be a faster way to implement this. *)
(* Assumes that recipes that meet common allergy restrictions contain a restriction something along the lines of "free of _", where _ is the common allergy food. *)

let rec filter responses = function
| [] -> []
| recipe :: rest -> 
  if meets_criteria (Recipe.get_restrict recipe) (Answers.get_restrict responses) then recipe :: (filter responses rest)
  else filter responses rest
(** [filter responses recipes] removes all recipes in [recipes] that contain one or more of the restrictions in the provided [responses] *)

let rank responses recipes = []
(** [rank recipes responses] ranks the recipes in [recipes] based on how well they fit the user's preferences in [responses]. Requires that all inputted recipes do not violate the user's dietary restrictions. Returns the recipes ranked by their scores from highest to lowest. *)

let get_recipe scored = scored.recipe
(** [get_recipe scored] returns the recipe in [scored] *)

let get_score scored = scored.score
(** [get_score scored_recipe] returns the score of the [scored_recipe] *)

let explain_score scored = scored.reason
(** [explain_score scored_recipe] returns the reason for the recipe's score *)