(** Filters and scores lists of recipes based on user's criteria *)

type scored_recipe = {
  recipe : Recipe.recipe;
  score : int;
  reason : string;
}
(** Type representation of a scored recipe, which contains a recipe, its score,
    and an explanation for its score. *)

type filter_type =
  | IncludesAll
  | ExcludesAll
      (** The type of filtering performed by the meets_criteria helper method.
          Not exposed to clients. *)

(** [meets_criteria filt_type rec_criter criteria] returns true if a single
    recipe's [rec_criter] [filt_type] ("includes all" or "excludes all") of the
    criteria listed in [criteria] *)
let rec meets_criteria filt_type rec_criter = function
  | [] -> true
  | restrict :: rest -> (
      let found = List.mem restrict rec_criter in
      match filt_type with
      | IncludesAll -> found && meets_criteria filt_type rec_criter rest
      | ExcludesAll -> (not found) && meets_criteria filt_type rec_criter rest)

(** [filter_allergens allergens recipes] removes all recipes in [recipes] that
    contain one or more of the allergens in the provided [allergens] *)
let rec filter_allergens allergens = function
  | [] -> []
  | recipe :: rest ->
      if meets_criteria ExcludesAll (Recipe.get_allergens recipe) allergens then
        recipe :: filter_allergens allergens rest
      else filter_allergens allergens rest

(** [filter_restricts restricts recipes] removes all recipes in [recipes] that
    do not meet all of the restrictions in [restricts] *)
let rec filter_restricts restricts = function
  | [] -> []
  | recipe :: rest ->
      if meets_criteria IncludesAll (Recipe.get_restrict recipe) restricts then
        recipe :: filter_restricts restricts rest
      else filter_restricts restricts rest

(** [filter responses recipes] removes all recipes in [recipes] that contain at
    least one of the allergens in [responses] or fail to meet all of the
    restrictions outlined in [responses] *)
let filter responses recipes =
  let no_allergens =
    filter_allergens (Answers.get_allergens responses) recipes
  in
  filter_restricts (Answers.get_restrict responses) no_allergens

(** [score_flavor resp_flavor rec_flavor] compares [resp_flavor] and
    [rec_flavor], returning a numerical score and reason for the score. *)
let score_flavor resp_flavor rec_flavor =
  match (resp_flavor, rec_flavor) with
  | Answers.Either, _ -> (1, "Matches preferred flavor")
  | Answers.Sweet, "sweet" | Answers.Savory, "savory" ->
      (1, "Matches preferred flavor")
  | Answers.Sweet, _ | Answers.Savory, _ ->
      (0, "Does not match preferred flavor")

(** [score_time resp_time rec_time] compares [resp_time] and [rec_time],
    returning a numerical score and reason for the score. *)
let score_time resp_time rec_time =
  if rec_time <= resp_time then
    (1, Printf.sprintf "Cook time under %d minutes" resp_time)
  else (0, Printf.sprintf "Cook time over %d minutes" resp_time)

(** [score_and_explain responses recipe] tabulates the score for a single
    [recipe] against the user's [responses], returning a scored_recipe with the
    recipe and scoring information. For MS2, each category contributes a binary
    score of 0 or 1 to signal if the recipe meets the requirement, but MS3 may
    expand beyond this. *)
let score_and_explain responses recipe =
  let flavor_score, flavor_reason =
    score_flavor (Answers.get_flavor responses) (Recipe.get_flavor recipe)
  in
  let time_score, time_reason =
    score_time (Answers.get_time responses) (Recipe.cook_time recipe)
  in
  {
    recipe;
    score = flavor_score + time_score;
    reason = flavor_reason ^ ", " ^ time_reason;
  }

(** [rank recipes responses] ranks the recipes in [recipes] based on how well
    they fit the user's preferences in [responses]. Requires that all inputted
    recipes do not violate the user's dietary restrictions. Returns the recipes
    ranked by their scores from highest to lowest. Sorting is stable - that is,
    if two recipes have the same score, they will be returned in the same order
    they were passed in as. *)
let rank responses recipes =
  let scored_recipes = List.map (score_and_explain responses) recipes in
  (* Sorts the list of scored recipes in decreasing order, starting with the
     recipe with the highest score. For example, if recipe 1 has a score of 5
     and recipe 2 has a score of 3, comparing the two yields a negative number,
     which means recipe 1 goes before recipe 2. Ties are currently broken
     arbitrarily *)
  List.stable_sort (fun rec1 rec2 -> rec2.score - rec1.score) scored_recipes

(** [get_recipe scored] returns the recipe in [scored] *)
let get_recipe scored = scored.recipe

(** [get_score scored_recipe] returns the score of the [scored_recipe] *)
let get_score scored = scored.score

(** [explain_score scored_recipe] returns the reason for the recipe's score *)
let explain_score scored = scored.reason
