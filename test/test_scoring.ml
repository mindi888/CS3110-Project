open OUnit2
open Recipebook

(* --------------- LOAD ALL RECIPES FOR LATER USE ------------------------ *)
let recipes = Recipe.load_all "../data"

(* --------------- HELPER METHODS FOR RECIPE INDEXING ---------------------- *)
let get_recipe name = List.find (fun r -> Recipe.get_name r = name) recipes

(* --------------- BEGINNING OF FILTERING TESTS ---------------------------- *)
let all_empty _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:[]
          ~restrict:[] ~time:30 ~serving:2 in
  let filtered = Scoring.filter answers [] in
  assert_equal filtered []

let no_recipes _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:["gluten"]
          ~restrict:["vegan"] ~time:30 ~serving:2 in
  let filtered = Scoring.filter answers [] in
  assert_equal filtered []

let no_allergens_or_restrict _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:[]
          ~restrict:[] ~time:30 ~serving:2 in
  let matcha = get_recipe "Matcha Cookies" in
  let filtered = Scoring.filter answers [matcha] in
  assert_equal [matcha] filtered

let free_of_allergens _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:["gluten"; "fish"]
          ~restrict:[] ~time:30 ~serving:2 in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [cheesecake] in
  assert_equal [cheesecake] filtered

let allergen_found_only _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:["gluten"; "fish"]
          ~restrict:[] ~time:30 ~serving:2 in
  let matcha = get_recipe "Matcha Cookies" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [matcha; cheesecake] in
  assert_equal [cheesecake] filtered

let meets_all_restrictions _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:[]
          ~restrict:["halal";"kosher";"vegan";"vegetarian"] ~time:30 ~serving:2 in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let filtered = Scoring.filter answers [kale; rice] in
  assert_equal [kale; rice] filtered

let restriction_violated _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:[]
          ~restrict:["halal";"kosher";"vegan";"vegetarian"] ~time:30 ~serving:2 in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [kale; cheesecake; rice] in
  assert_equal [kale; rice] filtered

let recipe_list_okay _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:["fish";"shellfish"]
          ~restrict:["halal";"vegetarian"] ~time:30 ~serving:2 in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [kale; cheesecake; rice] in
  assert_equal [kale; cheesecake; rice] filtered

let one_allergen_one_restrict _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:["soy"]
          ~restrict:["vegan"] ~time:30 ~serving:2 in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [kale; cheesecake; rice] in
  assert_equal [rice] filtered

let allergen_and_restrict_violated _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:["dairy"]
          ~restrict:["vegan"] ~time:30 ~serving:2 in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [kale; cheesecake; rice] in
  assert_equal [kale; rice] filtered

let ignore_preferences _ =
  let answers = Answers.make ~flavor:Answers.Savory ~allergens:["dairy"]
          ~restrict:["vegan"] ~time:60 ~serving:8 in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [kale; cheesecake; rice] in
  assert_equal [kale; rice] filtered

let single_allergen_drop _ =
  let answers = Answers.make ~flavor:Answers.Savory ~allergens:["dairy"]
          ~restrict:[] ~time:60 ~serving:8 in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [cheesecake] in
  assert_equal [] filtered

(* ------------------ BEGINNING OF RANKING TESTS --------------------------- *)
let empty_list _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:[]
          ~restrict:[] ~time:30 ~serving:2 in
  let ranked = Scoring.rank answers [] in
  assert_equal [] ranked
(*
let recipe_zero_score _ =
  let answers = Answers.make ~flavor:Answers.Savory ~allergens:[]
          ~restrict:[] ~time:30 ~serving:2 in
  let matcha = get_recipe "Matcha Cookies" in
  let ranked = Scoring.rank answers [matcha] in
  check_ranking [{}] *)

(* -------------------------- ALL TEST SUITES ------------------------------ *)
let filter_tests_suite = "Filtering test suite" >::: [
  "Filtering an empty recipe list with no allergens or restrictions in answers should return the empty list" >:: all_empty;
  "Filtering an empty recipe list with allergens and restrictions in answers should return the empty list" >:: no_recipes;
  "Filtering a non-empty recipe list with no allergens or restrictions in answers should return the entire list unchanged" >:: no_allergens_or_restrict;
  "Filtering with allergens, no restrictions, and a non-empty recipe list that does not contain the allergens returns the entire list" >:: free_of_allergens;
  "Filtering with allergens, no restrictions, and a non-empty recipe list with one recipe with a single allergen drops the recipe" >:: allergen_found_only;
  "Filtering with no allergens, multiple restrictions, and a non-empty recipe list that meets the restrictions returns the list unchanged" >:: meets_all_restrictions;
  "Filtering with no allergens, multiple restrictions, and a recipe list with one recipe that satisfies but not all restrictions drops the recipe" >:: restriction_violated;
  "Filtering with both allergens, restrictions, and a recipe list that meets all criteria returns the entire list" >:: recipe_list_okay;
  "Filtering with allergens, restrictions, and a recipe list where one recipe contains an allergen and the other violates a restriction" >:: one_allergen_one_restrict;
  "A recipe that contains both an allergen and does not meet a restriction is removed from the list" >:: allergen_and_restrict_violated;
  "Varying soft preferences like flavor and cook time should not change filtering results" >:: ignore_preferences;
  "A recipe with several allergens should be dropped if at least one is met by the user" >:: single_allergen_drop;
]

let rank_tests_suite = "Ranking test suite" >::: [
  "Ranking an empty recipe list should return the empty list" >:: empty_list;
  (* "Ranking a recipe list with a single recipe that does not meet any preferences should return a single-element list with a score of 0 and the proper explanation" >:: recipe_zero_score; *)
]

let _ = run_test_tt_main ("Tests" >:::
      [
        filter_tests_suite;
        rank_tests_suite
      ])