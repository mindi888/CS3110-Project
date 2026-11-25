open OUnit2
open Recipebook

(* --------------- LOAD ALL RECIPES FOR LATER USE ------------------------ *)
let recipes = Recipe.load_all "../data"

(* --------------- HELPER METHODS FOR RECIPE INDEXING ---------------------- *)
let get_recipe name = List.find (fun r -> Recipe.get_name r = name) recipes

let check_scores (recipe, score, explanation) scored =
  assert_equal recipe (Scoring.get_recipe scored);
  assert_equal score (Scoring.get_score scored);
  assert_equal explanation (Scoring.explain_score scored)

(* --------------- BEGINNING OF FILTERING TESTS ---------------------------- *)

let all_empty _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[] ~restrict:[] ~time:30
      ~serving:2
  in
  let filtered = Scoring.filter answers [] in
  assert_equal filtered []

let no_recipes _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[ "gluten" ]
      ~restrict:[ "vegan" ] ~time:30 ~serving:2
  in
  let filtered = Scoring.filter answers [] in
  assert_equal filtered []

let no_allergens_or_restrict _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[] ~restrict:[] ~time:30
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let filtered = Scoring.filter answers [ matcha ] in
  assert_equal [ matcha ] filtered

let free_of_allergens _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[ "gluten"; "fish" ]
      ~restrict:[] ~time:30 ~serving:2
  in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [ cheesecake ] in
  assert_equal [ cheesecake ] filtered

let allergen_found_only _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[ "gluten"; "fish" ]
      ~restrict:[] ~time:30 ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [ matcha; cheesecake ] in
  assert_equal [ cheesecake ] filtered

let meets_all_restrictions _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[]
      ~restrict:[ "halal"; "kosher"; "vegan"; "vegetarian" ]
      ~time:30 ~serving:2
  in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let filtered = Scoring.filter answers [ kale; rice ] in
  assert_equal [ kale; rice ] filtered

let restriction_violated _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[]
      ~restrict:[ "halal"; "kosher"; "vegan"; "vegetarian" ]
      ~time:30 ~serving:2
  in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [ kale; cheesecake; rice ] in
  assert_equal [ kale; rice ] filtered

let recipe_list_okay _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[ "fish"; "shellfish" ]
      ~restrict:[ "halal"; "vegetarian" ] ~time:30 ~serving:2
  in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [ kale; cheesecake; rice ] in
  assert_equal [ kale; cheesecake; rice ] filtered

let one_allergen_one_restrict _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[ "soy" ]
      ~restrict:[ "vegan" ] ~time:30 ~serving:2
  in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [ kale; cheesecake; rice ] in
  assert_equal [ rice ] filtered

let allergen_and_restrict_violated _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[ "dairy" ]
      ~restrict:[ "vegan" ] ~time:30 ~serving:2
  in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [ kale; cheesecake; rice ] in
  assert_equal [ kale; rice ] filtered

let ignore_preferences _ =
  let answers =
    Answers.make ~flavor:Answers.Savory ~allergens:[ "dairy" ]
      ~restrict:[ "vegan" ] ~time:60 ~serving:8
  in
  let kale = get_recipe "Kale Side Dish" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [ kale; cheesecake; rice ] in
  assert_equal [ kale; rice ] filtered

let single_allergen_drop _ =
  let answers =
    Answers.make ~flavor:Answers.Savory ~allergens:[ "dairy" ] ~restrict:[]
      ~time:60 ~serving:8
  in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let filtered = Scoring.filter answers [ cheesecake ] in
  assert_equal [] filtered

(* ------------------ BEGINNING OF RANKING TESTS --------------------------- *)

let empty_list _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[] ~restrict:[] ~time:30
      ~serving:2
  in
  let ranked = Scoring.rank answers [] in
  assert_equal [] ranked

let recipe_zero_score _ =
  let answers =
    Answers.make ~flavor:Answers.Savory ~allergens:[] ~restrict:[] ~time:30
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let ranked = Scoring.rank answers [ matcha ] in
  match ranked with
  | [] ->
      assert_failure "The ranking of a single-recipe list should have length 1"
  | [ scored ] ->
      check_scores
        (matcha, 0, "Does not match preferred flavor, Cook time over 30 minutes")
        scored
  | _ ->
      assert_failure "The ranking of a single-recipe list should have length 1"

let yes_flavor_no_time _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[] ~restrict:[] ~time:30
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let ranked = Scoring.rank answers [ matcha ] in
  match ranked with
  | [] ->
      assert_failure "The ranking of a single-recipe list should have length 1"
  | [ scored ] ->
      check_scores
        (matcha, 1, "Matches preferred flavor, Cook time over 30 minutes")
        scored
  | _ ->
      assert_failure "The ranking of a single-recipe list should have length 1"

let no_flavor_yes_time _ =
  let answers =
    Answers.make ~flavor:Answers.Savory ~allergens:[] ~restrict:[] ~time:60
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let ranked = Scoring.rank answers [ matcha ] in
  match ranked with
  | [] ->
      assert_failure "The ranking of a single-recipe list should have length 1"
  | [ scored ] ->
      check_scores
        ( matcha,
          1,
          "Does not match preferred flavor, Cook time under 60 minutes" )
        scored
  | _ ->
      assert_failure "The ranking of a single-recipe list should have length 1"

let no_flavor_exact_time _ =
  let answers =
    Answers.make ~flavor:Answers.Savory ~allergens:[] ~restrict:[] ~time:35
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let ranked = Scoring.rank answers [ matcha ] in
  match ranked with
  | [] ->
      assert_failure "The ranking of a single-recipe list should have length 1"
  | [ scored ] ->
      check_scores
        ( matcha,
          1,
          "Does not match preferred flavor, Cook time under 35 minutes" )
        scored
  | _ ->
      assert_failure "The ranking of a single-recipe list should have length 1"

let flavor_and_time_match _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[] ~restrict:[] ~time:60
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let ranked = Scoring.rank answers [ matcha ] in
  match ranked with
  | [] ->
      assert_failure "The ranking of a single-recipe list should have length 1"
  | [ scored ] ->
      check_scores
        (matcha, 2, "Matches preferred flavor, Cook time under 60 minutes")
        scored
  | _ ->
      assert_failure "The ranking of a single-recipe list should have length 1"

let no_flavor_preference _ =
  let answers =
    Answers.make ~flavor:Answers.Either ~allergens:[] ~restrict:[] ~time:60
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let rice = get_recipe "Fried Rice" in
  let ranked_matcha = Scoring.rank answers [ matcha ] in
  let ranked_rice = Scoring.rank answers [ rice ] in
  let _ =
    match ranked_matcha with
    | [] ->
        assert_failure
          "The ranking of a single-recipe list should have length 1"
    | [ scored ] ->
        check_scores
          (matcha, 2, "Matches preferred flavor, Cook time under 60 minutes")
          scored
    | _ ->
        assert_failure
          "The ranking of a single-recipe list should have length 1"
  in
  match ranked_rice with
  | [] ->
      assert_failure "The ranking of a single-recipe list should have length 1"
  | [ scored ] ->
      check_scores
        (rice, 2, "Matches preferred flavor, Cook time under 60 minutes")
        scored
  | _ ->
      assert_failure "The ranking of a single-recipe list should have length 1"

let independent_scores _ =
  let answers =
    Answers.make ~flavor:Answers.Savory ~allergens:[] ~restrict:[] ~time:35
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let rice = get_recipe "Fried Rice" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let ranked = Scoring.rank answers [ rice; matcha; cheesecake ] in
  match ranked with
  | [] ->
      assert_failure "The ranking of a three-recipe list should have length 3"
  | [ scored_rice; scored_matcha; scored_cheesecake ] ->
      check_scores
        (rice, 2, "Matches preferred flavor, Cook time under 35 minutes")
        scored_rice;
      check_scores
        ( matcha,
          1,
          "Does not match preferred flavor, Cook time under 35 minutes" )
        scored_matcha;
      check_scores
        ( cheesecake,
          0,
          "Does not match preferred flavor, Cook time over 35 minutes" )
        scored_cheesecake
  | _ ->
      assert_failure "The ranking of a three-recipe list should have length 3"

let both_zero _ =
  let answers =
    Answers.make ~flavor:Answers.Savory ~allergens:[] ~restrict:[] ~time:30
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let ranked = Scoring.rank answers [ matcha; cheesecake ] in
  match ranked with
  | [] -> assert_failure "The ranking of a two-recipe list should have length 2"
  | [ scored_matcha; scored_cheesecake ] ->
      check_scores
        (matcha, 0, "Does not match preferred flavor, Cook time over 30 minutes")
        scored_matcha;
      check_scores
        ( cheesecake,
          0,
          "Does not match preferred flavor, Cook time over 30 minutes" )
        scored_cheesecake
  | _ -> assert_failure "The ranking of a two-recipe list should have length 2"

let both_one _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[] ~restrict:[] ~time:30
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let kale = get_recipe "Kale Side Dish" in
  let ranked = Scoring.rank answers [ matcha; kale ] in
  match ranked with
  | [] -> assert_failure "The ranking of a two-recipe list should have length 2"
  | [ scored_matcha; scored_kale ] ->
      check_scores
        (matcha, 1, "Matches preferred flavor, Cook time over 30 minutes")
        scored_matcha;
      check_scores
        (kale, 1, "Does not match preferred flavor, Cook time under 30 minutes")
        scored_kale
  | _ -> assert_failure "The ranking of a two-recipe list should have length 2"

let first_score_better _ =
  let answers =
    Answers.make ~flavor:Answers.Savory ~allergens:[] ~restrict:[] ~time:30
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let rice = get_recipe "Fried Rice" in
  let ranked = Scoring.rank answers [ rice; matcha ] in
  match ranked with
  | [] -> assert_failure "The ranking of a two-recipe list should have length 2"
  | [ scored_rice; scored_matcha ] ->
      check_scores
        (rice, 2, "Matches preferred flavor, Cook time under 30 minutes")
        scored_rice;
      check_scores
        (matcha, 0, "Does not match preferred flavor, Cook time over 30 minutes")
        scored_matcha
  | _ -> assert_failure "The ranking of a two-recipe list should have length 2"

let second_score_better _ =
  let answers =
    Answers.make ~flavor:Answers.Savory ~allergens:[] ~restrict:[] ~time:30
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let rice = get_recipe "Fried Rice" in
  let ranked = Scoring.rank answers [ matcha; rice ] in
  match ranked with
  | [] -> assert_failure "The ranking of a two-recipe list should have length 2"
  | [ scored_rice; scored_matcha ] ->
      check_scores
        (rice, 2, "Matches preferred flavor, Cook time under 30 minutes")
        scored_rice;
      check_scores
        (matcha, 0, "Does not match preferred flavor, Cook time over 30 minutes")
        scored_matcha
  | _ -> assert_failure "The ranking of a two-recipe list should have length 2"

let some_same_scores _ =
  let answers =
    Answers.make ~flavor:Answers.Savory ~allergens:[] ~restrict:[] ~time:30
      ~serving:2
  in
  let matcha = get_recipe "Matcha Cookies" in
  let rice = get_recipe "Fried Rice" in
  let kale = get_recipe "Kale Side Dish" in
  let ranked = Scoring.rank answers [ rice; matcha; kale ] in
  match ranked with
  | [] -> assert_failure "The ranking of a two-recipe list should have length 3"
  | [ scored_rice; scored_kale; scored_matcha ] ->
      check_scores
        (rice, 2, "Matches preferred flavor, Cook time under 30 minutes")
        scored_rice;
      check_scores
        (kale, 2, "Matches preferred flavor, Cook time under 30 minutes")
        scored_kale;
      check_scores
        (matcha, 0, "Does not match preferred flavor, Cook time over 30 minutes")
        scored_matcha
  | _ -> assert_failure "The ranking of a two-recipe list should have length 3"

let same_category_weights _ =
  let answers =
    Answers.make ~flavor:Answers.Sweet ~allergens:[] ~restrict:[] ~time:35
      ~serving:2
  in
  let kale = get_recipe "Kale Side Dish" in
  let cheesecake = get_recipe "Blender Cheesecake" in
  let ranked = Scoring.rank answers [ kale; cheesecake ] in
  match ranked with
  | [] -> assert_failure "The ranking of a two-recipe list should have length 2"
  | [ scored_kale; scored_cheesecake ] ->
      check_scores
        (kale, 1, "Does not match preferred flavor, Cook time under 35 minutes")
        scored_kale;
      check_scores
        (cheesecake, 1, "Matches preferred flavor, Cook time over 35 minutes")
        scored_cheesecake
  | _ -> assert_failure "The ranking of a two-recipe list should have length 2"

(* -------------------------- ALL TEST SUITES ------------------------------ *)

let filter_tests_suite =
  "Filtering test suite"
  >::: [
         "Filtering an empty recipe list with no allergens or restrictions in \
          answers should return the empty list" >:: all_empty;
         "Filtering an empty recipe list with allergens and restrictions in \
          answers should return the empty list" >:: no_recipes;
         "Filtering a non-empty recipe list with no allergens or restrictions \
          in answers should return the entire list unchanged"
         >:: no_allergens_or_restrict;
         "Filtering with allergens, no restrictions, and a non-empty recipe \
          list that does not contain the allergens returns the entire list"
         >:: free_of_allergens;
         "Filtering with allergens, no restrictions, and a non-empty recipe \
          list with one recipe with a single allergen drops the recipe"
         >:: allergen_found_only;
         "Filtering with no allergens, multiple restrictions, and a non-empty \
          recipe list that meets the restrictions returns the list unchanged"
         >:: meets_all_restrictions;
         "Filtering with no allergens, multiple restrictions, and a recipe \
          list with one recipe that satisfies but not all restrictions drops \
          the recipe" >:: restriction_violated;


         "Filtering with both allergens, restrictions, and a recipe list that \
          meets all criteria returns the entire list" >:: recipe_list_okay;
         "Filtering with allergens, restrictions, and a recipe list where one \
          recipe contains an allergen and the other violates a restriction"
         >:: one_allergen_one_restrict;
         "A recipe that contains both an allergen and does not meet a \
          restriction is removed from the list"
         >:: allergen_and_restrict_violated;
         "Varying soft preferences like flavor and cook time should not change \
          filtering results" >:: ignore_preferences;
         "A recipe with several allergens should be dropped if at least one is \
          met by the user" >:: single_allergen_drop;
       ]

let rank_tests_suite =
  "Ranking test suite"
  >::: [
         "Ranking an empty recipe list should return the empty list"
         >:: empty_list;
         "Ranking a recipe list with a single recipe that does not meet any \
          preferences should return a single-element list with a score of 0 \
          and the proper explanation" >:: recipe_zero_score;
         "Ranking a single recipe with a flavor match but no time match should \
          return a single-element list with a score of 1 and the proper \
          explanation" >:: yes_flavor_no_time;
         "Ranking a single recipe with no flavor match but with a time match \
          should return a single-element list with a score of 1 and the proper \
          explanation" >:: no_flavor_yes_time;
         "A single recipe with no flavor match and an exact time match should \
          have a score of 1 and the proper explanation" >:: no_flavor_exact_time;
         "A single recipe with both a flavor match and a time match should \
          have a score of 2 and the proper explanation"
         >:: flavor_and_time_match;


         "A flavor preference of 'Either' gives both savory and sweet recipes \
          a score of 1 with the proper explanation" >:: no_flavor_preference;
         "Ranking a list of three recipes with different scores should produce \
          the correct score and explanation for each recipe"
         >:: independent_scores;
         "Ranking two recipes that both get a score of 0 should return the \
          scored recipes in the same order they were passed in, with the \
          correct explanations" >:: both_zero;
         "Ranking two recipes that both get a score of 1 should return the \
          scored recipes in the same order they were passed in, with the \
          correct explanations" >:: both_one;
         "If the first recipe has a higher score than the second, the scored \
          recipes are ordered the same way they were passed in, with the \
          correct scores and explanations" >:: first_score_better;
         "If the second recipe has a higher score than the first, their order \
          is swapped in the returned list, and both have the correct \
          score/explanations" >:: second_score_better;
         "If two recipes have the same score, they are listed consecutively in \
          the returned list, ordered correctly relative to the other recipes, \
          and have the correct scores/explanations" >:: some_same_scores;
         "Ranking two recipes that both get a score of 1 but for different \
          reasons should return the recipes in the same order they were passed \
          in, with the correct explanations" >:: same_category_weights;
       ]


let _ = run_test_tt_main ("Tests" >::: 
  [ 
    filter_tests_suite; 
    rank_tests_suite 
  ])
