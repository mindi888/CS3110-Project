open OUnit2
open Recipebook

let recipes = Recipe.load_all "../data"

let test_filter _ =
  let answers = Answers.make ~flavor:Answers.Sweet ~allergens:[ "gluten" ]
          ~restrict:[ "vegan" ] ~time:30 ~serving:2 in
  let matcha =
        List.find (fun r -> Recipe.get_name r = "Matcha Cookies") recipes in
  let filtered = Scoring.filter answers [matcha] in
  assert_equal filtered []

let filter_tests_suite = "filter suite" >::: [
  "a trivial test" >:: test_filter
]

let _ = run_test_tt_main filter_tests_suite