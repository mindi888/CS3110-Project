open OUnit2
open Recipebook

(** [ans ()] is [Questions.to_answers] with default labels, so each test only
    has to name the label it is checking. *)
let ans ?(flavor = "Sweet") ?(allergens = []) ?(restrict = [])
    ?(time = "Under 1 hour") ?(serving = "4") () =
  Questions.to_answers ~flavor ~allergens ~restrict ~time ~serving

let string_list = List.fold_left (fun acc s -> acc ^ "\"" ^ s ^ "\"; ") ""
let print_list l = "[ " ^ string_list l ^ "]"

(* ---------- Questions: what the user sees ---------- *)

let question_tests =
  [
    ( "there are 5 questions" >:: fun _ ->
      assert_equal ~printer:string_of_int 5 (List.length Questions.questions) );
    ( "flavor choices come from Answers.flavor_options" >:: fun _ ->
      assert_equal ~printer:print_list
        [ "Sweet"; "Savory"; "No preference" ]
        (Questions.get_choices Questions.flavor_q) );
    ( "allergen choices include None of these" >:: fun _ ->
      assert_equal ~printer:print_list
        [
          "Gluten"; "Dairy"; "Eggs"; "Shellfish"; "Fish"; "Sesame"; "Nuts";
          "None of these";
        ]
        (Questions.get_choices Questions.allergens_q) );
    ( "restriction choices include None of these" >:: fun _ ->
      assert_equal ~printer:print_list
        [ "Kosher"; "Vegan"; "Vegetarian"; "Halal"; "None of these" ]
        (Questions.get_choices Questions.restrict_q) );
    ( "time choices in display order" >:: fun _ ->
      assert_equal ~printer:print_list
        [ "Under 30 minutes"; "Under 1 hour"; "Under 2 hours"; "Doesn't matter" ]
        (Questions.get_choices Questions.time_q) );
    ( "serving choices are the numbers as strings" >:: fun _ ->
      assert_equal ~printer:print_list
        [ "1"; "2"; "3"; "4"; "6"; "8"; "12" ]
        (Questions.get_choices Questions.serving_q) );
    ( "styles" >:: fun _ ->
      assert_equal Questions.Buttons (Questions.get_style Questions.flavor_q);
      assert_equal Questions.Checkboxes
        (Questions.get_style Questions.allergens_q);
      assert_equal Questions.Checkboxes
        (Questions.get_style Questions.restrict_q);
      assert_equal Questions.Buttons (Questions.get_style Questions.time_q);
      assert_equal Questions.Dropdown (Questions.get_style Questions.serving_q)
    );
  ]

(* ---------- to_answers: clicked labels -> answers ---------- *)

let to_answers_tests =
  [
    ( "a full set of labels" >:: fun _ ->
      let a =
        ans ~flavor:"Savory" ~allergens:[ "Gluten"; "Nuts" ]
          ~restrict:[ "Vegan" ] ~time:"Under 2 hours" ~serving:"6" ()
      in
      assert_equal Answers.Savory (Answers.get_flavor a);
      assert_equal ~printer:print_list [ "gluten"; "nuts" ]
        (Answers.get_allergens a);
      assert_equal ~printer:print_list [ "vegan" ] (Answers.get_restrict a);
      assert_equal ~printer:string_of_int 120 (Answers.get_time a);
      assert_equal ~printer:string_of_int 6 (Answers.get_serving a) );
    ( "No preference is Either" >:: fun _ ->
      assert_equal Answers.Either
        (Answers.get_flavor (ans ~flavor:"No preference" ())) );
    (* allergens *)
    ( "allergens: None of these gives []" >:: fun _ ->
      assert_equal ~printer:print_list []
        (Answers.get_allergens (ans ~allergens:[ "None of these" ] ())) );
    ( "allergens: no boxes checked gives []" >:: fun _ ->
      assert_equal ~printer:print_list []
        (Answers.get_allergens (ans ~allergens:[] ())) );
    ( "allergens: None of these mixed with others adds nothing" >:: fun _ ->
      assert_equal ~printer:print_list [ "dairy" ]
        (Answers.get_allergens
           (ans ~allergens:[ "Dairy"; "None of these" ] ())) );
    ( "allergens: all seven, in order" >:: fun _ ->
      assert_equal ~printer:print_list
        [ "gluten"; "dairy"; "eggs"; "shellfish"; "fish"; "sesame"; "nuts" ]
        (Answers.get_allergens
           (ans
              ~allergens:
                [ "Gluten"; "Dairy"; "Eggs"; "Shellfish"; "Fish"; "Sesame"; "Nuts" ]
              ())) );
    (* restrictions *)
    ( "restrict: None of these gives []" >:: fun _ ->
      assert_equal ~printer:print_list []
        (Answers.get_restrict (ans ~restrict:[ "None of these" ] ())) );
    ( "restrict: no boxes checked gives []" >:: fun _ ->
      assert_equal ~printer:print_list []
        (Answers.get_restrict (ans ~restrict:[] ())) );
    ( "restrict: None of these mixed with others adds nothing" >:: fun _ ->
      assert_equal ~printer:print_list [ "halal" ]
        (Answers.get_restrict (ans ~restrict:[ "Halal"; "None of these" ] ())) );
    ( "restrict: all four, in order" >:: fun _ ->
      assert_equal ~printer:print_list
        [ "kosher"; "vegan"; "vegetarian"; "halal" ]
        (Answers.get_restrict
           (ans ~restrict:[ "Kosher"; "Vegan"; "Vegetarian"; "Halal" ] ())) );
    ( "allergens and restrictions stay separate" >:: fun _ ->
      let a = ans ~allergens:[ "Eggs" ] ~restrict:[ "Kosher" ] () in
      assert_equal ~printer:print_list [ "eggs" ] (Answers.get_allergens a);
      assert_equal ~printer:print_list [ "kosher" ] (Answers.get_restrict a) );
    (* time and servings *)
    ( "Doesn't matter is max_int" >:: fun _ ->
      assert_equal ~printer:string_of_int max_int
        (Answers.get_time (ans ~time:"Doesn't matter" ())) );
    ( "every time label maps to its minutes" >:: fun _ ->
      List.iter
        (fun (label, minutes) ->
          assert_equal ~printer:string_of_int minutes
            (Answers.get_time (ans ~time:label ())))
        Answers.time_options );
    ( "every serving label maps to its number" >:: fun _ ->
      List.iter
        (fun (label, n) ->
          assert_equal ~printer:string_of_int n
            (Answers.get_serving (ans ~serving:label ())))
        Answers.serving_options );
  ]

(* ---------- Answers: make and the options tables ---------- *)

let answers_tests =
  [
    ( "make stores what it is given" >:: fun _ ->
      let a =
        Answers.make ~flavor:Answers.Sweet ~allergens:[ "nuts" ]
          ~restrict:[ "vegan" ] ~time:30 ~serving:2
      in
      assert_equal Answers.Sweet (Answers.get_flavor a);
      assert_equal [ "nuts" ] (Answers.get_allergens a);
      assert_equal [ "vegan" ] (Answers.get_restrict a);
      assert_equal 30 (Answers.get_time a);
      assert_equal 2 (Answers.get_serving a) );
    ( "make rejects time 0" >:: fun _ ->
      assert_raises (Invalid_argument "Time must be positive") (fun () ->
          Answers.make ~flavor:Answers.Sweet ~allergens:[] ~restrict:[] ~time:0
            ~serving:1) );
    ( "make rejects serving 0" >:: fun _ ->
      assert_raises (Invalid_argument "Serving must be positive") (fun () ->
          Answers.make ~flavor:Answers.Sweet ~allergens:[] ~restrict:[] ~time:30
            ~serving:0) );
    ( "all time and serving options are positive" >:: fun _ ->
      List.iter
        (fun (label, n) -> assert_bool label (n > 0))
        (Answers.time_options @ Answers.serving_options) );
    ( "allergen and restriction tags are lowercase, like the recipe JSON"
    >:: fun _ ->
      List.iter
        (fun (label, tags) ->
          List.iter
            (fun t -> assert_equal ~msg:label (String.lowercase_ascii t) t)
            tags)
        (Answers.allergens_options @ Answers.restrict_options) );
  ]

let tests =
  "recipebook"
  >::: [
         "questions" >::: question_tests;
         "to_answers" >::: to_answers_tests;
         "answers" >::: answers_tests;
       ]

let _ = run_test_tt_main tests
