open OUnit2
open Recipebook

(** [ans ()] is [Questions.to_answers] with default labels, so each test only
    has to name the label it is checking. *)
let ans ?(flavor = "Sweet") ?(restrict = []) ?(time = "Under 1 hour")
    ?(serving = "4") () =
  Questions.to_answers ~flavor ~restrict ~time ~serving

let string_list = List.fold_left (fun acc s -> acc ^ "\"" ^ s ^ "\"; ") ""
let print_list l = "[ " ^ string_list l ^ "]"

(* ---------- Questions: what the user sees ---------- *)

let question_tests =
  [
    ( "there are 4 questions" >:: fun _ ->
      assert_equal 4 (List.length Questions.questions) );
    ( "flavor choices come from Answers.flavor_options" >:: fun _ ->
      assert_equal ~printer:print_list
        [ "Sweet"; "Savory"; "No preference" ]
        (Questions.get_choices Questions.flavor_q) );
    ( "restriction choices include None of these" >:: fun _ ->
      assert_equal ~printer:print_list
        [ "Gluten"; "Dairy"; "Meat"; "Poultry"; "None of these" ]
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
        (Questions.get_style Questions.restrict_q);
      assert_equal Questions.Dropdown (Questions.get_style Questions.serving_q)
    );
  ]

(* ---------- to_answers: clicked labels -> answers ---------- *)

let to_answers_tests =
  [
    ( "a full set of labels" >:: fun _ ->
      let a =
        ans ~flavor:"Savory" ~restrict:[ "Gluten"; "Meat" ] ~time:"Under 2 hours"
          ~serving:"6" ()
      in
      assert_equal Answers.Savory (Answers.get_flavor a);
      assert_equal ~printer:print_list [ "gluten"; "meat" ]
        (Answers.get_restrict a);
      assert_equal ~printer:string_of_int 120 (Answers.get_time a);
      assert_equal ~printer:string_of_int 6 (Answers.get_serving a) );
    ( "No preference is Either" >:: fun _ ->
      assert_equal Answers.Either
        (Answers.get_flavor (ans ~flavor:"No preference" ())) );
    ( "None of these gives no restrictions" >:: fun _ ->
      assert_equal ~printer:print_list []
        (Answers.get_restrict (ans ~restrict:[ "None of these" ] ())) );
    ( "no boxes checked gives no restrictions" >:: fun _ ->
      assert_equal ~printer:print_list []
        (Answers.get_restrict (ans ~restrict:[] ())) );
    ( "None of these mixed with others adds nothing" >:: fun _ ->
      assert_equal ~printer:print_list [ "dairy" ]
        (Answers.get_restrict (ans ~restrict:[ "Dairy"; "None of these" ] ())) );
    ( "all four restrictions, in order" >:: fun _ ->
      assert_equal ~printer:print_list
        [ "gluten"; "dairy"; "meat"; "poultry" ]
        (Answers.get_restrict
           (ans ~restrict:[ "Gluten"; "Dairy"; "Meat"; "Poultry" ] ())) );
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
        Answers.make ~flavor:Answers.Sweet ~restrict:[ "dairy" ] ~time:30
          ~serving:2
      in
      assert_equal Answers.Sweet (Answers.get_flavor a);
      assert_equal [ "dairy" ] (Answers.get_restrict a);
      assert_equal 30 (Answers.get_time a);
      assert_equal 2 (Answers.get_serving a) );
    ( "make rejects time 0" >:: fun _ ->
      assert_raises (Invalid_argument "Time must be positive") (fun () ->
          Answers.make ~flavor:Answers.Sweet ~restrict:[] ~time:0 ~serving:1) );
    ( "make rejects serving 0" >:: fun _ ->
      assert_raises (Invalid_argument "Serving must be positive") (fun () ->
          Answers.make ~flavor:Answers.Sweet ~restrict:[] ~time:30 ~serving:0)
    );
    ( "all time and serving options are positive" >:: fun _ ->
      List.iter
        (fun (label, n) -> assert_bool label (n > 0))
        (Answers.time_options @ Answers.serving_options) );
    ( "restriction tags are lowercase, like the recipe JSON" >:: fun _ ->
      List.iter
        (fun (label, tags) ->
          List.iter
            (fun t -> assert_equal ~msg:label (String.lowercase_ascii t) t)
            tags)
        Answers.restrict_options );
  ]

let tests =
  "recipebook"
  >::: [
         "questions" >::: question_tests;
         "to_answers" >::: to_answers_tests;
         "answers" >::: answers_tests;
       ]

let _ = run_test_tt_main tests
