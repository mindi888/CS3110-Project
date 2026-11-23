open OUnit2
open Recipebook

let string_list = List.fold_left (fun acc s -> acc ^ "\"" ^ s ^ "\"; ") ""
let print_list l = "[ " ^ string_list l ^ "]"

let preferences_tests =
  [
    ( "saved preferences load with the same values" >:: fun _ ->
      let original =
        Answers.make ~flavor:Answers.Savory ~restrict:[ "dairy" ] ~time:30
          ~serving:4 ~allergens:[]
      in
      let filename = Filename.temp_file "preferences_test" ".dat" in

      Preferences.save filename original;

      match Preferences.load filename with
      | None ->
          Sys.remove filename;
          assert_failure "Preferences were not saved"
      | Some loaded ->
          assert_equal Answers.Savory (Answers.get_flavor loaded);
          assert_equal ~printer:print_list [ "dairy" ]
            (Answers.get_restrict loaded);
          assert_equal ~printer:string_of_int 30 (Answers.get_time loaded);
          assert_equal ~printer:string_of_int 4 (Answers.get_serving loaded);
          Sys.remove filename );
    ( "loading a nonexistent file gives None" >:: fun _ ->
      let filename = "file_that_should_not_exist.dat" in
      if Sys.file_exists filename then Sys.remove filename;
      assert_equal None (Preferences.load filename) );
  ]

let labels_tests =
  [
    ( "answers convert back to display labels" >:: fun _ ->
      let a =
        Answers.make ~flavor:Answers.Savory ~restrict:[ "vegan" ] ~time:30
          ~serving:4 ~allergens:[]
      in

      let labels = Preferences.to_labels a in

      assert_equal ~printer:(fun x -> x) "Savory" labels.flavor;
      assert_equal ~printer:print_list [ "Vegan" ] labels.restrict;
      assert_equal ~printer:(fun x -> x) "Under 30 minutes" labels.time;
      assert_equal ~printer:(fun x -> x) "4" labels.serving );
  ]

let tests =
  "preferences"
  >::: [
         "saving and loading" >::: preferences_tests; "labels" >::: labels_tests;
       ]

let _ = run_test_tt_main tests
