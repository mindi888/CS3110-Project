(** Prints a row of 52 dashes. It's a divider so the screen looks tidy. *)
let line () = print_endline (String.make 52 '-')

let welcome () =
  line ();
  print_endline "  Welcome to your personalized recipe book!";
  print_endline "  Answer a few questions and we'll find recipes for you.";
  line ()

(** Reads one line the user typed and trims extra spaces. Returns Some text if 
  they typed something. Returns None if the input ended (Ctrl-D). This avoids a crash. *)
let read_line_opt () =
  try Some (String.trim (read_line ())) with End_of_file -> None

(** Shows prompt and waits for a whole number between lo and hi. If the user types letters, 
  nothing, or a number that's too big or small, it says "Please enter a number from lo to hi" 
  and asks again. It only returns once the number is valid. *)
let rec read_int_in_range ~lo ~hi prompt =
  print_string prompt;
  flush stdout;
  match read_line_opt () with
  | None -> exit 0
  | Some s -> (
      match int_of_string_opt s with
      | Some n when n >= lo && n <= hi -> n
      | _ ->
          Printf.printf "Please enter a number from %d to %d.\n" lo hi;
          read_int_in_range ~lo ~hi prompt)

(** Prints the answer choices as a numbered list: 1. Sweet, 2. Savory, 
  and so on. options is a list of (label, value) pairs, and it only shows the labels. *)
let print_choices options =
  List.iteri (fun i (label, _) -> Printf.printf "  %d. %s\n" (i + 1) label) options

(* Asks one question where the user picks exactly one answer (flavor, time, servings). 
  It prints the question, shows the numbered choices, and reads their pick. Then it returns 
  the value attached to that choice, not the label. For example, picking "Under 30 min" returns 30. *)
let ask_single q options =
  print_newline ();
  print_endline (Questions.get_prompt q);
  print_choices options;
  let n = read_int_in_range ~lo:1 ~hi:(List.length options) "> " in
  snd (List.nth options (n - 1))

(* Asks one question where the user can pick several answers (restrictions). The user types numbers 
  separated by commas, like 1,3, or presses Enter to pick none. It returns all the chosen tags combined 
  into one list with no duplicates. If any number is invalid, it asks again. *)
let rec ask_multi q options =
  print_newline ();
  print_endline (Questions.get_prompt q);
  print_choices options;
  print_string "Enter numbers separated by commas, or press Enter for none: ";
  flush stdout;
  match read_line_opt () with
  | None -> exit 0
  | Some "" -> []
  | Some s ->
      let n = List.length options in
      let nums =
        String.split_on_char ',' s
        |> List.map (fun p -> int_of_string_opt (String.trim p))
      in
      let valid = function Some k -> k >= 1 && k <= n | None -> false in
      if List.for_all valid nums then
        nums |> List.filter_map Fun.id |> List.sort_uniq compare
        |> List.concat_map (fun k -> snd (List.nth options (k - 1)))
        |> List.sort_uniq compare
      else begin
        Printf.printf "Please enter numbers from 1 to %d, like 1,3.\n" n;
        ask_multi q options
      end

let collect_answers () =
  let flavor = ask_single Questions.flavor_q Answers.flavor_options in
  let restrict = ask_multi Questions.restrict_q Answers.restrict_options in
  let time = ask_single Questions.time_q Answers.time_options in
  let serving = ask_single Questions.serving_q Answers.serving_options in
  Answers.make ~flavor ~restrict ~time ~serving

(* Temporary: names only, since Recipe exposes only get_name for now. *)
let show_recipe_names rs =
  print_newline ();
  line ();
  (match rs with
  | [] -> print_endline "No recipes matched. Try changing an answer."
  | _ ->
      print_endline "Recipes that match:";
      List.iteri (fun i r -> Printf.printf "  %d. %s\n" (i + 1) (Recipe.get_name r)) rs);
  line ()