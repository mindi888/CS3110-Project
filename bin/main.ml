open Recipebook

let rec run recipes =
  let answers = Display.final_answers () in
  Display.show_recipe_names (Scoring.filter answers recipes);
  if Display.ask_restart () then run recipes else print_endline "Happy cooking!"

let () =
  Display.welcome ();
  run (Recipe.load_all "data")
