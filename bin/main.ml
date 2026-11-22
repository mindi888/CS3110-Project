open Recipebook

let () =
  Display.welcome ();
  let answers = Display.collect_answers () in
  let recipes = Recipe.load_all "data" in
  Display.show_recipe_names (Scoring.filter answers recipes)