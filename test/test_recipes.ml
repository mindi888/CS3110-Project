open OUnit2
open Recipebook

let recipes = Recipe.load_all "../data"

let rec checkIng num lst =
  match num, lst with 
  |[], [] -> true
  |len::rest, (_,l):: r -> (len = List.length l) && checkIng rest r
  |_ -> false

let testMatcha = 
[ ( "Test if matcha cookie recipes were properly parsed" >:: fun _ ->
  let matcha = List.find (fun r -> Recipe.get_name r = "Matcha Cookies") recipes in
  assert_equal "sweet" (Recipe.get_flavor matcha); 
  assert_equal ["gluten"; "dairy"] (Recipe.get_restrict matcha);
  assert_equal 35 (Recipe.cook_time matcha);
  assert_equal 24 (Recipe.servings matcha);
  let directions = Recipe.get_directions matcha in 
  assert_equal "Preheat oven to 350° F" (List.hd directions);
  assert_equal "Optional: for frog faces, melt dark chocolate and use piping bag to decorate" (List.hd (List.rev directions));
  let ing = Recipe.get_ing matcha in 
  let headers = List.map fst ing in 
  assert_equal ["dry";"wet"; "optional topping"] headers;
  assert_bool "check if group sizes wrong" (checkIng [4;5;2] ing))]

let testMac = 
[ ( "Test if the vegan mac and cheese were properly parsed" >:: fun _ ->
  let mac = List.find (fun r -> Recipe.get_name r = "Vegan Mac and Cheese") recipes in
  assert_equal "savory" (Recipe.get_flavor mac); 
  assert_equal ["gluten"] (Recipe.get_restrict mac);
  assert_equal 45 (Recipe.cook_time mac);
  assert_equal 8 (Recipe.servings mac);
  let directions = Recipe.get_directions mac in 
  assert_equal "Bring a large pot of water to a boil for the macaroni. Add generous amount of salt and cook 1 minute less than time indicated on box" (List.hd directions);
  assert_equal "Bake for 20 minutes or until topping is golden brown. Once done, take out of the oven and cool for 10 to 20 minutes" (List.hd (List.rev directions));
  let ing = Recipe.get_ing mac in 
  let headers = List.map fst ing in 
  assert_equal ["Cheese Sauce";"Crispy Topping"; "Macaroni"] headers;
  assert_bool "check if group sizes wrong" (checkIng [15;4;2] ing))]

let tests = "test suite" >::: testMatcha @ testMac
let _ = run_test_tt_main tests