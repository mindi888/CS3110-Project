open Yojson.Basic.Util

type ingredient = {
  item : string;
  amount : float;
  unit : string;
}

type recipe = {
  name : string;
  flavor : string;
  restricts : string list;
  cook_time : int;
  servings : int;
  ingredients : (string * ingredient list) list;
  directions : string list;
}

let parse_ingredients ing =
  ing
  |> List.map (fun (g, i) -> (g, i |> to_list))
  |> List.map (fun (g, i) ->
      ( g,
        List.map
          (fun x ->
            {
              item = x |> member "item" |> to_string;
              amount = x |> member "amount" |> to_number;
              unit = x |> member "unit" |> to_string;
            })
          i ))

let create json =
  {
    name = json |> member "name" |> to_string;
    flavor = json |> member "flavor" |> to_string;
    restricts = json |> member "restrictions" |> to_list |> List.map to_string;
    cook_time = json |> member "cooking_time_min" |> to_int;
    servings = json |> member "servings" |> to_int;
    ingredients = json |> member "ingredients" |> to_assoc |> parse_ingredients;
    directions = json |> member "directions" |> to_list |> List.map to_string;
  }

let load_all dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".json")
  |> List.map (fun f -> Filename.concat dir f)
  |> List.map Yojson.Basic.from_file
  |> List.map create

let get_name r = r.name
let get_flavor r = r.flavor
let get_restrict r = r.restricts
let cook_time r = r.cook_time
let servings r = r.servings
let get_ing r = r.ingredients
let get_directions r = r.directions
