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

let create s =
  let json = Yojson.Basic.from_file s in
  {
    name = json |> member "name" |> to_string;
    flavor = json |> member "flavor" |> to_string;
    restricts = json |> member "restrictions" |> to_list |> List.map to_string;
    cook_time = json |> member "cooking_time_min" |> to_string |> int_of_string;
    servings = json |> member "cooking_time_min" |> to_string |> int_of_string;
    ingredients = [];
    directions = json |> member "directions" |> to_list |> List.map to_string;
  }

(*let get_name r = r.name let get_flavor r = r.flavor let get_restrict r =
  List.fold_left (fun acc s -> acc ^ ", " ^ s) "" r.restricts

  let cook_time r = r.cook_time let servings r = r.servings*)
