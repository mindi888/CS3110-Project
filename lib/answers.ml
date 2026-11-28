type flavor =
  | Sweet
  | Savory
  | Either


(* ANSWER PAIRINGS *)

let flavor_options =
  [ ("Sweet", Sweet);
    ("Savory", Savory);
    ("No preference", Either) ]

let allergens_options =
  [ ("Gluten", [ "gluten" ]);
    ("Dairy", [ "dairy" ]);
    ("Eggs", [ "eggs" ]);
    ("Shellfish", [ "shellfish" ]);
    ("Fish", [ "fish" ]);
    ("Sesame", [ "sesame" ]);
    ("Nuts", [ "nuts" ]);
    ("None of these", []) ]

let restrict_options = 
  [ ("Kosher", [ "kosher" ]);
    ("Vegan", [ "vegan" ]);
    ("Vegetarian", [ "vegetarian" ]);
    ("Halal", [ "halal" ]);
    ("None of these", []) ]

let time_options =
  [ ("Under 30 minutes", 30);
    ("Under 1 hour", 60);
    ("Under 2 hours", 120);
    ("Doesn't matter", max_int) ]

let serving_options =
  List.map (fun n -> (string_of_int n, n)) [ 1; 2; 3; 4; 6; 8; 12 ]


type answers = {
  flavor : flavor;
  allergens : string list;
  restrict : string list; 
  time : int;
  serving : int;  
}

let check_positive name n = 
  if n < 1 then invalid_arg (name ^ " must be positive")

let make ~flavor ~allergens ~restrict ~time ~serving =
  check_positive "Time" time;
  check_positive "Serving" serving;
  { flavor; allergens; restrict; time; serving }

let get_flavor a = a.flavor
let get_allergens a = a.allergens
let get_restrict a = a.restrict
let get_time a = a.time
let get_serving a = a.serving
