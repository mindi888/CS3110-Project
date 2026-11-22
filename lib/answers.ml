type flavor =
  | Sweet
  | Savory
  | Either

(* 
type restriction =
  | Gluten
  | Dairy
  | Meat
  | Poultry *)

(* ANSWER PAIRINGS *)

let flavor_options =
  [ ("Sweet", Sweet);
    ("Savory", Savory);
    ("No preference", Either) ]

let restrict_options =
  [ ("Gluten", [ "gluten" ]);
    ("Dairy", [ "dairy" ]);
    ("Meat", [ "meat" ]);
    ("Poultry", [ "poultry" ]);
    ("None of these", []) ]

let time_options =
  [ ("Under 30 minutes", 30);
    ("Under 1 hour", 60);
    ("Under 2 hours", 120);
    ("Doesn't matter", max_int) ]

(* TODO: Still not sure what choices this should/how to represent them *)
let serving_options =
  List.map (fun n -> (string_of_int n, n)) [ 1; 2; 3; 4; 6; 8; 12 ]


type answers = {
  flavor : flavor;
  restrict : string list; 
  time : int;
  serving : int;  
}

let check_positive name n = 
  if n < 1 then invalid_arg (name ^ " must be positive")

let make ~flavor ~restrict ~time ~serving =
  check_positive "Time" time;
  check_positive "Serving" serving;
  { flavor; restrict; time; serving }

let get_flavor a = a.flavor
let get_restrict a = a.restrict
let get_time a = a.time
let get_serving a = a.serving