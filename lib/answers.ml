type flavor =
  | Sweet
  | Savory
  | Either

type restriction =
  | Gluten
  | Dairy
  | Meat
  | Poultry

type time =
  | Under30
  | Under60
  | Under120
  | Anytime

type answer = {
  flavor : flavor;
  restrictions : restriction list;  (** [] if the user selected none *)
  time : time;
  serving : int;  (** required, must be > 0 *)
}

let flavor r = r.flavor
let restrictions r = r.restrictions
let time r = r.time
let serving r = r.serving

let make f rs t s =
  if s < 1 then invalid_arg "Serving size must be positive"
  else { flavor = f; restrictions = rs; time = t; serving = s }

let restriction_to_string = function
  | Gluten -> "gluten"
  | Dairy -> "dairy"
  | Meat -> "meat"
  | Poultry -> "poultry"

let time_to_minutes = function
  | Under30 -> 30
  | Under60 -> 60
  | Under60 -> 120
  | Anytime -> max_int

let change_flavor a f = { a with flavor = f }
let change_restrictions a rs = { a with restrictions = rs }
let change_time a t = { a with time = t }

let change_serving a s =
  if s < 1 then invalid_arg "Serving size must be positive"
  else { a with serving = s }
