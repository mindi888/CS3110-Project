type style =
  | Buttons
  | Checkboxes
  | Dropdown

type question = {
  prompt : string;
  choices : string list;
  style : style;
}

let get_prompt q = q.prompt
let get_choices q = q.choices
let get_style q = q.style

(* PRIVATE HELPERS *)

(* getting the answer choices to display*)
let labels options = List.map (fun (label, _) -> label) options

(* getting the option the user selected *)
let parse options label = List.assoc label options

(* getting the multiple options the user selected *)
(* NOTE: If user selects None of the above, then the display must uncheck 
previously checked options *)
let parse_multiple options label = List.concat_map (parse options) label

(* QUESTIONS *)

let flavor_q =
  {
    prompt = "What flavor would you prefer?";
    choices = labels Answers.flavor_options;
    style = Buttons;
  }

let allergens_q =
  {
    prompt = "Do you have any allergies?";
    choices = labels Answers.allergens_options;
    style = Checkboxes;
  }

let restrict_q =
  {
    prompt = "Do you have any dietary restrictions?";
    choices = labels Answers.restrict_options;
    style = Checkboxes;
  }

let time_q =
  {
    prompt = "How much time would you like to spend cooking?";
    choices = labels Answers.time_options;
    style = Buttons;
  }

let serving_q =
  {
    prompt = "How many servings would you like?";
    choices = labels Answers.serving_options;
    style = Dropdown;
  }

let questions = [ flavor_q; allergens_q; restrict_q; time_q; serving_q ]

let to_answers ~flavor ~allergens ~restrict ~time ~serving =
  Answers.make
    ~flavor:(parse Answers.flavor_options flavor)
    ~allergens:(parse_multiple Answers.allergens_options allergens)
    ~restrict:(parse_multiple Answers.restrict_options restrict)
    ~time:(parse Answers.time_options time)
    ~serving:(parse Answers.serving_options serving)

let selections_to_answers sel =
  let multi_ans q = List.assq q sel in
  let single_ans q =
    match multi_ans q with
    | [ label ] -> label
    | _ -> invalid_arg "Needs exactly one choice, given multiple"
  in
  to_answers ~flavor:(single_ans flavor_q) ~allergens:(multi_ans allergens_q)
    ~restrict:(multi_ans restrict_q) ~time:(single_ans time_q)
    ~serving:(single_ans serving_q)
