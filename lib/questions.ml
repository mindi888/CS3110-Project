type style =
  | Buttons
  | Checkboxes
  | Dropdown

type question = {
  prompt : string;
  choices : string list;
  style : style;
}

let prompt q = q.prompt
let choices q = q.choices
let style q = q.style

let flavor_q =
  {
    prompt = "What flavor would you prefer?";
    choices = [ "Sweet"; "Savory"; "No preference" ];
    style = Buttons;
  }

let restriction_q =
  {
    prompt = "Do you have any dietary restrictions?";
    choices =
      [ "Gluten-free"; "Dairy-free"; "No meat"; "No poultry"; "None of these" ];
    style = Checkboxes;
  }

let time_q =
  {
    prompt = "How much time would you like to spend cooking?";
    choices =
      [ "Under 30 minutes"; "Under 1 hour"; "Under 2 hours"; "Doesn't matter" ];
    style = Buttons;
  }

let serving_q =
  {
    prompt = "How many servings would you like?";
    choices = [ "1"; "2"; "3"; "4"; "6"; "8"; "12" ];
    style = Dropdown;
  }

let questions = [ flavor_q; restriction_q; time_q; serving_q ]

