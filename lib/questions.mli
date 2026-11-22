type style =
  | Buttons
  | Checkboxes
  | Dropdown

type question

val prompt : question -> string
val choices : question -> string list
val style : question -> style

val flavor_q : question
val restriction_q : question
val time_q : question
val serving_q : question

val questions : question list

val parse_flavor : string -> Answers.flavor
val parse_restrictions : string -> Answers.restriction list
val parse_time : string -> Answers.time
val parse_serving : string -> int