type style =
  | Buttons
  | Checkboxes
  | Dropdown

type question

val get_prompt : question -> string
(** [get_prompt q] is the text of [q], the question. *)

val get_choices : question -> string list
(** [get_choices q] is the labels the user can select for [q], in display order.
*)

val get_style : question -> style
(** [get_style q] is how the choices of [q] should be displayed. *)

val flavor_q : question
val allergens_q : question
val restrict_q : question
val time_q : question
val serving_q : question
val questions : question list

val selections_to_answers : (question * string list) list -> Answers.answers
(** [selections_to_answers sel] is the answers built from the labels the user
    picked, where [sel] pairs each question with its selected labels: exactly
    one for [Buttons] and [Dropdown], any number for [Checkboxes].
    Requires: every question in [questions] is in [sel], and every label is one
    of that question's [get_choices].
    Raises: [Not_found] if a question is missing from [sel], and
    [Invalid_argument] if a single-choice question has more or fewer than one
    label. *)

val to_answers :
  flavor:string ->
  allergens:string list ->
  restrict:string list ->
  time:string ->
  serving:string ->
  Answers.answers
(** [to_answers ~flavor ~allergens ~restrict ~time ~serving] is the answers built from the
    labels the user selected. *)
