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
val restrict_q : question
val time_q : question
val serving_q : question
val questions : question list

val to_answers :
  flavor:string ->
  restrict:string list ->
  time:string ->
  serving:string ->
  Answers.answers
(** [to_answers ~flavor ~restrict ~time ~serving] is the answers built from the
    labels the user selected. *)
