type labels = {
  flavor : string;
  restrict : string list;
  time : string;
  serving : string;
}

val save : string -> Answers.answers -> unit
(** [save filename answers] saves [answers] to [filename]. *)

val load : string -> Answers.answers option
(** [load filename] returns the saved answers in [filename], or [None] if the
    file does not exist. *)

val to_labels : Answers.answers -> labels
(** [to_labels answers] converts [answers] into the display labels used by the
    questionnaire UI. *)
