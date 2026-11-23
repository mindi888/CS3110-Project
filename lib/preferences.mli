type labels = {
  allergens : string list;
  flavor : string;
  restrict : string list;
  time : string;
  serving : string;
}

val save : string -> Answers.answers -> unit
(** [save filename answers] saves [answers] to the file named [filename]. If the
    file already exists, its contents are replaced. *)

val load : string -> Answers.answers option
(** [load filename] returns [Some answers] if [filename] contains saved
    preferences. Returns [None] if the file does not exist. *)

val to_labels : Answers.answers -> labels
(** [to_labels answers] converts the stored values in [answers] into the string
    labels shown in the questionnaire UI. For example, a savory flavor becomes
    ["Savory"] and a time value of [30] becomes ["Under 30 minutes"]. *)
