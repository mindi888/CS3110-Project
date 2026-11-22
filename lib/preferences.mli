val save : string -> Answers.answers -> unit
(** [save filename answers] saves [answers] to [filename]. *)

val load : string -> Answers.answers option
(** [load filename] returns the saved answers in [filename], or [None] if the
    file does not exist. *)
