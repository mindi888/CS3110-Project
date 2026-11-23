val welcome : unit -> unit
(** [welcome ()] prints the welcome message at the top of the program. It takes
    nothing and returns nothing. It only prints. *)

val collect_answers : unit -> Answers.answers
(** [collect_answers ()] asks the user all recipe preference questions, creates
    an [Answers.answers] value from their responses, and gives the user the
    option to save those answers as their preferences. Returns the answers
    selected by the user. *)

val final_answers : unit -> Answers.answers
(** [final_answers ()] checks whether saved preferences exist.

    If saved preferences exist, it asks the user whether they want to use them.
    If the user chooses yes, it returns the saved preferences. Otherwise, it
    calls [collect_answers ()] to collect new answers. If no saved preferences
    exist, it also calls [collect_answers ()]. *)

val show_recipe_names : Recipe.recipe list -> unit
(** [show_recipe_names rs] prints a numbered list of the recipe names in [rs].
    If the list is empty, it prints "No recipes matched" instead. *)

val ask_restart : unit -> bool
(** [ask_restart ()] asks "Start over?" and returns [true] for yes, [false] for
    no. It keeps asking until the user types y or n. *)
