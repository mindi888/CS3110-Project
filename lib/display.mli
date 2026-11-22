(** [welcome ()] prints the welcome message at the top of the program.
    It takes nothing and returns nothing. It only prints. *)
val welcome : unit -> unit

(** [collect_answers ()] asks the user all four questions (flavor,
    restrictions, cook time, servings), one after another, and returns
    their answers bundled together.
    If the user types something invalid, it asks again until they get it
    right. If they press Ctrl-D, the program exits. *)
val collect_answers : unit -> allergens:string list -> Answers.answers

(** [show_recipe_names rs] prints a numbered list of the recipe names in
    [rs]. If the list is empty, it prints "No recipes matched" instead. *)
val show_recipe_names : Recipe.recipe list -> unit

(** [ask_restart ()] asks "Start over?" and returns [true] for yes, [false]
    for no. It keeps asking until the user types y or n. *)
val ask_restart : unit -> bool