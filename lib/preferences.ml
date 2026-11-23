type labels = {
  allergens : string list;
  flavor : string;
  restrict : string list;
  time : string;
  serving : string;
}

(** [save filename answers] saves [answers] to the file named [filename]. If
    [filename] already exists, its previous contents are replaced. *)
let save filename answers =
  let out_channel = open_out_bin filename in
  Marshal.to_channel out_channel answers [];
  close_out out_channel

(** [load filename] returns [Some answers] if [filename] exists and contains
    saved preferences. Returns [None] if the file does not exist. *)
let load filename =
  if Sys.file_exists filename then (
    let in_channel = open_in_bin filename in
    let answers = Marshal.from_channel in_channel in
    close_in in_channel;
    Some answers)
  else None

(** [flavor_label flavor] returns the display label corresponding to [flavor].
*)
let flavor_label = function
  | Answers.Sweet -> "Sweet"
  | Answers.Savory -> "Savory"
  | Answers.Either -> "No preference"

(** [label_of_value options value] returns the label paired with [value] in
    [options]. Requires [value] is paired with a label in [options]. *)
let label_of_value options value =
  fst (List.find (fun (_, v) -> v = value) options)

(** [allergen_labels allergens] returns the display labels corresponding to the
    allergen tags in [allergens]. If [allergens] is empty, it returns
    [["None of these"]]. *)
let allergen_labels allergens =
  if allergens = [] then [ "None of these" ]
  else
    Answers.allergens_options
    |> List.filter_map (fun (label, tags) ->
        if List.exists (fun tag -> List.mem tag allergens) tags then Some label
        else None)

(** [time_label time] returns the display label corresponding to [time]. *)
let time_label time = label_of_value Answers.time_options time

(** [serving_label serving] returns the display label corresponding to
    [serving]. *)
let serving_label serving = label_of_value Answers.serving_options serving

(** [restriction_labels restrictions] returns the display labels corresponding
    to the restriction tags in [restrictions]. If [restrictions] is empty, it
    returns [["None of these"]]. *)
let restriction_labels restrictions =
  if restrictions = [] then [ "None of these" ]
  else
    Answers.restrict_options
    |> List.filter_map (fun (label, tags) ->
        if List.exists (fun tag -> List.mem tag restrictions) tags then
          Some label
        else None)

(** [to_labels answers] converts the stored values in [answers] into the string
    labels used by the questionnaire UI. *)
let to_labels answers =
  {
    allergens = allergen_labels (Answers.get_allergens answers);
    flavor = flavor_label (Answers.get_flavor answers);
    restrict = restriction_labels (Answers.get_restrict answers);
    time = time_label (Answers.get_time answers);
    serving = serving_label (Answers.get_serving answers);
  }
