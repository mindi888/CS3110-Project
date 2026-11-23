type labels = {
  flavor : string;
  restrict : string list;
  time : string;
  serving : string;
}

(* ---------- Saving / loading ---------- *)

let save filename answers =
  let out_channel = open_out_bin filename in
  Marshal.to_channel out_channel answers [];
  close_out out_channel

let load filename =
  if Sys.file_exists filename then (
    let in_channel = open_in_bin filename in
    let answers = Marshal.from_channel in_channel in
    close_in in_channel;
    Some answers)
  else None

(* ---------- PRIVATE HELPERS ---------- *)

(** [flavor_label flavor] returns the display label corresponding to [flavor].
*)
let flavor_label = function
  | Answers.Sweet -> "Sweet"
  | Answers.Savory -> "Savory"
  | Answers.Either -> "No preference"

(** [label_of_value options value] returns the label paired with [value] in
    [options]. *)
let label_of_value options value =
  fst (List.find (fun (_, v) -> v = value) options)

(** [time_label time] returns the display label corresponding to [time]. *)
let time_label time = label_of_value Answers.time_options time

(** [serving_label serving] returns the display label corresponding to
    [serving]. *)
let serving_label serving = label_of_value Answers.serving_options serving

(** [restriction_labels restrictions] returns the display labels corresponding
    to [restrictions]. *)
let restriction_labels restrictions =
  if restrictions = [] then [ "None of these" ]
  else
    Answers.restrict_options
    |> List.filter_map (fun (label, tags) ->
        if tags <> [] && List.exists (fun tag -> List.mem tag restrictions) tags
        then Some label
        else None)

(** [to_labels answers] converts saved answers back into the labels expected by
    the questionnaire UI. *)
let to_labels answers =
  {
    flavor = flavor_label (Answers.get_flavor answers);
    restrict = restriction_labels (Answers.get_restrict answers);
    time = time_label (Answers.get_time answers);
    serving = serving_label (Answers.get_serving answers);
  }
