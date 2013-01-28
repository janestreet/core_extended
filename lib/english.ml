open Core.Std

let parse_int = function
  | "one"       -> 1
  | "two"       -> 2
  | "three"     -> 3
  | "four"      -> 4
  | "five"      -> 5
  | "six"       -> 6
  | "seven"     -> 7
  | "eight"     -> 8
  | "nine"      -> 9
  | "ten"       -> 10
  | "eleven"    -> 11
  | "twelve"    -> 12
  | "thirteen"  -> 13
  | "fourteen"  -> 14
  | "fifteen"   -> 15
  | "sixteen"   -> 16
  | "seventeen" -> 17
  | "eighteen"  -> 18
  | "nineteen"  -> 19
  | "twenty"    -> 20
  | s -> int_of_string s

let failure which s =
  let which_s =
    match which with
    | `date -> "date"
    | `time -> "time"
  in
  invalid_argf "Unrecognized %s format \"%s\"" which_s s ()

let add_years d i = Date.add_months d (12 * i)

let parse_date dt =
  let dt' = String.lowercase dt in
  let failure () = failure `date dt in
  match dt' with
  | "today" -> Date.today ()
  | "yesterday" -> Date.add_days (Date.today ()) (-1)
  | "tomorrow" -> Date.add_days (Date.today ()) 1
  | _ ->
    try
      Date.of_string dt
    with
    | Invalid_argument _ ->
      try
        match String.split_on_chars dt' ~on:[ ' '; '\t'; '\n'; '\r'; '_' ] with
        | [num; "days"]
        | [num; "days";     "hence"] -> Date.add_days     (Date.today ()) (parse_int num)
        | [num; "weekdays"]
        | [num; "weekdays"; "hence"] -> Date.add_weekdays (Date.today ()) (parse_int num)
        | [num; "months"]
        | [num; "months";   "hence"] -> Date.add_months   (Date.today ()) (parse_int num)
        | [num; "years"]
        | [num; "years";    "hence"] -> add_years         (Date.today ()) (parse_int num)
        | [num; "days";     "ago"] ->   Date.add_days     (Date.today ()) ( -(parse_int num))
        | [num; "weekdays"; "ago"] ->   Date.add_weekdays (Date.today ()) ( -(parse_int num))
        | [num; "months";   "ago"] ->   Date.add_months   (Date.today ()) ( -(parse_int num))
        | [num; "years";    "ago"] ->   add_years         (Date.today ()) ( -(parse_int num))
        | _ ->
          failure ()
      with
      | _ -> failure ()

let parse_time ts =
  let failure () = failure `time ts in
  try
    Time.of_string ts
  with
    _ ->
      let words = Array.of_list (String.split ts ~on:' ') in
      match Array.findi words ~f:(fun _i word -> word = "at") with
      | None -> failure ()
      | Some (idx, _) ->
        let range_to_string idx1 idx2 = String.concat (
          Array.to_list (Array.slice words idx1 idx2)
        ) ~sep:" "
        in
        let date = parse_date (range_to_string 0 idx) in
        let rest = range_to_string (idx + 1) 0 in
        Time.of_string (Date.to_string date ^ " " ^ rest)
