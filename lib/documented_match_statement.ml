open Core.Std

type ('input,'output) case = {
  pattern : 'input list;
  documentation : string;
  value : 'output;
}

let map_case case ~f_pattern ~f_value =
  { case with
    pattern = List.map case.pattern ~f:f_pattern;
    value = f_value case.value;
  }

type ('input,'output) t = {
  specific_cases : ('input, unit -> 'output) case list;
  catchall_case :
    [`Used of ([ `Catchall ], 'input -> 'output ) case
        | `Unused of unit -> 'output];
}

let map t ~f =
  { specific_cases = List.map t.specific_cases
      ~f:(map_case ~f_pattern:ident ~f_value:(fun value -> fun () -> f (value ())));
    catchall_case =
      begin
        match t.catchall_case with
        | `Unused g -> `Unused (fun () -> f (g ()))
        | `Used case ->
          `Used (map_case case ~f_pattern:ident ~f_value:(fun g -> (fun k -> f (g k))))
      end;
  }

let map_cases l ~f =
  List.map l ~f:(map_case ~f_pattern:ident ~f_value:(fun g () -> f (g ())))

let map_pattern t ~f1 ~f2 =
  { specific_cases = List.map t.specific_cases ~f:(map_case ~f_pattern:f1 ~f_value:ident);
    catchall_case = begin
      match t.catchall_case with
      | `Unused x -> `Unused x
      | `Used case ->
        `Used (map_case case ~f_pattern:ident ~f_value:(fun g -> (fun k -> (g (f2 k)))))
    end;
  }

let prepend ~specific_cases t =
  { t with
    specific_cases = specific_cases @ t.specific_cases;
  }

let match_ t x =
  match List.filter t.specific_cases
    ~f:(fun { pattern = x'; _ } -> List.exists x' ~f:(fun y -> x = y)) with
  | case1::case2::_ -> failwithf "pattern appears twice in documented_match (%s,%s)"
    case1.documentation case2.documentation ()
  | [case] -> case.value ()
  | [] -> begin
    match t.catchall_case with
    | `Used case -> case.value x
    | `Unused f -> f ()
  end

let documentation t ~input_to_string ~title =
  let to_multiline_doc l =
    let to_multiline_doc (left,right) =
      List.mapi (String.split right ~on:'\n')
        ~f:(fun i right' -> if i = 0 then (left,right') else ("",right'))
    in
    List.concat_map l ~f:to_multiline_doc
  in
  let specific_case_lines =
    List.map t.specific_cases ~f:(fun case ->
      String.concat ~sep:", " (List.map ~f:input_to_string case.pattern),
      case.documentation
    ) |! to_multiline_doc
  in
  let catchall_case_lines =
    to_multiline_doc (
      match t.catchall_case with
      | `Unused _ -> []
      | `Used catchall -> [
        "any other key", catchall.documentation
      ]
    )
  in
  let header = [
    "Key", "Action";
    "---", "------";
  ]
  in
  let lines = header @ specific_case_lines @ catchall_case_lines in
  let left_length = List.fold lines ~init:0
    ~f:(fun max_length (key_string,_) -> max max_length (String.length key_string))
  in
  title :: "" ::
    List.map lines ~f:(fun (key_string,documentation) ->
      let str_len = String.length key_string in
      key_string ^ String.make (left_length - str_len + 5) ' ' ^ documentation)

