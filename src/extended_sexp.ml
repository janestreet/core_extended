open Core
open Sexplib.Sexp

let is_atom = function Atom _ -> true | List _ -> false
let is_list = function Atom _ -> false | List _ -> true

let list l = List l
let atom a = Atom a

let comment s =
  match (String.split s ~on:'\n') with
  | [] -> ""
  | h::l -> (String.concat ~sep:"\n;; " ((";; "^h)::l)) ^ "\n"

open Pp.Infix
let indent = 2

let rec pp_hum' fmt = function
  | Atom s -> Format.pp_print_string fmt (Sexplib.Pre_sexp.mach_maybe_esc_str s)
  | List l when List.for_all ~f:is_atom l ->
    Format.pp_open_hovbox fmt 2;
    pp_hum_rest' fmt l
  | List l ->
      Format.pp_open_hvbox fmt 2;
      pp_hum_rest' fmt l
and pp_hum_rest' fmt l =
    Format.pp_print_string fmt "(";
    let rec loop = function
      | [] -> ()
      | [v] -> pp_hum' fmt v
      | h::t ->
          pp_hum' fmt h;
          Format.pp_print_space fmt ();
          loop t
    in
    loop l;
    Format.pp_print_string fmt ")";
    Format.pp_close_box fmt ()


let rec format = function
  | Atom s -> Pp.text (Sexplib.Pre_sexp.mach_maybe_esc_str s)
  | List l when List.for_all ~f:is_atom l -> Pp.fgrp (par l)
  | List l -> Pp.agrp (par l)
and par l =
  Pp.text "(" $ Pp.nest indent (Pp.list ~sep:Pp.break ~f:format l) $ Pp.text ")"

let to_string_hum' sexp = Pp.to_string (format sexp)

module Diff :
sig
  type t
  val print : ?oc:Out_channel.t -> t -> unit
  val to_buffer : t -> Buffer.t
  val to_string : t -> string
  val of_sexps : original:Sexp.t -> updated:Sexp.t -> t option
end
=
struct
  type t =
      | Different of ([`Original of Sexp.t] * [`Updated of Sexp.t])
      | List of t list
      | Record of record_field list

  and record_field =
      | New_in_updated of Sexp.t
      | Not_in_updated of Sexp.t
      | Bad_match of string * t

  let make_tail make tail acc =
    Some (Record (List.rev (List.rev_map_append ~f:make tail acc)))

  let recf (k, v) = Sexp.List [Sexp.Atom k; v]

  let maybe_record sexps =
    let is_list_of_atom_pairs = function
      | Sexp.List [Sexp.Atom _; _] -> true
      | _ -> false
    in
    sexps <> [] &&
      (List.for_all ~f:is_list_of_atom_pairs sexps)

  let sort_record_fields sexp_list =
    let to_pair = function
      | Sexp.List [Sexp.Atom k; v] -> k, v
      | _ -> assert false  (* impossible *)
    in
    let pairs = List.map ~f:to_pair sexp_list in
    List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2) pairs

  let rec of_record_fields acc pairs_orig pairs_upd =
    match pairs_orig, pairs_upd with
    | [], [] when acc = [] -> None
    | [], [] -> Some (Record (List.rev acc))
    | [], tail -> make_tail (fun kv -> New_in_updated (recf kv)) tail acc
    | tail, [] -> make_tail (fun kv -> Not_in_updated (recf kv)) tail acc
    | (((k_o, v_o) as h_o) :: t_o as l_o), (((k_u, v_u) as h_u) :: t_u as l_u) ->
        let c = String.compare k_o k_u in
        if c = 0 then
          match of_sexps ~original:v_o ~updated:v_u with
          | None -> of_record_fields acc t_o t_u
          | Some diff -> of_record_fields (Bad_match (k_u, diff) :: acc) t_o t_u
        else if c > 0 then of_record_fields (New_in_updated (recf h_u) :: acc) l_o t_u
        else of_record_fields (Not_in_updated (recf h_o) :: acc) t_o l_u

  and of_lists acc original updated =
    match original, updated with
    | [], [] when acc = [] -> None
    | [], [] -> Some (List (List.rev acc))
    | [], _ | _, [] -> assert false  (* impossible *)
    | h_orig :: t_orig, h_upd :: t_upd ->
        match of_sexps ~original:h_orig ~updated:h_upd with
        | None -> of_lists acc t_orig t_upd
        | Some res -> of_lists (res :: acc) t_orig t_upd

  and of_sexps ~original ~updated =
    match original, updated with
    | Sexp.List [], Sexp.List [] -> None
    | Sexp.Atom a1, Sexp.Atom a2 when a1 = a2 -> None
    | Sexp.List orig, Sexp.List upd ->
        if maybe_record orig && maybe_record upd then
          of_record_fields [] (sort_record_fields orig) (sort_record_fields upd)
        else if List.length orig = List.length upd then of_lists [] orig upd
        else Some (Different (`Original original, `Updated updated))
    | _ -> Some (Different (`Original original, `Updated updated))

  let to_buffer diff =
    let buf = Buffer.create 80 in
    let print_string ~tag ~indent str =
      Buffer.add_string buf (Printf.sprintf "%-*s %s\n%!" indent tag str)
    in
    let print_sexp ~tag ~indent sexp =
      print_string ~tag ~indent (Sexp.to_string sexp)
    in
    let rec loop indent = function
      | Different (`Original sexp1, `Updated sexp2) ->
        print_sexp ~tag:"-" ~indent sexp1;
        print_sexp ~tag:"+" ~indent sexp2
      | List lst ->
        print_string ~tag:"" ~indent "(";
        List.iter ~f:(loop (indent + 1)) lst;
        print_string ~tag:"" ~indent ")"
      | Record record_fields ->
        let print_record_field = function
          | New_in_updated sexp -> print_sexp ~tag:"+" ~indent sexp
          | Not_in_updated sexp -> print_sexp ~tag:"-" ~indent sexp
          | Bad_match (key, diff) ->
            print_string ~tag:"" ~indent key;
            loop (indent + 1) diff
        in
        List.iter ~f:print_record_field record_fields;
    in
    loop 0 diff;
    buf

  let to_string diff = Buffer.contents (to_buffer diff)

  let print ?(oc = stdout) diff = Caml.Buffer.output_buffer oc (to_buffer diff)
end

let print_diff ?oc ~original ~updated () =
  Option.iter (Diff.of_sexps ~original ~updated) ~f:(fun diff -> Diff.print ?oc diff)

(* The purpose of this module is just to group this craziness together. *)
module Summarize = struct
  (* An arbitrary distance metric between the nodes of an sexp, which is thought of as a
     tree.  Take a description of the path: `Pos i means move to the ith element of a list,
     `Back i means the current node is the ith element of its parents list and move to that
     parent. *)
  let rec path_depth = function
    | `Found -> 0
    | `Pos (_, path) -> 1 + path_depth path
    | `Back (i, (`Pos (n, path))) -> 1 + (min (abs (n-i)) i) + path_depth path
    | `Back (i, path) -> 1 + min 3 i + path_depth path
  ;;

  let dot_dot_dot = Sexp.Atom "...";;

  (* returns the parts of sexp that are "close" to the part of the sexp that path points
     to. *)
  let rec neighbors sexp path max_distance =
    match sexp, max_distance with
    | Sexp.Atom _, 0 -> failwith "Bug"
    | Sexp.Atom str, depth ->
        (* large atoms are more distant *)
        let length_punishment = float (max (String.length str - 3) 0) /. 10. in
        (* let length_punishment = log (float (String.length str)) /. 1.8 in *)
        let my_distance = float (path_depth path) +. length_punishment in
        if my_distance < float depth
        then Sexp.Atom str
        else dot_dot_dot
    (* performance hack: if a list is going to contain all "..." atoms, then "..." the
       list itself *)
    | Sexp.List _, (0 | 1) -> dot_dot_dot
    | Sexp.List sexps, max_distance ->
      if path_depth path >= max_distance
      then dot_dot_dot
      else
        let sexps =
          List.mapi sexps ~f:(fun i sexp ->
            let new_path =
              match path with
              | `Found | `Back _ ->
                `Back (i, path)
              | `Pos (n, path) ->
                if n = i
                then path
                else `Back (i, `Pos (n, path))
            in
            neighbors sexp new_path max_distance
          )
        in
        let sexps =
          (* consolidate consecutive "..." atoms into one "..." atom *)
          List.fold sexps ~init:[] ~f:(fun accum sexp ->
            match accum with
            | [] -> [ sexp ]
            | hd :: _tl ->
              if phys_equal sexp dot_dot_dot && phys_equal hd dot_dot_dot
              then accum
              else sexp :: accum
          ) |> List.rev
        in
        (* replace "(...)" with just "..." *)
        if sexps = [ dot_dot_dot ] then dot_dot_dot
        else Sexp.List sexps
  ;;

  (* given an sexp, an "erroneous" sub_sexp, and a maximum distance, returns an sexp of
     nodes near sub_sexp. *)
  let summarize_sexp sexp sub_sexp depth =
    let search_result = Sexp.search_physical sexp ~contained:sub_sexp in
    match search_result with
    | `Not_found ->
        failwithf "Sexp %s not found in sexp %s" (Sexp.to_string sub_sexp) (Sexp.to_string sexp) ()
    | (`Found | `Pos _) as path ->
        let subst = Sexp.List [ Sexp.Atom "ERROR-->"; sub_sexp; Sexp.Atom "<--ERROR" ] in
        let annotated_sexp = Sexp.subst_found sexp ~subst path in
        let rec new_path path =
          match path with
          | `Pos (n, path) -> `Pos (n, new_path path)
          | `Found -> `Pos (1, `Found)
        in
        neighbors annotated_sexp (new_path path) depth

  (* Replaced below by a faster estimate of the size *)
  let _sexp_size sexp = String.length (Sexp.to_string sexp);;

  (* FIXME: does not take into account escaping *)
  let rec my_sexp_size = function
    | Sexp.List l ->
        List.fold l ~init:2
          ~f:(fun sum sexp -> sum + my_sexp_size sexp)
    | Sexp.Atom str -> String.length str  (* should really be +2 if spaces present *)
  (* should add 1 for the space between two adjacent atoms *)
  ;;

  (* summarizes sexp to have a maximum string length *)
  let summarize_sexp_length sexp sub_sexp length =
    let full_length = my_sexp_size sexp in
    let is_too_big max_depth =
      let sexp = summarize_sexp sexp sub_sexp max_depth in
      length >= full_length
      || my_sexp_size sexp > length
    in
    let rec binary_search lower_bound upper_bound =
      if upper_bound = Some (lower_bound + 1)
      then lower_bound
      else
        let depth_to_try =
          match upper_bound with
          | None -> lower_bound * 2
          | Some upper_bound -> (lower_bound + upper_bound) / 2
        in
        if is_too_big depth_to_try
        then binary_search lower_bound (Some depth_to_try)
        else binary_search depth_to_try upper_bound
    in
    let perfect_depth = binary_search 1 None in
    summarize_sexp sexp sub_sexp perfect_depth
  ;;
end

let summarize sexp ~sub_sexp ~size =
  match size with
  | `string s ->
      Summarize.summarize_sexp_length sexp sub_sexp s
  | `depth d ->
      Summarize.summarize_sexp sexp sub_sexp d

let of_generated_sexp of_sexp ~original_sexp ~generated_sexp =
  try of_sexp generated_sexp with
  | Of_sexp_error (exn, error_sexp) ->
    let error_sexp =
      if phys_equal error_sexp generated_sexp then
        original_sexp
      else
        error_sexp
    in
    raise (Of_sexp_error (exn, error_sexp))
;;

module Make_explicit_sexp_option (T: sig
  type t [@@deriving sexp]
  val explicit_sexp_option_fields : string list
end) : sig
  type t = T.t [@@deriving sexp]
end = struct
  type t = T.t

  let fail () = failwith "Make_explicit_sexp_option failure"

  let t_of_sexp sexp =
    let sexp =
      match sexp with
      | Sexp.Atom _ -> sexp
      | Sexp.List l when List.exists l ~f:(function
          | Sexp.List [Sexp.Atom _;_] -> false
          | _ -> true
      ) -> sexp
      | Sexp.List l -> Sexp.List (List.filter_map l ~f:(fun field ->
        let name, value =
          match field with
          | Sexp.List [Sexp.Atom name;sexp] -> name,sexp
          | _ -> assert false
        in
        if not (List.mem T.explicit_sexp_option_fields name ~equal:String.equal)
        then Some field
        else
          match value with
          | Sexp.List [] -> None
          | Sexp.List [sexp] -> Some sexp
          | Sexp.Atom _ | Sexp.List (_::_::_) -> fail ()
      ))
    in
    T.t_of_sexp sexp

  let sexp_of_t t =
    let sexp = T.sexp_of_t t in
    let field_names =
      match sexp with
      | Sexp.Atom _ -> fail ()
      | Sexp.List l -> Map.Poly.of_alist_exn (List.map l ~f:(fun field ->
        match field with
        | Sexp.List [Sexp.Atom name;sexp] -> name,sexp
        | _ -> fail ()
      ))
    in
    let field_names = List.fold T.explicit_sexp_option_fields ~init:field_names
      ~f:(fun field_names explicit_sexp_option_field ->
        let value = [%sexp_of: Sexp.t option]
          (Map.find field_names explicit_sexp_option_field)
        in
        Map.set field_names ~key:explicit_sexp_option_field ~data:value
      )
    in
    Sexp.List (List.map (Map.to_alist field_names) ~f:(fun (name,sexp) ->
      Sexp.List [Sexp.Atom name;sexp]
    ))
end

module Records_table = struct
  type 'a t = 'a list [@@deriving sexp]

  exception Invalid_record_sexp of Sexp.t [@@deriving sexp]

  exception Record_sexp_missing_field of (string * Sexp.t) list * string
  [@@deriving sexp]

  let sexp_of_t sexp_of_record records =
    let records =
      List.map records ~f:(fun record ->
        let sexp = sexp_of_record record in
        match sexp with
        | Sexp.List fields ->
          List.map fields ~f:(function
            | Sexp.List [ Sexp.Atom name; value ] -> (name, value)
            | _ -> raise (Invalid_record_sexp sexp))
        | _ -> raise (Invalid_record_sexp sexp))
    in
    let rows =
      match records with
      | [] -> []
      | record :: _ ->
        let field_names = List.map record ~f:fst in
        let header =
          Sexp.List (List.map field_names ~f:(fun field_name ->
            Sexp.Atom field_name))
        in
        let tuples =
          List.map records ~f:(fun record ->
            Sexp.List
              (List.map field_names ~f:(fun field_name ->
                match
                  List.find_map record ~f:(fun (field_name', value) ->
                    if field_name = field_name' then Some value else None)
                with
                | None -> raise (Record_sexp_missing_field (record, field_name))
                | Some value -> value)))
        in
        header :: tuples
    in
    Sexp.List rows
  ;;

  exception Invalid_table_sexp [@@deriving sexp]

  let t_of_sexp record_of_sexp sexp =
    let error () = raise (Of_sexp_error (Invalid_table_sexp, sexp)) in
    let rows =
      match sexp with
      | Sexp.Atom _ -> error ()
      | Sexp.List l ->
        List.map l ~f:(fun row ->
          match row with
          | Sexp.Atom _ -> error ()
          | Sexp.List l -> l)
    in
    match rows with
    | [] -> []
    | header :: rest ->
      List.map rest ~f:(fun tuple ->
        of_generated_sexp record_of_sexp
          ~original_sexp:sexp
          ~generated_sexp:
          (Sexp.List
             (List.map2_exn header tuple ~f:(fun field value ->
               Sexp.List [field; value]))))
  ;;
end

let load_sexp_conv_exn_sample ?strict ?buf ?(on_non_existence=`Exit) ?name
    file ~sexp_of_t ~t_of_sexp ~sample =
  if Sys.file_exists_exn file
  then Sexp.load_sexp_conv_exn ?strict ?buf file t_of_sexp
  else
    begin
      Sexp.save_hum file (sexp_of_t sample);
      let name =
        match name with
        | None -> ""
        | Some x -> " " ^ x
      in
      let message = sprintf "No file found at %s. Writing a sample%s" file
        name
      in
      match on_non_existence with
      | `Exit ->
        Printf.eprintf "%s and exiting.\n%!" message;
        exit 1
      | `Raise -> failwithf "%s." message ()
    end


module Comprehension = struct
  module Format = struct
    type ispec =
    | Sing of int
    | IRange of int * int * int
    [@@deriving sexp]

    type t =
    | Atom of string
    | Plus of string list
    | Times of t list
    | Set of ispec list
    | CRange of char * char
    [@@deriving sexp]

    let print_list print_elem list =
      match list with
      | [] -> failwith "Invalid arity"
      | e :: es -> sprintf "{%s%s}" (print_elem e)
        (String.concat (List.map es ~f:(fun e -> sprintf ",%s" (print_elem e))))

    let spec_to_string spec =
      match spec with
      | Sing n -> Int.to_string n
      | IRange(n,m,1) -> sprintf "%d..%d" n m
      | IRange(n,m,inc) -> sprintf "%d..%d..%d" n m inc

    let rec to_string fmt =
      match fmt with
      | Atom str -> str
      | Plus [str] -> str
      | Plus strs -> print_list Fn.id strs
      | Times es -> String.concat (List.map es ~f:to_string)
      | Set [Sing i] -> Int.to_string i
      | Set specs -> print_list spec_to_string specs
      | CRange (c1,c2) -> sprintf "{%c..%c}" c1 c2
  end

  type 'a t = 'a list [@@deriving sexp]

  (* Takes a comprehension spec and produces all the strings it matches *)
  let rec expand fmt =
    let open Format in
    match fmt with
    | Atom str -> [str]
    | Plus strs -> strs
    | Times comps -> List.fold_right comps ~init:[""]
      ~f:(fun comp acc ->
        let strs = expand comp in
        List.concat (List.map strs ~f:(fun str1 -> List.map acc ~f:(fun str2 ->
          str1 ^ str2))))
    (* Range is inclusive, but should never generate numbers outside the range
     * (see tests for clarification if needed) *)
    | Set elems -> List.concat (List.map elems ~f:(function
      | Sing x -> [Int.to_string x]
      | IRange (min, max, inc) -> List.init (1 + ((max - min) / inc))
        ~f:(fun i -> Int.to_string (min+(i*inc)))))
    | CRange (cmin, cmax) -> let min, max = Char.to_int cmin, Char.to_int cmax in
                             try
                               List.init (1 + max - min)
                                 ~f:(fun i -> String.of_char (Char.of_int_exn (i + min)))
                             with Failure _ -> failwith "Invalid spec for character range"

  (* If str begins with {, then split_brace_exn 0 str finds the matching }
   * and splits the string at that location. Raises an exception if
   * the braces aren't matched *)
  let rec split_brace_exn n str =
    match n, str with
    | _, '{' :: str' -> let left, right = split_brace_exn (n+1) str' in
                        '{' :: left, right
    | 1, '}' :: str' -> ['}'], str'
    | _, '}' :: str' -> let left, right = split_brace_exn (n-1) str' in
                        '}' :: left, right
    | _, c :: str' -> let left, right = split_brace_exn n str' in
                      c :: left, right
    | _, [] -> failwith "Unbalanced braces in list comprehension"

  let str_split_brace_exn str =
    let str1, str2 = split_brace_exn 0 (String.to_list str)
    in
    String.of_char_list str1, String.of_char_list str2

  (* If str begins with an alphanumeric, split_char str
   * splits it at the next opening brace *)
  let rec split_char str =
    match str with
    | [] -> [], []
    | '{' :: _ -> [], str
    | c :: cs -> let left, right = split_char cs in
                 c :: left, right

  let str_split_char str =
    let left, right = split_char (String.to_list str) in
    String.of_char_list left, String.of_char_list right

  let extract_opt ~rex str =
    match Re2.find_submatches rex str with
    | Error _ -> None
    | Ok ar -> Some (Array.map ar ~f:(Option.value ~default:""))

  let rec parse_exn' str =
    let open Format in
    let find_crange cs =
      let str = String.of_char_list cs in
      (* Matches str {a .. z} and returns |str, a, z| *)
      let r = Re2.create_exn "^{ *([a-zA-Z]) *\\.\\. *([a-zA-Z]) *}$" in
      Option.bind (extract_opt ~rex:r str) ~f:(fun matches ->
        let str1, str2 = matches.(1), matches.(2) in
        match String.length str1, String.length str2 with
        | 1, 1 -> let c1, c2 = str1.[0], str2.[0] in
                  if Char.to_int c1 < Char.to_int c2 then
                    Some(CRange(c1,c2))
                  else
                    failwith "Invalid character range in comprehension"
        | _ -> None)
    in
    (* Some <range> if cs is an integer range, None if not an integer range,
     * exception if an invalid integer range *)
    let find_range cs =
      let str = String.of_char_list cs in
      (* Matches str {num1 .. num2 .. inc} and returns |str, num1, num2, inc| *)
      let r_inc =
        Re2.create_exn "^ *([0-9]+) *\\.\\. *([0-9]+) *\\.\\. *([0-9]+) *$"
      in
      match extract_opt ~rex:r_inc str with
      | Some matches ->
        let min = Int.of_string matches.(1) in
        let max = Int.of_string matches.(2) in
        let inc = Int.of_string matches.(3) in
        if inc = 0
        then failwith "Invalid increment for range, must be positive"
        else IRange(min,max,inc)
      | None ->
        (* Matches str {num1 .. num2} and returns |str, num1, num2| *)
        let r = Re2.create_exn "^ *([0-9]+) *\\.\\. *([0-9]*) *$" in
        match extract_opt ~rex:r str with
        | Some matches ->
            let min, max = Int.of_string matches.(1), Int.of_string matches.(2) in
            if min >= max
            then failwith (sprintf "Invalid range in sexp: %d to %d" min max)
            else IRange(min,max,1)
        | None -> try Sing(Int.of_string (String.strip str))
          with _ -> failwith (sprintf "Invalid integer spec in comprehension: %s" str)
    in
    let find_range_opt cs =
      try Some(find_range cs)
      with _ -> None
    in
    let find_set cs =
      let str = String.of_char_list cs in
      (* Sets look like {5, 11.. 23, 16, 1 .. 5 .. 2}, so as long as the first
       * non-blank character is a digit we've found one*)
      let r = Re2.create_exn "^{ *([0-9].*) *}$" in
      Option.bind (extract_opt ~rex:r str) ~f:(fun matches ->
        let separated = matches.(1) in
        let strs = String.split ~on:',' separated in
        match List.map strs ~f:(fun str -> find_range (String.to_list str)) with
        (* Singleton sets aren't very interesting, so let this get interpreted
         * as an atom later instead. Not clear whether we want this behavior -
         * just copying bash *)
        | [Sing _] -> None
        | specs -> Some(Set specs))
    in
    (* Some <cs> if Plus <cs> is a valid sum expression, None otherwise *)
    let find_sum cs =
      let module S = String in
      let str = S.of_char_list cs in
      (* Check for comma because sums of one thing aren't very interesting, so let
       * them get interpreted as atoms later instead *)
      if S.is_prefix str ~prefix:"{" && S.is_suffix str ~suffix:"}" && S.contains str ','
      then
        let stripped = S.drop_prefix (S.drop_suffix str 1) 1 in
        let split = S.split ~on:',' stripped in
          (* If it still contains braces then it looks like {foo{a,b,c}bar}
           * which isn't a sum. *)
        if S.contains stripped '{' || S.contains stripped '}'
        then None
          (* If one of the branches of the sum can be interpreted
           * as an integer range, then someone's probably trying to mix
           * integers and strings, which isn't supported right now, so don't let them. *)
        else if List.exists split ~f:(fun str -> Option.is_some
          (find_range_opt (String.to_list str)))
        then failwith "Can't mix integers and strings in sum comprehension"
        else Some(List.map ~f:S.strip split)
      else None
    in
    (* Splits str into consecutive blocks of brace-delimited and undelimited characters *)
    let rec split_prod str =
      match str with
      | [] -> []
      | '{' :: _ -> let head, str' = split_brace_exn 0 str in
                    head :: (split_prod str')
      | _ :: _ -> let head, str' = split_char str in
                  head :: (split_prod str')
    in
    (* Handles the case where str is wrapped in braces but doesn't denote an expression.
     * This should come up rarely in our use cases, but it recursively looks for
     * expressions inside str *)
    let singleton str =
      match str with
      | '{' :: cs -> let cs' = List.take cs (List.length cs - 1) in
                     Times[Atom "{"; parse_exn' cs'; Atom "}"]
      | cs -> Atom (String.of_char_list cs)
    in
    let find_prod str =
      match split_prod str with
      | [] -> Some(Atom "")
      | [_] -> None
      | strs -> Some(Times(List.map strs ~f:(function
        | ('{' :: _) as str -> parse_exn' str
        | ('}' :: _) -> failwith "Unbalanced braces in list comprehension"
        | str -> Atom (String.of_char_list str))))
    in
    match find_prod str with
    | Some exp -> exp
    | None ->
      match find_crange str with
      | Some exp -> exp
      | None ->
        match find_set str with
        | Some exp -> exp
        | None ->
          match find_sum str with
          | Some strs -> Plus strs
          | None -> singleton str

  let parse_exn str = parse_exn' (String.to_list str)
  let expand_string_exn str = expand (parse_exn str)
  let expand_strings_exn strs = List.concat (List.map strs ~f:expand_string_exn)

  let t_of_sexp elem_of_sexp sexp =
    match sexp with
    | Atom _ -> failwith "Invalid sexp, expected list of atoms, but got atom"
    | List elems -> List.concat (List.map elems ~f:(fun sexp ->
      sexp |> Sexp.to_string |> expand_string_exn
      |> List.map ~f:(fun str -> str |> Sexp.of_string |> elem_of_sexp)))

  (* Count consecutive elements of ints that differ by delta *)
  let rec extract_run delta ints =
    match ints with
    | [] -> 0, []
    | [_] -> 1, []
    | x :: y :: xs ->
      if y = x + delta then
        let len, tail = extract_run delta (y :: xs) in
        len+1, tail
      else
        1, y :: xs

  (* Split ints into consecutive runs*)
  let rec find_runs ints =
    match ints with
    | [] -> []
    | [x] -> [Format.Sing x]
    | x :: y :: _ ->
      match extract_run (y - x) ints with
      | 1, tail -> (Format.Sing x) :: find_runs tail
      (* Use whatever format requires the least information:
       * {1,3} is less numbers than {1..3..2} but not less than
       * {1..2} *)
      | 2, tail -> if (y-x) = 1
        then
          (Format.IRange(x,x+1,1)) :: find_runs tail
        else
          (Format.Sing x) :: (Format.Sing y) :: find_runs tail
      | len, tail -> (Format.IRange(x, x + (len-1)*(y-x), y-x)) :: find_runs tail

  let compress_ints ints = Format.Set(find_runs (List.dedup_and_sort ints ~compare:Int.compare))

  (* Simplistic compression scheme: split each string into str1<num>str2 if possible.
   * For each set of matching str1 and str2, group together all the numbers in
   * a subpattern. *)
  let compress strs =
    let rec collect_num nums =
      match nums with
      | [] -> []
      | (front,_,back) :: _->
        let same, diff = List.partition_map nums ~f:(fun (front',i',back') ->
          if front = front' && back = back'
          then `Fst i'
          else `Snd(front', i', back'))
        in
        (front, compress_ints same, back) :: collect_num diff
    in
    let rec collect_suff nums =
      match nums with
      | [] -> []
      | (front, ints, _) :: _ ->
        let same, diff = List.partition_map nums ~f:(fun (front', ints', back') ->
          if front = front' && ints = ints'
          then `Fst back'
          else `Snd(front', ints', back'))
        in
        Format.(Times[Atom front; ints; Plus same]) :: collect_suff diff
    in
    let collect_suff nums =
      if List.exists nums ~f:(fun (_, _, suff) -> String.exists suff ~f:(fun c -> c = ')' || c = '('))
      then List.map nums ~f:(fun (front, ints, back) -> Format.(Times[Atom front; ints; Atom back]))
      else collect_suff nums
    in
    let num, alph = List.partition_map strs ~f:(fun str ->
      let r = Re2.create_exn "^([^0-9]*)([0-9]+)(.*)$" in
      match extract_opt ~rex:r str with
      | Some matches -> `Fst(matches.(1), Int.of_string matches.(2), matches.(3))
      | None -> `Snd (Format.Atom str))
    in
    List.map ~f:Format.to_string (alph @ collect_suff (collect_num num))

  let sexp_of_t sexp_of_elem elems =
    let strs = List.map ~f:(fun elem -> elem |> sexp_of_elem |> Sexp.to_string) elems in
    List (List.map (compress strs) ~f:Sexp.of_string)

  let die f = try let _ = f () in false with _ -> true

  (* You should always be able to convert a string list to comprehensions and back. *)
  let roundtrip strs =
    let sort = List.sort ~compare:String.compare in
    sort strs = (strs |> sort |> compress |> expand_strings_exn |> sort)

  let%test _ = Format.(["1";"2";"3"] = expand (Set[IRange(1,3,1)]))
  let%test _ = Format.(["ab1";"ab2";"ab3"] = expand (Times[Atom "ab"; Set[IRange (1,3,1)]]))
  let%test _ = Format.(["foobar"] = expand(Times[Atom "foo"; Atom "bar"]))
  let%test _ = Format.(["a1";"b1";"c1"] = expand(Times[Plus["a";"b";"c"]; Set[IRange(1,1,1)]]))
  let%test _ = Format.(["a1";"a2";"d1";"d2"] = expand(Times[Plus["a";"d"]; Set[IRange(1,2,1)]]))
  let%test _ = Format.(["135"; "136"; "145"; "146"; "235"; "236"; "245"; "246"]
                 = expand(Times[Set[IRange(1,2,1)]; Set[IRange(3,4,1)]; Set[IRange(5,6,1)]]))
  (* Sums where the branches are different sizes *)
  let%test _ = ["adefg"; "adfg"; "adg"; "abdefg"; "abdfg"; "abdg";
          "abcdefg"; "abcdfg"; "abcdg"] = expand_string_exn "{a,ab,abc}d{efg,fg,g}"
  (* Brace matching *)
  let%test _ = ("{foo}", "bar") = str_split_brace_exn "{foo}bar"
  let%test _ = ("foo", "{bar}") = str_split_char "foo{bar}"
  let%test _ = ("{{{}}{}{{}}{}}", "{{}{}}") = str_split_brace_exn "{{{}}{}{{}}{}}{{}{}}"
  let%test _ = Format.(Atom "") = parse_exn ""
  (* Whitepsace insensitivity for integer ranges *)
  let%test _ = Format.(Set[IRange(12,25,1)]) = parse_exn "{12       ..  25}"
  let%test _ = Format.(Set[IRange(12,25,1)]) = parse_exn "{12 .. 25}"
  let%test _ = Format.(Set[IRange(12,25,1)]) = parse_exn "{12 ..25}"
  let%test _ = Format.(Set[IRange(5,25,1)]) = parse_exn "{5.. 25}"
  let%test _ = Format.(Set[IRange(3,6,3)] = parse_exn "{3..6..3}")
  (* Simple sums *)
  let%test _ = Format.(Plus["a";"b";"c"]) = parse_exn "{a,b,c}"
  (* Character ranges *)
  let%test _ = Format.(Times([Atom "hi"; CRange('a','d'); Plus["a";"e";"f"]])
                 = parse_exn "hi{a..d}{a,e,f}")

  let%test _ = Format.(Times[Atom "a"; Set[IRange(1,3,1)]; Atom "c"]) = parse_exn "a{1 .. 3}c"
  let%test _ = (["{a}"; "{b}"; "{c}"] = expand_string_exn "{{a,b,c}}")
  let%test _ = (["this{is}{not}a{test}"] = expand_string_exn "this{is}{not}a{test}")
  let%test _ = (["{{{hi}}}"] = expand_string_exn "{{{hi}}}")

  let%test _ = (["{{1}{2}}hi{{a}{b}}"] = expand_string_exn "{{1}{2}}hi{{a}{b}}")

  let%test _ = (["{{1}{2}}hi{{a}{b}}"] = t_of_sexp String.t_of_sexp (List[Atom( "{{1}{2}}hi{{a}{b}}")]))
  let%test _ = (["1";"2";"5";"6";"7"] = t_of_sexp String.t_of_sexp (List[Atom("{1..2}"); Atom("{5..7}")]))

  let%test _ = (["Afoo7"; "Afoo8"; "Afoo9"; "bfoo7"; "bfoo8"; "bfoo9"] =
      expand_string_exn "{A,b}foo{7..9}")

  let%test _ = (["1"; "3"; "5"] = expand_string_exn "{1..5..2}")
  let%test _ = (["1"; "3"; "5"] = expand_string_exn "{1..6..2}")
  let%test _ = (["a1"; "a2"; "b1"; "b2"; "c1"; "c2"] = expand_string_exn "{a..c}{1..2}")

  let%test _ = (["{ a }b5{4}"; "{ a }b1{4}"; "{ a }b2{4}"; "{ a }b1{4}";
           "{ a }b7{4}"; "{ a }c5{4}"; "{ a }c1{4}"; "{ a }c2{4}";
           "{ a }c1{4}"; "{ a }c7{4}"]
          = expand_string_exn  "{ a }{   b  .. c  }{  5 , 1 .. 2 , 1 ..  2   .. 2   , 7  }{4}")

  let%test _ = roundtrip ["foo1a"; "foo2a"; "foo3a"; "foo3b"; "foo4b"; "foo5c"; "foo7c";
                    "foo7d"; "foo9d"; "foo11d"]
  let%test _ = roundtrip ["foo1a"; "foo2a"; "foo3b"]
  let%test _ = roundtrip["a1b1"; "a2b1"; "a1b2"; "a2b2"]

  (* Brace matching *)
  let%test _ = roundtrip["{{{}}{}{{}}{}}{{}{}}"]
  (* Keyboard-mashing*)
  let%test _ = roundtrip["1212k1"; "121k2"; "12k"; "12k1"; "12k12"; "12k12121k2"; "1k1"; "1k2";
                   "1kk"; "2"; "21k"; "21k2"; "2k"; "2k1"; "2k12"; "2k12121k2"; "2k121k";
                   "2k12k1"; "2k12k121"; "2k12k12k"; "k"; "k1"; "k21k212"; "k2k121k212k";
                   "k2k12k1"]
  (* Longish range *)
  let%test _ = (["a{1..100}b"] = compress (List.init 100 ~f:(fun i -> sprintf "a%db" (i+1))))
  let%test _ = (["{1..100}b"] = compress (List.init 100 ~f:(fun i -> sprintf "%db" (i+1))))
  let%test _ = (["a{1..100}"] = compress (List.init 100 ~f:(fun i -> sprintf "a%d" (i+1))))
  let%test _ = (["{1..100}"] = compress (List.init 100 ~f:(fun i -> sprintf "%d" (i+1))))
  (* Compress suffixes *)
  let%test _ = (["a{1..2}{foo,bar}"] = compress ["a1foo"; "a1bar"; "a2foo"; "a2bar"])
  (* Don't compress not-suffices - one buggy implementation turns this into {1..2}a{1..2} *)
  let%test _ = (["1a1"; "2a2"] = compress ["1a1"; "2a2"])
  (* Same test but considering that we compress suffixes *)
  let%test _ = roundtrip ["a1a"; "a2b"]
  let%test _ = roundtrip ["a1a"; "b1b"]
  let%test _ = roundtrip ["a1a"; "b2a"]
  (* Similar test, but some things should actually get compressed *)
  let%test _ = roundtrip ["a1a"; "a1b"; "aa1a"; "aa2a"; "a2b"]
  (* Order shouldn't matter for compression *)
  let%test _ = (compress ["a1"; "a3"; "a5"; "a7"]) = (compress ["a5"; "a1"; "a7"; "a3"])
  (* If  you accidentally sort the numbers as strings they could end up in lexicographic
   * order and finding runs would fail. So test numbers whose lexicographic order
   * differs from numeric order *)
  let%test _ = ["{500..1500..500}"] = compress ["1500"; "500"; "1000"]

  let%test _ = die (fun () -> expand_string_exn "{1..1}")
  let%test _ = die (fun () -> expand_string_exn "{2..1}")
  let%test _ = die (fun () -> expand_string_exn "{foo}{")
  let%test _ = die (fun () -> expand_string_exn "}{1..3}")
  let%test _ = die (fun () -> expand_string_exn "{{1}{2}}}}{{{a}")
  let%test _ = die (fun () -> expand_string_exn "{1..5..0}")
  let%test _ = die (fun () -> expand_string_exn "{z..a}")
  let%test _ = die (fun () -> expand_string_exn "{a..a}")
  let%test _ = die (fun () -> expand_string_exn "{1..23,a}")
  let%test _ = die (fun () -> expand_string_exn "{a,1..23}")
  let%test _ = die (fun () -> t_of_sexp String.t_of_sexp (Atom "hi"))
  let%test _ = die (fun () -> t_of_sexp String.t_of_sexp (List [Atom "hi"; List []]))


end

let%test_module "sexp_parens" = (module struct
  type t = (int * int option) Comprehension.t [@@deriving sexp]

  let sexp_roundtrip t =
    t = t_of_sexp (sexp_of_t t)

  (* These elements have sexps that differ only by a suffix which contains
   * parens. If you compress these strings naively you'll get an invalid sexp,
   * and parsing it will fail *)
  let%test _ = sexp_roundtrip [(100, Some 200); (100, None)]
end)

