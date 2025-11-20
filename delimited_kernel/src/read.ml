open Core

exception Bad_csv_formatting = Parse_state.Bad_csv_formatting

module On_invalid_row = struct
  type 'a t =
    line_number:int
    -> int String.Map.t
    -> string Append_only_buffer.t
    -> exn
    -> [ `Skip | `Yield of 'a | `Raise of exn | `Fallback of 'a t ]

  let raise ~line_number header_map buffer exn =
    `Raise
      (Exn.create_s
         [%message
           "Exception raised in Delimited.Read"
             (line_number : int)
             (header_map : int String.Map.t)
             ~buffer:(Append_only_buffer.to_list buffer : string list)
             (exn : exn)])
  ;;

  let raise_with_filename ~filename ~line_number header_map buffer exn =
    `Raise
      (Exn.create_s
         [%message
           "Exception raised in Delimited.Read"
             (filename : string)
             (line_number : int)
             (header_map : int String.Map.t)
             ~buffer:(Append_only_buffer.to_list buffer : string list)
             (exn : exn)])
  ;;

  let skip ~line_number:_ _ _ _ = `Skip
  let create = Fn.id
end

module Header = Header

module Builder = struct
  module T = struct
    type _ t =
      | Column : int -> string t
      | Header : string -> string t
      | Header_opt : string -> string option t
      | Return : 'a -> 'a t
      | Apply : ('b -> 'a) t * 'b t -> 'a t
      | Map : ('b -> 'a) * 'b t -> 'a t
      | Map2 : ('b -> 'c -> 'a) * 'b t * 'c t -> 'a t
      | Both : 'a t * 'b t -> ('a * 'b) t
      | Lambda : (int String.Map.t -> string Append_only_buffer.t -> 'a) -> 'a t
      | Label : 'a t * Info.t -> 'a t

    let return x = Return x
    let label t info = Label (t, info)

    let apply f x =
      match f with
      | Return f -> Map (f, x)
      | Map (f, w) -> Map2 (f, w, x)
      | _ -> Apply (f, x)
    ;;

    let map x ~f =
      match x with
      | Return x -> Return (f x)
      | _ -> Map (f, x)
    ;;

    let ( >>| ) t f = map t ~f

    let map2 x y ~f =
      match x, y with
      | Return x, Return y -> Return (f x y)
      | _ -> Map2 (f, x, y)
    ;;

    let map3 x y z ~f =
      match x, y, z with
      | Return x, Return y, Return z -> Return (f x y z)
      | _ -> Apply (Map2 (f, x, y), z)
    ;;

    let all ts = List.fold_right ts ~init:(return []) ~f:(map2 ~f:(fun x xs -> x :: xs))
    let all_unit ts = map ~f:ignore (all ts)
    let both x y = Both (x, y)
    let ( <*> ) = apply
    let ( *> ) u v = return (fun () y -> y) <*> u <*> v
    let ( <* ) u v = return (fun x () -> x) <*> u <*> v

    module Applicative_infix = struct
      let ( <*> ) = ( <*> )
      let ( *> ) = ( *> )
      let ( <* ) = ( <* )
      let ( >>| ) = ( >>| )
    end
  end

  include T

  let at_index i ~f =
    Label (Map (f, Column i), Info.of_lazy (lazy [%string "[at_index %{i#Int}]"]))
  ;;

  let at_header h ~f =
    Label (Map (f, Header h), Info.of_lazy (lazy [%string "[at_header \"%{h}\"]"]))
  ;;

  let at_header_opt h ~f =
    Label (Map (f, Header_opt h), Info.of_lazy (lazy [%string "[at_header_opt \"%{h}\"]"]))
  ;;

  let lambda f = Lambda f

  module Record_builder = Record_builder.Make (T)

  module Fields_O = struct
    let ( !! ) of_string field =
      Record_builder.field (at_header (Field.name field) ~f:of_string) field
    ;;

    let ( !? ) of_string field =
      Record_builder.field (at_header_opt (Field.name field) ~f:of_string) field
    ;;
  end

  module Open_on_rhs_intf = struct
    module type S = Read_intf.Open_on_rhs with type 'a t := 'a t
  end

  module Let_syntax = struct
    include T
    include Applicative_infix

    module Let_syntax = struct
      include T

      module Open_on_rhs = struct
        let at_index = at_index
        let at_header = at_header
        let at_header_opt = at_header_opt
        let label = label
      end
    end
  end

  module Without_headers = struct
    type 'a t =
      | Column : int -> string t
      | Return : 'a -> 'a t
      | Apply : ('b -> 'a) t * 'b t -> 'a t
      | Map : ('b -> 'a) * 'b t -> 'a t
      | Map2 : ('b -> 'c -> 'a) * 'b t * 'c t -> 'a t
      | Both : ('a t * 'b t) -> ('a * 'b) t
      | Lambda :
          (int String.Map.t -> string Append_only_buffer.t -> 'a) * int String.Map.t
          -> 'a t
      | Label : 'a t * Info.t -> 'a t

    let get_fields_used t =
      let open Option.Let_syntax in
      let rec fields : type a. a t -> Int.Set.t option = function
        | Return _ -> Some Int.Set.empty
        | Column i -> Some (Int.Set.singleton i)
        | Apply (f, x) ->
          let%bind f = fields f in
          let%map x = fields x in
          Set.union f x
        | Map (_, x) -> fields x
        | Map2 (_, x, y) ->
          let%bind x = fields x in
          let%map y = fields y in
          Set.union x y
        | Both (x, y) ->
          let%bind x = fields x in
          let%map y = fields y in
          Set.union x y
        | Lambda _ -> None
        | Label (t, _) -> fields t
      in
      fields t
    ;;

    let build t =
      let rec build' : type a. a t -> string Append_only_buffer.t -> a =
        fun t row ->
        match t with
        | Return x -> x
        | Column i -> Append_only_buffer.nth_exn row i
        | Apply (f, x) -> (build' f row) (build' x row)
        | Map (f, x) -> f (build' x row)
        | Map2 (f, x, y) -> f (build' x row) (build' y row)
        | Both (x, y) -> build' x row, build' y row
        | Lambda (f, header_map) -> f header_map row
        | Label (t, info) ->
          (try build' t row with
           | exn -> Exn.reraisef exn "Raised in %s" (Info.to_string_hum info) ())
      in
      let fields_used = get_fields_used t in
      match fields_used with
      | None -> build' t, None
      | Some fields_used ->
        let fields_used = Set.to_list fields_used in
        let mapping =
          List.mapi fields_used ~f:(fun i field_index -> field_index, i)
          |> Int.Map.of_alist_exn
        in
        let rec remap : type a. a t -> a t =
          fun t ->
          match t with
          | Column i -> Column (Map.find_exn mapping i)
          | Return x -> Return x
          | Apply (f, x) -> Apply (remap f, remap x)
          | Map (f, x) -> Map (f, remap x)
          | Map2 (f, x, y) -> Map2 (f, remap x, remap y)
          | Both (x, y) -> Both (remap x, remap y)
          | Lambda _ -> t
          | Label (t, info) -> Label (remap t, info)
        in
        build' (remap t), Some (Array.of_list fields_used)
    ;;
  end

  let build ~header_map t =
    let rec transform : type a. a t -> a Without_headers.t = function
      | Return x -> Without_headers.Return x
      | Column i -> Without_headers.Column i
      | Header column ->
        (match Map.find_exn (header_map : _ String.Map.t) column with
         | index -> Without_headers.Column index
         | exception (Not_found_s _ | Stdlib.Not_found) ->
           raise_s
             [%message
               "Missing column in header"
                 (column : string)
                 (header_map : int String.Map.t)])
      | Header_opt h ->
        (match Map.find_exn (header_map : _ String.Map.t) h with
         | index -> Without_headers.Map (Option.some, Column index)
         | exception (Not_found_s _ | Stdlib.Not_found) -> Without_headers.Return None)
      | Apply (f, x) -> Without_headers.Apply (transform f, transform x)
      | Map (f, x) -> Without_headers.Map (f, transform x)
      | Map2 (f, x, y) -> Without_headers.Map2 (f, transform x, transform y)
      | Both (x, y) -> Without_headers.Both (transform x, transform y)
      | Lambda f -> Without_headers.Lambda (f, header_map)
      | Label (t, info) -> Without_headers.Label (transform t, info)
    in
    let transformed = transform t in
    Without_headers.build transformed
  ;;
end

include Builder

module Parse_header = struct
  module Partial = struct
    type t =
      { state : string array option Parse_state.t
      ; transform : string array -> int String.Map.t
      }
  end

  module Success = struct
    type t =
      { header_map : int String.Map.t
      ; next_line_number : int
      ; consumed : int
      }
    [@@deriving sexp_of]
  end

  let header_map ~foldi header_row =
    foldi header_row ~init:String.Map.empty ~f:(fun i map header ->
      Map.set map ~key:header ~data:i)
  ;;

  let require_header required_headers' csv_headers' =
    let required_headers = String.Set.of_list required_headers' in
    let csv_headers = String.Set.of_array csv_headers' in
    let missing = Set.diff required_headers csv_headers in
    if not (Set.is_empty missing)
    then
      raise_s
        [%message
          "Header specified in `Require not present in csv document"
            (required_headers : String.Set.t)
            (csv_headers : String.Set.t)
            (missing : String.Set.t)];
    header_map ~foldi:Array.foldi csv_headers'
  ;;

  let create' ?strip ?sep ?quote ?start_line_number transform : Partial.t =
    let f ~line_number:_ r row =
      match r with
      | Some _ -> raise_s [%message "Header already parsed, cannot feed more input"]
      | None -> Some (Append_only_buffer.to_array row)
    in
    { state =
        Parse_state.create
          ?strip
          ?sep
          ?quote
          ?start_line_number
          ~fields_used:None
          ~init:None
          ~f
          ()
    ; transform
    }
  ;;

  let create ?strip ?sep ?quote ?(start_line_number = 1) ?(header = `No) () =
    match header with
    | `No ->
      Second
        { Success.header_map = String.Map.empty
        ; next_line_number = start_line_number
        ; consumed = 0
        }
    | `Add headers ->
      Second
        { header_map = header_map ~foldi:List.foldi headers
        ; next_line_number = start_line_number
        ; consumed = 0
        }
    | `Yes ->
      let f headers = header_map ~foldi:Array.foldi headers in
      First (create' ?strip ?sep ?quote ~start_line_number f)
    | `Require headers ->
      let f csv_headers = require_header headers csv_headers in
      First (create' ?strip ?sep ?quote ~start_line_number f)
    | `Replace headers ->
      let f _ = header_map ~foldi:List.foldi headers in
      First (create' ?strip ?sep ?quote ~start_line_number f)
    | `Transform f ->
      let f headers = header_map ~foldi:List.foldi (f (Array.to_list headers)) in
      First (create' ?strip ?sep ?quote ~start_line_number f)
    | `Filter_map f ->
      let foldi l ~init ~f =
        List.foldi l ~init ~f:(fun i acc x ->
          match x with
          | None -> acc
          | Some x -> f i acc x)
        [@nontail]
      in
      let f headers = header_map ~foldi (f (Array.to_list headers)) in
      First (create' ?strip ?sep ?quote ~start_line_number f)
  ;;

  let is_set (t : Partial.t) =
    match Parse_state.acc t.state with
    | Some row -> Some (t, row)
    | None -> None
  ;;

  (* We only want to parse the header here and nothing more. To achieve that, we feed the
     parser with chunks of input that does not contain newlines but at the end. After each
     chunk, we check whether a header was successfully parsed or not. *)
  let input_string (t : Partial.t) ?(pos = 0) ?len input : _ Either.t =
    match is_set t with
    | Some _ -> raise_s [%message "Header already parsed, cannot feed more input"]
    | None ->
      let len =
        match len with
        | None -> String.length input - pos
        | Some len -> len
      in
      let start_pos = pos in
      let max_pos = start_pos + len in
      if start_pos < 0 || len < 0 || max_pos > String.length input
      then
        invalid_arg
          "Delimited_kernel.Read.Expert.Parse_header.input_string: index out of bound";
      let rec loop (t : Partial.t) ~pos ~len =
        if pos >= max_pos
        then First t
        else (
          let end_pos =
            match String.index_from input pos '\n' with
            | None -> max_pos
            | Some end_pos -> min max_pos (end_pos + 1)
          in
          let line_len = end_pos - pos in
          let t =
            { t with state = Parse_state.input_string t.state ~pos ~len:line_len input }
          in
          match is_set t with
          | None -> loop t ~pos:end_pos ~len:(len - line_len)
          | Some (t, row) ->
            Second
              { Success.header_map = t.transform row
              ; consumed = end_pos - start_pos
              ; next_line_number = Parse_state.current_line_number t.state
              })
      in
      loop t ~pos ~len
  ;;

  let finish_exn (t : Partial.t) =
    let t = { t with state = Parse_state.finish t.state } in
    match is_set t with
    | Some (t, row) ->
      { Success.header_map = t.transform row
      ; consumed = 0
      ; next_line_number = Parse_state.current_line_number t.state
      }
    | None ->
      if Parse_state.at_beginning_of_row t.state
      then
        (* An empty header is parsed as a single column with an empty name *)
        { Success.header_map = t.transform [| "" |]
        ; consumed = 0
        ; next_line_number = Parse_state.current_line_number t.state
        }
      else (* [Parse_state.finish] would have raised already in the cases *)
        assert false
  ;;
end

module Streaming = struct
  type 'a builder_t = 'a t

  type 'a t =
    | Parsing_header of
        { partial : Parse_header.Partial.t
        ; init : 'a
        ; mk_state : 'a -> ?start_line_number:int -> int String.Map.t -> 'a Parse_state.t
        }
    | Parsing_rows of
        { header_map : int String.Map.t
        ; state : 'a Parse_state.t
        }

  let create_indexed
    ?strip
    ?sep
    ?quote
    ?start_line_number
    ?(on_invalid_row = (On_invalid_row.raise : _ On_invalid_row.t))
    ?(header : Header.t option)
    builder
    ~init
    ~f
    =
    let mk_state init ?start_line_number header_map =
      let row_to_'a, fields_used = Builder.build ~header_map builder in
      let f ~line_number init row =
        try f line_number init (row_to_'a row) with
        | exn ->
          let rec loop on_invalid_row =
            match on_invalid_row ~line_number header_map row exn with
            | `Yield x -> f line_number init x
            | `Skip -> init
            | `Fallback on_invalid_row -> loop on_invalid_row
            | `Raise exn -> raise exn
          in
          loop on_invalid_row
      in
      Parse_state.create ?strip ?sep ?quote ?start_line_number ~fields_used ~init ~f ()
    in
    match Parse_header.create ?strip ?sep ?quote ?start_line_number ?header () with
    | First partial -> Parsing_header { partial; mk_state; init }
    | Second { header_map; consumed = _; next_line_number } ->
      let state = mk_state init ~start_line_number:next_line_number header_map in
      Parsing_rows { header_map; state }
  ;;

  let create
    ?strip
    ?sep
    ?quote
    ?start_line_number
    ?on_invalid_row
    ?header
    builder
    ~init
    ~f
    =
    create_indexed
      ?strip
      ?sep
      ?quote
      ?start_line_number
      ?on_invalid_row
      ?header
      builder
      ~init
      ~f:(fun (_ : int) -> f)
  ;;

  let input_string t ?(pos = 0) ?len input =
    let len =
      match len with
      | None -> String.length input - pos
      | Some len -> len
    in
    match t with
    | Parsing_header { partial; mk_state; init } ->
      (match Parse_header.input_string partial ~pos ~len input with
       | First partial -> Parsing_header { partial; mk_state; init }
       | Second { consumed; header_map; next_line_number } ->
         let state = mk_state init header_map ~start_line_number:next_line_number in
         let state =
           Parse_state.input_string
             state
             ~pos:(pos + consumed)
             ~len:(len - consumed)
             input
         in
         Parsing_rows { header_map; state })
    | Parsing_rows { header_map; state } ->
      Parsing_rows { header_map; state = Parse_state.input_string state ~pos ~len input }
  ;;

  let input t ?pos ?len input =
    input_string
      t
      ?pos
      ?len
      (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:input)
  ;;

  let finish t =
    match t with
    | Parsing_header { partial; mk_state; init } ->
      let { Parse_header.Success.consumed = _; header_map; next_line_number } =
        Parse_header.finish_exn partial
      in
      let state =
        Parse_state.finish (mk_state init header_map ~start_line_number:next_line_number)
      in
      Parsing_rows { state; header_map }
    | Parsing_rows { state; header_map } ->
      Parsing_rows { state = Parse_state.finish state; header_map }
  ;;

  let acc t =
    match t with
    | Parsing_header { init; _ } -> init
    | Parsing_rows { state; _ } -> Parse_state.acc state
  ;;

  let state t =
    match t with
    | Parsing_header _ -> `Parsing_header
    | Parsing_rows _ -> `Parsing_rows
  ;;

  let headers t =
    match t with
    | Parsing_header _ -> None
    | Parsing_rows { header_map; _ } -> Some (Map.key_set header_map)
  ;;

  let list_of_headers t =
    match t with
    | Parsing_header _ -> None
    | Parsing_rows { header_map; _ } ->
      Map.to_alist header_map
      |> List.sort ~compare:(fun a b -> Int.compare (snd a) (snd b))
      |> List.map ~f:fst
      |> Option.some
  ;;
end

module Expert = struct
  module Append_only_buffer = Append_only_buffer
  module Parse_state = Parse_state
  module Builder = Builder
end

let fold_string ?strip ?sep ?quote ?header ?on_invalid_row builder ~init ~f csv_string =
  let state =
    Streaming.create ?strip ?sep ?quote ?header ?on_invalid_row builder ~init ~f
  in
  Streaming.input_string state csv_string |> Streaming.finish |> Streaming.acc
;;

let list_of_string ?strip ?sep ?quote ?header ?on_invalid_row builder csv_string =
  fold_string
    ?strip
    ?sep
    ?quote
    ?header
    ?on_invalid_row
    builder
    csv_string
    ~init:(Append_only_buffer.create ())
    ~f:(fun queue row ->
      Append_only_buffer.append queue row;
      queue)
  |> Append_only_buffer.to_list
;;

let read_lines ?strip ?sep ?quote ?header ?on_invalid_row builder in_channel =
  let contents = In_channel.input_all in_channel in
  list_of_string ?strip ?sep ?quote ?header ?on_invalid_row builder contents
;;

let fold_lines
  ?(buffer_size = 8192)
  ?strip
  ?sep
  ?quote
  ?header
  ?on_invalid_row
  builder
  ~init
  ~f
  in_channel
  =
  let buf = Bytes.create buffer_size in
  let rec loop state =
    match In_channel.input in_channel ~buf ~pos:0 ~len:buffer_size with
    | 0 -> state
    | len ->
      Streaming.input_string
        state
        ~len
        (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf)
      |> loop
  in
  Streaming.create ?strip ?sep ?quote ?header ?on_invalid_row builder ~init ~f
  |> loop
  |> Streaming.finish
  |> Streaming.acc
;;

module Row = struct
  type 'a builder_t = 'a t

  include Row

  let builder = lambda Row.Expert.of_buffer
end
