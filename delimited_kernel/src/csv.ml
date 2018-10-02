open Core_kernel
open Int.Replace_polymorphic_compare

(* row up to the error, and the field with the error up to the point of failure *)
exception Bad_csv_formatting of string list * string

module Fast_queue = struct
  (* Fast_queue is faster than Core_queue because it doesn't support dequeue, traversal, or
     detection of mutation during traversal. *)
  type 'a t =
    { mutable array : 'a Option_array.t
    ; mutable length : int
    ; mutable capacity : int
    }

  let to_array t = Array.init t.length ~f:(fun i -> Option_array.get_some_exn t.array i)

  let create ?(capacity = 1) () =
    { array = Option_array.create ~len:capacity; capacity; length = 0 }
  ;;

  let set_capacity t desired_capacity =
    let new_capacity = Int.ceil_pow2 (max 1 (max desired_capacity t.length)) in
    if new_capacity <> t.capacity
    then (
      let dst = Option_array.create ~len:new_capacity in
      Option_array.unsafe_blit ~len:t.length ~src:t.array ~src_pos:0 ~dst ~dst_pos:0;
      t.array <- dst;
      t.capacity <- new_capacity)
  ;;

  let enqueue t x =
    if t.length >= t.capacity then set_capacity t (2 * t.length);
    Option_array.unsafe_set_some t.array t.length x;
    t.length <- t.length + 1
  ;;

  let clear t = t.length <- 0

  let nth_exn t n =
    if n < 0 || n >= t.length
    then failwith "Index out of bounds"
    else Option_array.get_some_exn t.array n
  ;;

  let of_list l =
    let t = create ~capacity:(List.length l) () in
    List.iter l ~f:(fun x -> enqueue t x);
    t
  ;;

  let to_list t = List.init t.length ~f:(Option_array.get_some_exn t.array)
  let length t = t.length
end

module Header = Header

module Parse_state = struct
  module Step = struct
    type t =
      | Field_start
      | In_unquoted_field
      | In_quoted_field
      | In_quoted_field_after_quote
  end

  open Step

  type 'a t =
    { acc : 'a
    ; sep : char
    ; quote : char
    ; use_quoting :
        bool
    ; lineno : int
    ; step : Step.t
    ; field : string
    ; current_row : string list
    ; emit_field : string Fast_queue.t -> Buffer.t -> unit
    ; f : int -> 'a -> string Fast_queue.t -> 'a
    ; fields_used : int array option
    ; current_field : int
    ; next_field_index : int
    }
  [@@deriving fields]

  let make_emit_field ~strip current_row field =
    Fast_queue.enqueue
      current_row
      (if strip then Shared.strip_buffer field else Buffer.contents field);
    Buffer.clear field
  ;;

  let emit_row f i acc current_row =
    let acc = f (i + 1) acc current_row in
    Fast_queue.clear current_row;
    acc
  ;;

  let set_acc t acc = { t with acc }

  let create ?(strip = false) ?(sep = ',') ?(quote = `Using '"') ~fields_used ~init ~f ()
    =
    { acc = init
    ; sep
    ; quote =
        (match quote with
         | `Using char -> char
         | `No_quoting -> '"')
    ; use_quoting =
        (match quote with
         | `Using _ -> true
         | `No_quoting -> false)
    ; lineno = 1
    ; step = Field_start
    ; field = ""
    ; current_row = []
    ; emit_field = make_emit_field ~strip
    ; f
    ; fields_used
    ; current_field = 0
    ; next_field_index = 0
    }
  ;;

  let is_at_beginning_of_row t =
    String.is_empty t.field
    && List.is_empty t.current_row
    &&
    match t.step with
    | Field_start -> true
    | In_unquoted_field | In_quoted_field | In_quoted_field_after_quote -> false
  ;;

  let mutable_of_t t =
    let field = Buffer.create (String.length t.field) in
    Buffer.add_string field t.field;
    let current_row = Fast_queue.of_list t.current_row in
    field, current_row
  ;;

  (* To reduce the number of allocations, we keep an array [fields_used] of the field
     indexes we care about. [current_field] is the position of the parser within the
     input row, and [next_field_index] is an index into the [fields_used] array
     indicating the next field that we need to store.

     If [fields_used] is None, we need to store every field.
  *)
  let should_enqueue fields_used current_field next_field_index =
    match fields_used with
    | None -> true
    | Some array ->
      next_field_index < Array.length array && array.(next_field_index) = current_field
  ;;

  let input_aux ~get_length ~get t ?(pos = 0) ?len input =
    let field, current_row = mutable_of_t t in
    let enqueue =
      ref (should_enqueue t.fields_used t.current_field t.next_field_index)
    in
    let current_field = ref t.current_field in
    let next_field_index = ref t.next_field_index in
    let increment_field () =
      current_field := !current_field + 1;
      next_field_index := if !enqueue then !next_field_index + 1 else !next_field_index;
      enqueue := should_enqueue t.fields_used !current_field !next_field_index
    in
    let reset_field () =
      current_field := 0;
      next_field_index := 0;
      enqueue := should_enqueue t.fields_used !current_field !next_field_index
    in
    let loop_bound =
      match len with
      | Some i -> i + pos
      | None -> get_length input
    in
    let rec loop i t step =
      if i >= loop_bound
      then
        { t with
          step; current_field = !current_field; next_field_index = !next_field_index
        }
      else
        let open Char.Replace_polymorphic_compare in
        let continue = loop (i + 1) in
        let c = get input i in
        if c = '\r'
        then continue t step
        else (
          match step with
          | Field_start ->
            if c = t.quote && t.use_quoting
            then continue t In_quoted_field
            else if c = t.sep
            then (
              if !enqueue then t.emit_field current_row field;
              increment_field ();
              continue t Field_start)
            else if c = '\n'
            then (
              if !enqueue then t.emit_field current_row field;
              reset_field ();
              continue
                { t with acc = emit_row t.f i t.acc current_row; lineno = t.lineno + 1 }
                Field_start)
            else (
              if !enqueue then Buffer.add_char field c;
              continue t In_unquoted_field)
          | In_unquoted_field ->
            if c = t.sep
            then (
              if !enqueue then t.emit_field current_row field;
              increment_field ();
              continue t Field_start)
            else if c = '\n'
            then (
              if !enqueue then t.emit_field current_row field;
              reset_field ();
              continue
                { t with acc = emit_row t.f i t.acc current_row; lineno = t.lineno + 1 }
                Field_start)
            else (
              if !enqueue then Buffer.add_char field c;
              continue t step)
          | In_quoted_field ->
            if c = t.quote
            then continue t In_quoted_field_after_quote
            else (
              if !enqueue then Buffer.add_char field c;
              continue t step)
          | In_quoted_field_after_quote ->
            (* We must be using quoting to be in this state. *)
            if c = t.quote
            then (
              (* doubled quote *)
              if !enqueue then Buffer.add_char field t.quote;
              continue t In_quoted_field)
            else if c = '0'
            then (
              if !enqueue then Buffer.add_char field '\000';
              continue t In_quoted_field)
            else if c = t.sep
            then (
              if !enqueue then t.emit_field current_row field;
              increment_field ();
              continue t Field_start)
            else if c = '\n'
            then (
              if !enqueue then t.emit_field current_row field;
              reset_field ();
              continue
                { t with acc = emit_row t.f i t.acc current_row; lineno = t.lineno + 1 }
                Field_start)
            else if Char.is_whitespace c
            then continue t step
            else
              failwithf
                "In_quoted_field_after_quote looking at '%c' (lineno=%d)"
                c
                t.lineno
                ())
    in
    let t' = loop pos t t.step in
    { t' with
      field = Buffer.contents field
    ; current_row = Fast_queue.to_list current_row
    ; current_field = !current_field
    ; next_field_index = !next_field_index
    }
  ;;

  let input t ?pos ?len input =
    input_aux ~get_length:Bytes.length ~get:Bytes.get t ?pos ?len input
  ;;

  let input_string t ?pos ?len input =
    input_aux ~get_length:String.length ~get:String.get t ?pos ?len input
  ;;

  let finish t =
    let field, current_row = mutable_of_t t in
    let enqueue = should_enqueue t.fields_used t.current_field t.next_field_index in
    let acc =
      match t.step with
      | Field_start ->
        if Fast_queue.length current_row <> 0
        then (
          if enqueue then t.emit_field current_row field;
          emit_row t.f 0 t.acc current_row)
        else t.acc
      | In_unquoted_field | In_quoted_field_after_quote ->
        if enqueue then t.emit_field current_row field;
        emit_row t.f 0 t.acc current_row
      | In_quoted_field ->
        raise
          (Bad_csv_formatting (Fast_queue.to_list current_row, Buffer.contents field))
    in
    { t with
      field = Buffer.contents field
    ; current_row = Fast_queue.to_list current_row
    ; current_field = 0
    ; next_field_index = 0
    ; acc
    }
  ;;
end

module On_invalid_row = struct
  type 'a t =
    int String.Map.t
    -> string Fast_queue.t
    -> exn
    -> [`Skip | `Yield of 'a | `Raise of exn]

  let raise _ _ exn = `Raise exn
  let skip _ _ _ = `Skip
  let create = Fn.id
end

type 'a on_invalid_row = 'a On_invalid_row.t

module Builder = struct
  type _ t =
    | Column : int -> string t
    | Header : string -> string t
    | Return : 'a -> 'a t
    | Apply : ('b -> 'a) t * 'b t -> 'a t
    | Map : ('b -> 'a) * 'b t -> 'a t
    | Map2 : ('b -> 'c -> 'a) * 'b t * 'c t -> 'a t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | Lambda : (int String.Map.t -> string Fast_queue.t -> 'a) -> 'a t

  module T = struct
    let return x = Return x

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
    let all_ignore = all_unit
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

  let at_index i ~f = Map (f, Column i)
  let at_header h ~f = Map (f, Header h)
  let lambda f = Lambda f

  module Let_syntax = struct
    module Let_syntax = struct
      include T

      module Open_on_rhs = struct
        let at_index = at_index
        let at_header = at_header
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
          (int String.Map.t -> string Fast_queue.t -> 'a) * int String.Map.t
        -> 'a t

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
      in
      fields t
    ;;

    let build t =
      let rec build' : type a. a t -> string Fast_queue.t -> a =
        fun t row ->
          match t with
          | Return x -> x
          | Column i -> Fast_queue.nth_exn row i
          | Apply (f, x) -> (build' f row) (build' x row)
          | Map (f, x) -> f (build' x row)
          | Map2 (f, x, y) -> f (build' x row) (build' y row)
          | Both (x, y) -> build' x row, build' y row
          | Lambda (f, header_map) -> f header_map row
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
        in
        build' (remap t), Some (Array.of_list fields_used)
    ;;
  end

  let build ~header_map t =
    let rec transform : type a. a t -> a Without_headers.t = function
      | Return x -> Without_headers.Return x
      | Column i -> Without_headers.Column i
      | Header h ->
        let column_index =
          match String.Map.find_exn header_map h with
          | index -> index
          | exception (Not_found_s _ | Caml.Not_found) ->
            raise_s
              [%message "Header not found" (h : string) (header_map : int String.Map.t)]
        in
        Without_headers.Column column_index
      | Apply (f, x) -> Without_headers.Apply (transform f, transform x)
      | Map (f, x) -> Without_headers.Map (f, transform x)
      | Map2 (f, x, y) -> Without_headers.Map2 (f, transform x, transform y)
      | Both (x, y) -> Without_headers.Both (transform x, transform y)
      | Lambda f -> Without_headers.Lambda (f, header_map)
    in
    let transformed = transform t in
    Without_headers.build transformed
  ;;

  let rec headers_used : type a. a t -> String.Set.t = function
    | Return _ -> String.Set.empty
    | Column _ -> String.Set.empty
    | Header h -> String.Set.singleton h
    | Apply (f, x) -> Set.union (headers_used f) (headers_used x)
    | Map (_, x) -> headers_used x
    | Map2 (_, x, y) -> Set.union (headers_used x) (headers_used y)
    | Both (x, y) -> Set.union (headers_used x) (headers_used y)
    | Lambda _ -> String.Set.empty
  ;;
end

module Header_parse : sig
  (** Type [t] represents an incomplete header parse. Keep calling [input] on it until you
      get a map from header name to column number. *)
  type t

  val create
    :  ?strip:bool
    -> ?sep:char
    -> ?quote:[`No_quoting | `Using of char]
    -> ?header:Header.t
    -> _ Builder.t
    -> (t, int String.Map.t) Either.t

  (** [input t ~len s] reads the first [len] bytes from [s] and returns either [t] or
      [header_map, unused_input]. *)
  val input : t -> len:int -> Bytes.t -> (t, int String.Map.t * string) Either.t

  val input_string : t -> len:int -> string -> (t, int String.Map.t * string) Either.t
  val is_at_beginning_of_row : t -> bool
end = struct
  (* This exception is used to return early from the parser, so we don't consume more
     input than necessary. This is almost [With_return], except declaring the exception at
     top-level so we can freely pass around a closure that raises it. *)
  exception Header_parsed of string array * int

  type t =
    { state : unit Parse_state.t
    ; transform : string array -> int String.Map.t
    }

  let is_at_beginning_of_row t = Parse_state.is_at_beginning_of_row t.state

  let header_map_opt header_row =
    Array.foldi header_row ~init:String.Map.empty ~f:(fun i map header ->
      match header with
      | None -> map
      | Some header -> Map.set map ~key:header ~data:i)
  ;;

  let header_map header_row = header_map_opt (Array.map ~f:Option.some header_row)

  let limit_header builder limit_headers' csv_headers' =
    let limit_headers = String.Set.of_list limit_headers' in
    let builder_headers = Builder.headers_used builder in
    if not (Set.is_subset builder_headers ~of_:limit_headers)
    then
      raise_s
        [%message
          "Builder uses header not specified in `Limit"
            (builder_headers : String.Set.t)
            (limit_headers : String.Set.t)];
    let csv_headers = String.Set.of_array csv_headers' in
    let missing = Set.diff limit_headers csv_headers in
    if not (Set.is_empty missing)
    then
      raise_s
        [%message
          "Header specified in `Limit not present in csv document"
            (limit_headers : String.Set.t)
            (csv_headers : String.Set.t)
            (missing : String.Set.t)];
    header_map csv_headers'
  ;;

  let create' ?strip ?sep ?quote transform =
    let f offset () row = raise (Header_parsed (Fast_queue.to_array row, offset)) in
    { state = Parse_state.create ?strip ?sep ?quote ~fields_used:None ~init:() ~f ()
    ; transform
    }
  ;;

  let create ?strip ?sep ?quote ?(header = `No) builder =
    match header with
    | `No -> Second String.Map.empty
    | `Add headers -> Second (header_map (Array.of_list headers))
    | `Yes ->
      let f headers = limit_header builder (Array.to_list headers) headers in
      First (create' ?strip ?sep ?quote f)
    | `Limit headers ->
      let f csv_headers = limit_header builder headers csv_headers in
      First (create' ?strip ?sep ?quote f)
    | `Replace headers ->
      let f _ = header_map (Array.of_list headers) in
      First (create' ?strip ?sep ?quote f)
    | `Transform f ->
      let f headers = header_map (Array.of_list (f (Array.to_list headers))) in
      First (create' ?strip ?sep ?quote f)
    | `Filter_map f ->
      let f headers = header_map_opt (Array.of_list (f (Array.to_list headers))) in
      First (create' ?strip ?sep ?quote f)
  ;;

  let input_string t ~len input =
    try First { t with state = Parse_state.input_string t.state ~len input } with
    | Header_parsed (row, offset) ->
      Second (t.transform row, String.sub input ~pos:offset ~len:(len - offset))
  ;;

  let input t ~len input =
    try First { t with state = Parse_state.input t.state ~len input } with
    | Header_parsed (row, offset) ->
      Second (t.transform row, Bytes.To_string.sub input ~pos:offset ~len:(len - offset))
  ;;
end

let create_parse_state
      ?strip
      ?sep
      ?quote
      ?(on_invalid_row = On_invalid_row.raise)
      ~header_map
      builder
      ~init
      ~f
  =
  let row_to_'a, fields_used = Builder.build ~header_map builder in
  let f _offset init row =
    try f init (row_to_'a row) with
    | exn ->
      (match on_invalid_row header_map row exn with
       | `Yield x -> f init x
       | `Skip -> init
       | `Raise exn -> raise exn)
  in
  Parse_state.create ?strip ?sep ?quote ~fields_used ~init ~f ()
;;

let fold_string ?strip ?sep ?quote ?header ?on_invalid_row builder ~init ~f csv_string =
  match
    match Header_parse.create ?strip ?sep ?quote ?header builder with
    | Second header_map -> Some (header_map, csv_string)
    | First header_parse ->
      (match
         Header_parse.input_string
           header_parse
           ~len:(String.length csv_string)
           csv_string
       with
       | First _ ->
         if String.is_empty csv_string
         then None
         else
           raise_s
             [%message
               "String ended mid-header row"
                 (csv_string : string)
                 (sep : char option)
                 (header : Header.t option)]
       | Second (header_map, csv_string) -> Some (header_map, csv_string))
  with
  | None -> init
  | Some (header_map, csv_string) ->
    Parse_state.input_string
      (create_parse_state ?strip ?sep ?quote ?on_invalid_row ~header_map builder ~init ~f)
      csv_string
    |> Parse_state.finish
    |> Parse_state.acc
;;

include Builder

module Row = struct
  include Row

  let create_of_fq header_map row_queue =
    create' header_map (Fast_queue.to_array row_queue)
  ;;

  let builder = Builder.lambda create_of_fq
end

(** Non-applicative interface. All readers defined below will raise if they encounter unparsable content. *)

module Manual = struct
  let manual_parse_data parse_state input =
    let parse_state =
      match input with
      | `Eof -> Parse_state.finish parse_state
      | `Data s -> Parse_state.input_string parse_state s
    in
    let queue = Parse_state.acc parse_state in
    let result = Fast_queue.to_list queue in
    Fast_queue.clear queue;
    Second parse_state, result
  ;;

  let create_parse_state ?strip ?sep ?quote header_map =
    Parse_state.create
      ?strip
      ?sep
      ?quote
      ~fields_used:None
      ~init:(Fast_queue.create ())
      ~f:(fun _ queue row ->
        Fast_queue.enqueue queue (Row.create_of_fq header_map row);
        queue)
      ()
  ;;

  let manual_parse_header ?strip ?sep ?quote header_state input =
    let input =
      match input with
      | `Eof -> ""
      | `Data s -> s
    in
    match Header_parse.input_string header_state ~len:(String.length input) input with
    | First header_state -> First header_state, []
    | Second (header_map, input) ->
      let state = create_parse_state ?strip ?sep ?quote header_map in
      manual_parse_data state (`Data input)
  ;;

  let create_manual ?strip ?sep ~header () =
    let state =
      Header_parse.create ?strip ?sep ~header (Builder.return ())
      |> Either.Second.map ~f:(create_parse_state ?strip ?sep)
      |> ref
    in
    let parse_chunk input =
      let state', results =
        match !state with
        | First state -> manual_parse_header ?strip ?sep state input
        | Second state -> manual_parse_data state input
      in
      state := state';
      results
    in
    stage parse_chunk
  ;;
end

let create_manual = Manual.create_manual

let parse_string ?strip ?sep ~header csv_string =
  fold_string
    ?strip
    ?sep
    ~header
    Row.builder
    csv_string
    ~init:(Fast_queue.create ())
    ~f:(fun queue row ->
      Fast_queue.enqueue queue row;
      queue)
  |> Fast_queue.to_list
;;
