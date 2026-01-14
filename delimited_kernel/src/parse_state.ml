open Core
module Row_buffer = Append_only_buffer

exception Bad_csv_formatting of string list * string

module Step = struct
  type t =
    | Row_start
    | Field_start
    | In_unquoted_field
    | In_quoted_field
    | In_quoted_field_after_quote
end

open Step

module Config = struct
  type 'a t =
    { sep : char
    ; quote : char
    ; use_quoting : bool
    ; strip : bool
    ; f : line_number:int -> 'a -> string Row_buffer.t -> 'a
    ; fields_used : int array option
    }

  let create ~sep ~quote ~strip ~f ~fields_used =
    let fields_used =
      match fields_used with
      | None -> None
      | Some fields_used as x
        when Array.is_sorted_strictly fields_used ~compare:Int.ascending -> x
      | Some fields_used ->
        Some
          (Array.of_list
             (List.dedup_and_sort (Array.to_list fields_used) ~compare:Int.ascending))
    in
    (match quote with
     | `Using c when Char.equal c sep ->
       invalid_arg
         "Delimited_kernel.Parse_state.create: cannot use the same character for [sep] \
          and [quote]"
     | `Using (('\r' | '\n') as c) ->
       invalid_arg
         (sprintf "Delimited_kernel.Parse_state.create: invalid [quote] character %C" c)
     | _ -> ());
    (match sep with
     | ('\r' | '\n') as c ->
       invalid_arg
         (sprintf "Delimited_kernel.Parse_state.create: invalid [sep] character %C" c)
     | _ -> ());
    { sep
    ; quote =
        (match quote with
         | `Using char -> char
         | `No_quoting -> '"')
    ; use_quoting =
        (match quote with
         | `Using _ -> true
         | `No_quoting -> false)
    ; strip
    ; f
    ; fields_used
    }
  ;;
end

module State = struct
  type 'a t =
    { field_buffer : Buffer.t
    ; row_buffer : string Row_buffer.t
    ; current_field : int
    ; current_line_number : int (** The current line number *)
    ; row_line_number : int (** The line number of the beginning of the current row *)
    ; acc : 'a
    ; step : Step.t
    ; finish : bool
    }

  let create ~init ~start_line_number =
    { acc = init
    ; step = Row_start
    ; field_buffer = Buffer.create 0
    ; row_buffer = Row_buffer.create ()
    ; current_field = 0
    ; row_line_number = start_line_number
    ; current_line_number = start_line_number
    ; finish = false
    }
  ;;

  let acc { acc; _ } = acc
  let set_acc t acc = { t with acc }
end

type 'a t =
  { config : 'a Config.t
  ; state : 'a State.t
  }

let acc t = State.acc t.state
let set_acc t acc = { t with state = State.set_acc t.state acc }

let create
  ?(strip = false)
  ?(sep = ',')
  ?(quote = `Using '"')
  ?(start_line_number = 1)
  ~fields_used
  ~init
  ~f
  ()
  =
  { config = Config.create ~sep ~quote ~strip ~f ~fields_used
  ; state = State.create ~init ~start_line_number
  }
;;

module Char_kind = struct
  type t =
    | Backslash_r
    | Newline
    | Sep
    | Quote
    | Whitespace
    | Normal

  let of_char (t : _ Config.t) c =
    let open Char.Replace_polymorphic_compare in
    match c with
    | '\r' -> Backslash_r
    | '\n' -> Newline
    | _ when c = t.quote && t.use_quoting -> Quote
    | _ when c = t.sep -> Sep
    | _ when Char.is_whitespace c -> Whitespace
    | _ -> Normal
  ;;
end

module Mutable_state = struct
  (* We don't capture state [step] in here to avoid having to mutate the record at every
     single iteration *)
  type 'a t =
    { field_buffer : Buffer.t
    ; row_buffer : string Row_buffer.t
    ; config : 'a Config.t
    ; mutable current_field : int
    ; mutable enqueue : bool (* cache for should_enqueue *)
    ; mutable current_line_number : int
    ; mutable row_line_number : int
    ; mutable acc : 'a
    }

  let row_length t = Row_buffer.length t.row_buffer

  (* To reduce the number of allocations, we keep an array [fields_used] of the field
     indexes we care about. [current_field] is the position of the parser within the input
     row, and [next_field_index] is an index into the [fields_used] array indicating the
     next field that we need to store.

     If [fields_used] is None, we need to store every field.
  *)
  let should_enqueue fields_used state =
    match fields_used with
    | None -> true
    | Some array ->
      let next_field_index = row_length state in
      next_field_index < Array.length array
      && array.(next_field_index) = state.current_field
  ;;

  let create ~(config : 'a Config.t) ~(state : 'a State.t) =
    let state =
      { field_buffer = state.field_buffer
      ; row_buffer = state.row_buffer
      ; current_field = state.current_field
      ; enqueue = false
      ; config
      ; row_line_number = state.row_line_number
      ; current_line_number = state.current_line_number
      ; acc = state.acc
      }
    in
    if should_enqueue config.fields_used state then state.enqueue <- true;
    state
  ;;

  let emit_char t c = if t.enqueue then Buffer.add_char t.field_buffer c

  let emit_field state =
    if state.enqueue
    then (
      Row_buffer.append
        state.row_buffer
        (if state.config.strip
         then Shared.strip_buffer state.field_buffer
         else Buffer.contents state.field_buffer);
      Buffer.clear state.field_buffer);
    state.current_field <- state.current_field + 1;
    state.enqueue <- should_enqueue state.config.fields_used state
  ;;

  let emit_row state =
    let acc =
      state.config.f ~line_number:state.row_line_number state.acc state.row_buffer
    in
    state.acc <- acc;
    Row_buffer.lax_clear state.row_buffer;
    state.current_field <- 0;
    state.enqueue <- should_enqueue state.config.fields_used state;
    state.current_line_number <- state.current_line_number + 1;
    state.row_line_number <- state.current_line_number
  ;;

  let freeze ~step t : 'a State.t =
    { acc = t.acc
    ; step
    ; field_buffer = t.field_buffer
    ; row_buffer = t.row_buffer
    ; current_field = t.current_field
    ; current_line_number = t.current_line_number
    ; row_line_number = t.row_line_number
    ; finish = false
    }
  ;;

  let incr_line_number t = t.current_line_number <- t.current_line_number + 1
end

let input_aux ~get t ~pos ~len input =
  if t.state.finish
  then
    raise_s
      [%message
        "Delimited.Expert.Parse_state.input: Cannot feed more input to a state that has \
         already been finalized"];
  let state = Mutable_state.create ~config:t.config ~state:t.state in
  let feed_one c step =
    match step, Char_kind.of_char t.config c with
    | _, Backslash_r -> step
    | (Row_start | Field_start), Quote -> In_quoted_field
    | (Row_start | Field_start), Sep ->
      Mutable_state.emit_field state;
      Field_start
    | (Row_start | Field_start), Newline ->
      Mutable_state.emit_field state;
      Mutable_state.emit_row state;
      Row_start
    | (Row_start | Field_start), (Normal | Whitespace) ->
      Mutable_state.emit_char state c;
      In_unquoted_field
    | In_unquoted_field, Sep ->
      Mutable_state.emit_field state;
      Field_start
    | In_unquoted_field, Newline ->
      Mutable_state.emit_field state;
      Mutable_state.emit_row state;
      Row_start
    | In_unquoted_field, (Whitespace | Normal) ->
      Mutable_state.emit_char state c;
      step
    | In_unquoted_field, Quote ->
      Mutable_state.emit_char state c;
      step
    | In_quoted_field, Quote -> In_quoted_field_after_quote
    | In_quoted_field, Newline ->
      Mutable_state.emit_char state c;
      Mutable_state.incr_line_number state;
      step
    | In_quoted_field, (Normal | Sep | Whitespace) ->
      Mutable_state.emit_char state c;
      step
    | In_quoted_field_after_quote, Quote ->
      (* doubled quote *)
      Mutable_state.emit_char state c;
      In_quoted_field
    | In_quoted_field_after_quote, _ when Char.equal c '0' ->
      Mutable_state.emit_char state '\000';
      In_quoted_field
    | In_quoted_field_after_quote, Sep ->
      Mutable_state.emit_field state;
      Field_start
    | In_quoted_field_after_quote, Newline ->
      Mutable_state.emit_field state;
      Mutable_state.emit_row state;
      Row_start
    | In_quoted_field_after_quote, Whitespace -> step
    | In_quoted_field_after_quote, Normal ->
      failwithf
        "In_quoted_field_after_quote looking at '%c' (line_number=%d)"
        c
        state.current_line_number
        ()
  in
  let loop_bound = len + pos in
  let rec loop i step =
    if i >= loop_bound
    then step
    else (
      let c = get input i in
      let step = feed_one c step in
      loop (i + 1) step)
  in
  let step = loop pos t.state.step in
  let state = Mutable_state.freeze ~step state in
  { t with state }
;;

let input t ?(pos = 0) ?len input =
  let len =
    match len with
    | None -> Bytes.length input - pos
    | Some len -> len
  in
  if len < 0 || pos < 0 || pos + len > Bytes.length input
  then invalid_arg "Delimited_kernel.Parse_state.input: index out of bound";
  input_aux ~get:Bytes.unsafe_get t ~pos ~len input
;;

let input_string t ?(pos = 0) ?len input =
  let len =
    match len with
    | None -> String.length input - pos
    | Some len -> len
  in
  if len < 0 || pos < 0 || pos + len > String.length input
  then invalid_arg "Delimited_kernel.Parse_state.input_string: index out of bound";
  input_aux ~get:String.unsafe_get t ~pos ~len input
;;

let current_line_number t = t.state.current_line_number

let at_beginning_of_row t =
  match t.state.step with
  | Row_start -> true
  | Field_start | In_quoted_field | In_quoted_field_after_quote | In_unquoted_field ->
    false
;;

let finish ({ config; state } as t) =
  if t.state.finish
  then t
  else (
    let state = Mutable_state.create ~config ~state in
    (match t.state.step with
     | Row_start -> ()
     | Field_start ->
       Mutable_state.emit_field state;
       Mutable_state.emit_row state
     | In_unquoted_field | In_quoted_field_after_quote ->
       Mutable_state.emit_field state;
       Mutable_state.emit_row state
     | In_quoted_field ->
       raise
         (Bad_csv_formatting
            (Row_buffer.to_list t.state.row_buffer, Buffer.contents t.state.field_buffer)));
    let state = { (Mutable_state.freeze ~step:t.state.step state) with finish = true } in
    { t with state })
;;
