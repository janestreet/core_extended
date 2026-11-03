open Core

module type To_string = Write_intf.To_string

type -'a t =
  { headers : string list
  ; to_columns : 'a -> tail:string list -> string list
  }
[@@deriving fields ~getters]

module type S = sig
  type -'a delimited_writer := 'a t
  type t

  val delimited_writer : t delimited_writer
end

let empty @ portable = { headers = []; to_columns = (fun _ ~tail -> tail) }

[%%template
[@@@mode.default p = (nonportable, portable)]

let column (to_string @ p) ~header =
  { headers = [ header ]; to_columns = (fun x ~tail -> to_string x :: tail) }
;;

let column_opt ?(default = "") (to_string @ p) ~header =
  (column [@mode p]) (fun x -> Option.value_map x ~default ~f:to_string) ~header
;;

let append (l @ p) (r @ p) =
  let to_columns_l = l.to_columns
  and to_columns_r = r.to_columns in
  { headers = List.append l.headers r.headers
  ; to_columns = (fun x ~tail -> to_columns_l x ~tail:(to_columns_r x ~tail))
  }
;;]

let of_list = function
  | [] -> empty
  | [ x ] -> x
  | first :: others -> List.fold others ~init:first ~f:append
;;

let%template[@mode p = portable] of_list (ts @ p) =
  match Modes.Portable.wrap_list ts with
  | [] -> empty
  | [ x ] -> Modes.Portable.unwrap x
  | first :: others ->
    List.fold others ~init:first ~f:(fun l r ->
      (append [@mode portable]) (Modes.Portable.unwrap l) (Modes.Portable.unwrap r)
      |> Modes.Portable.wrap)
    |> Modes.Portable.unwrap
;;

let%template[@mode p = (nonportable, portable)] contra_map (x @ p) ~(f @ p) =
  let to_columns = x.to_columns in
  { x with to_columns = (fun x ~tail -> to_columns (f x) ~tail) }
;;

let map_headers t ~f = { t with headers = List.map t.headers ~f }

let optional ?(default = "") t =
  { t with
    to_columns =
      (fun x ~tail ->
        match x with
        | Some x -> t.to_columns x ~tail
        | None ->
          (* Header order doesn't matter as each column has the same value *)
          List.fold t.headers ~init:tail ~f:(fun tail _ -> default :: tail))
  }
;;

let to_columns t x = to_columns t x ~tail:[]

module Fields_O = struct
  let ( !! ) to_string field =
    let read_field = Field.get field in
    column (fun r -> to_string (read_field r)) ~header:(Field.name field)
  ;;

  let ( !> ) inner field =
    map_headers
      inner
      ~f:
        (let prefix = Field.name field ^ "_" in
         fun name -> prefix ^ name)
    |> contra_map ~f:(Field.get field)
  ;;
end

module O = struct
  let ( <<| ) t f = contra_map t ~f
  let ( <> ) = append
end

let to_string_m (type t) (module T : To_string with type t = t) = T.to_string
let column_m m ~header = column (to_string_m m) ~header
let column_m_opt ?default m ~header = column_opt ?default (to_string_m m) ~header

module Expert = struct
  (* The standard string transformations are split in two:
     - one to get the length of the result (can work on substring)
     - another one to perform the action (with string blit semmantic)

     Common arguments

     -> to figure out how to escape/print quote and separators. -> to operate on
     substrings : pos len -> to perform string transformations: all the blit arguments
  *)

  (* Field handling *)
  let rec quote_blit_loop ~quote ~src ~dst ~src_pos ~dst_pos src_end =
    if src_pos = src_end
    then dst_pos
    else (
      match src.[src_pos] with
      | c when Char.equal c quote ->
        Bytes.set dst dst_pos quote;
        Bytes.set dst (dst_pos + 1) quote;
        quote_blit_loop
          ~quote
          ~src
          ~dst
          ~src_pos:(src_pos + 1)
          ~dst_pos:(dst_pos + 2)
          src_end
      | c ->
        Bytes.set dst dst_pos c;
        quote_blit_loop
          ~quote
          ~src
          ~dst
          ~src_pos:(src_pos + 1)
          ~dst_pos:(dst_pos + 1)
          src_end)
  ;;

  let quote_blit ~(quote : char) ~src ~dst ~src_pos ~dst_pos ~len =
    quote_blit_loop ~quote ~src ~dst ~src_pos ~dst_pos (src_pos + len)
  ;;

  open! Unboxed

  module String = struct
    include String

    (*= JS-only:
    *)

    (* i64 is the best type we have to store char--it won't tag. We don't have a usable
       char# yet.

       We can't yet _index_ string by i64.
    *)
    let[@inline] unsafe_get__i64 s pos = unsafe_get s pos |> Char.to_int |> I64.of_int
  end

  let quote_len ~quote ~sep ~pos ~len s =
    if len = 0
    then None
    else (
      let trailing_ws =
        let open I64.O in
        (* optimized is_whitespace: bitmask the position of whitespace chars in an i64 and
           shift to it (checking for out of range.) *)
        let mask =
          let c c = #1L lsl Char.to_int c in
          c ' ' lor c '\n' lor c '\t' lor c '\011' lor c '\012' lor c '\r'
        in
        let zero = Sys.opaque_identity #0L in
        let[@inline] check c =
          I64.select (c < #64L) ((mask lsr I64.to_int_trunc c) land #1L) zero
        in
        let c1 = String.unsafe_get__i64 s pos in
        let c2 = String.unsafe_get__i64 s Int.O.(pos + len - 1) in
        check c1 lor check c2
      in
      (* Work in unboxed i64 (best type we have that doesn't do a lot of tagging) *)
      let quote = Char.to_int quote |> I64.of_int in
      let sep = Char.to_int sep |> I64.of_int in
      let newline = Char.to_int '\n' |> I64.of_int in
      let open I64.O in
      let open I64.Ref.O in
      (* we repeatedly (conditionally) move out of registers; this [opaque_identity]
         prevents constant propagation which just means re-loading into a register
         repeatedly. *)
      let one = #1L |> Sys.opaque_identity in
      let zero = #0L |> Sys.opaque_identity in
      (* no let mutable in upstream-compatible sigh *)
      let quote_count = ref zero in
      let should_escape = trailing_ws |> ref in
      (* I'd index this loop by i64, but you can't do anything efficient with accessing
         strings yet. *)
      let end_pos = Int.O.(pos + len) in
      let rec loop pos =
        let c = String.unsafe_get__i64 s pos in
        let pos = Int.O.(pos + 1) in
        should_escape := I64.select (c = sep) one !should_escape;
        should_escape := I64.select (c = newline) one !should_escape;
        quote_count += I64.select (c = quote) one zero;
        if Int.O.(pos < end_pos) then loop pos else ()
      in
      loop pos;
      let quote_count = !quote_count in
      let should_escape = !should_escape in
      match quote_count lor should_escape = #0L with
      | true -> None
      | false ->
        let quote_count = I64.to_int_trunc quote_count in
        let quoted_len = Int.O.(quote_count + len) in
        Some quoted_len)
  ;;

  (** Tables *)

  let maybe_escape_field ?(quote = '"') ?(sep = ',') s =
    let len = String.length s in
    match quote_len s ~quote ~sep ~len ~pos:0 with
    | None -> s
    | Some qlen ->
      let res = Bytes.create (qlen + 2) in
      Bytes.set res 0 quote;
      Bytes.set res (qlen + 1) quote;
      ignore (quote_blit ~quote ~src:s ~src_pos:0 ~dst:res ~dst_pos:1 ~len : int);
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
  ;;

  let escape_field ?(quote = '"') s =
    let len = String.length s in
    match quote_len s ~quote ~sep:',' ~len ~pos:0 with
    | None ->
      let res = Bytes.create (len + 2) in
      Bytes.set res 0 quote;
      Bytes.set res (len + 1) quote;
      Bytes.From_string.blit ~src_pos:0 ~dst_pos:1 ~len ~src:s ~dst:res;
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
    | Some qlen ->
      let res = Bytes.create (qlen + 2) in
      Bytes.set res 0 quote;
      Bytes.set res (qlen + 1) quote;
      ignore (quote_blit ~quote ~src:s ~src_pos:0 ~dst:res ~dst_pos:1 ~len : int);
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
  ;;
end

let line_break_string = function
  | `Windows -> "\r\n"
  | `Unix -> "\n"
;;

module By_row = struct
  type row = string list

  (** Line handling *)
  let rec line_spec_loop ~quote ~sep esc_acc size acc =
    match acc, esc_acc with
    | [], [] -> [], 0
    | [], _ -> List.rev esc_acc, size - 1 (* We overshot our count by one comma *)
    | h :: t, _ ->
      let len = String.length h in
      (match Expert.quote_len h ~quote ~sep ~len ~pos:0 with
       | None -> line_spec_loop ~quote ~sep ((false, h) :: esc_acc) (size + len + 1) t
       | Some qlen ->
         line_spec_loop ~quote ~sep ((true, h) :: esc_acc) (size + qlen + 3) t)
  ;;

  let field_blit ~quote ~dst ~pos = function
    | true, h ->
      Bytes.set dst pos quote;
      let len = String.length h in
      let qpos =
        Expert.quote_blit ~quote ~src:h ~src_pos:0 ~dst ~dst_pos:(pos + 1) ~len
      in
      Bytes.set dst qpos quote;
      qpos + 1
    | false, h ->
      let len = String.length h in
      Bytes.From_string.blit ~dst_pos:pos ~src_pos:0 ~dst ~src:h ~len;
      pos + len
  ;;

  let rec line_blit_loop ~quote ~sep ~dst ~pos = function
    | [] -> pos
    | [ v ] -> field_blit ~quote:'"' ~dst ~pos v
    | v :: (_ :: _ as t) ->
      let pos = field_blit ~quote:'"' ~dst ~pos v in
      Bytes.set dst pos sep;
      line_blit_loop ~quote ~sep ~dst ~pos:(pos + 1) t
  ;;

  let line_to_string ?(quote = '"') ?(sep = ',') l =
    let spec, len = line_spec_loop ~quote ~sep [] 0 l in
    let res = Bytes.create len in
    ignore (line_blit_loop ~quote ~sep ~dst:res ~pos:0 spec : int);
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
  ;;

  module Out_channel = struct
    type t =
      { oc : Out_channel.t
      ; mutable buff : Bytes.t
      ; quote : char
      ; sep : char
      ; line_break : string
      }

    let create ?(quote = '"') ?(sep = ',') ?(line_breaks = `Windows) oc =
      { quote
      ; sep
      ; line_break = line_break_string line_breaks
      ; buff = Bytes.create 256
      ; oc
      }
    ;;

    let output_line t h =
      let { quote; sep; line_break; oc; _ } = t in
      let spec, len = line_spec_loop ~quote ~sep [] 0 h in
      if Bytes.length t.buff < len then t.buff <- Bytes.create (2 * len);
      ignore (line_blit_loop ~quote ~sep ~dst:t.buff ~pos:0 spec : int);
      Out_channel.output oc ~buf:t.buff ~pos:0 ~len;
      Out_channel.output_string oc line_break
    ;;
  end

  let output_lines ?quote ?sep ?line_breaks oc l =
    let ch = Out_channel.create ?quote ?sep ?line_breaks oc in
    List.iter l ~f:(Out_channel.output_line ch)
  ;;
end

let line ?quote ?sep t a = By_row.line_to_string ?quote ?sep (to_columns t a)
let header_line ?quote ?sep t = By_row.line_to_string ?quote ?sep (headers t)

module Out_channel = struct
  type 'a write = 'a t

  type 'a t =
    { ch : By_row.Out_channel.t
    ; t : 'a write
    }

  let channel t = t.ch.oc

  let create ?quote ?sep ?line_breaks ~write_header t ch =
    let t = { ch = By_row.Out_channel.create ?quote ?sep ?line_breaks ch; t } in
    if write_header then By_row.Out_channel.output_line t.ch (headers t.t);
    t
  ;;

  let output_row t row = By_row.Out_channel.output_line t.ch (to_columns t.t row)
end

let save ?quote ?sep ?line_breaks ~write_header writer file rows =
  let ch =
    Out_channel.create
      ?quote
      ?sep
      ?line_breaks
      ~write_header
      writer
      (Core.Out_channel.create file)
  in
  List.iter rows ~f:(Out_channel.output_row ch);
  Core.Out_channel.close ch.ch.oc
;;

let to_string ?quote ?sep ?(line_breaks = `Windows) ~write_header t rows =
  let line_to_string = By_row.line_to_string ?quote ?sep in
  let line_break_string = line_break_string line_breaks in
  let content_rows = List.map rows ~f:(fun row -> line_to_string (to_columns t row)) in
  String.concat
    ~sep:line_break_string
    (if write_header then line_to_string (headers t) :: content_rows else content_rows)
  ^ line_break_string
;;
