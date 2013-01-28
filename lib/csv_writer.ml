open Core.Std

(**
 *
 * The standard string transformations are split in two:
 * - one to get the length of the result (can work on substring)
 * - another one to perform the action (with string blit semmantic)
 *
 * Common arguments
 *
 * -> to figure out how to escape/print quote and separators.
 * -> to operate on substrings : pos len
 * -> to perform string transformations: all the blit arguments
 *
 *)

(** Field handling *)
let rec quote_blit_loop ~quote ~src ~dst ~src_pos ~dst_pos src_end =
  if src_pos = src_end then
    dst_pos
  else
    match src.[src_pos] with
    | c when c = quote ->
        dst.[dst_pos]     <- quote;
        dst.[dst_pos + 1] <- quote;
        quote_blit_loop
          ~quote
          ~src
          ~dst
          ~src_pos:(src_pos + 1)
          ~dst_pos:(dst_pos + 2)
          src_end
    | c ->
        dst.[dst_pos] <- c;
        quote_blit_loop
          ~quote
          ~src
          ~dst
          ~src_pos:(src_pos + 1)
          ~dst_pos:(dst_pos + 1)
          src_end

let quote_blit ~quote ~src ~dst ~src_pos ~dst_pos ~len =
  quote_blit_loop ~quote ~src ~dst ~src_pos ~dst_pos (src_pos + len)

(** Find the length of a quoted field... *)
let rec quote_len_loop ~quote ~sep ~pos ~end_pos ~should_escape s acc =
  if pos = end_pos then
    if should_escape then
      Some acc
    else
      None
  else match s.[pos] with
  | c when c = quote ->
      quote_len_loop
        s
        ~quote
        ~sep
        ~pos:(pos + 1)
        ~end_pos
        ~should_escape:true
        (acc + 1)
  | c when c = sep ->
      quote_len_loop
        s
        ~quote
        ~sep
        ~pos:(pos + 1)
        ~end_pos
        ~should_escape:true
        acc
  | '\n' ->
      quote_len_loop
        s
        ~quote
        ~sep
        ~pos:(pos + 1)
        ~end_pos
        ~should_escape:true
        acc
  | _ ->
      quote_len_loop
        s
        ~quote
        ~sep
        ~pos:(pos + 1)
        ~end_pos
        ~should_escape
        acc

let quote_len ~quote ~sep ~pos ~len s =
  if len = 0 then
    None
  else
    let trailling_ws =
      s.[pos]=' '
      || s.[pos]= '\t'
      || s.[pos + len -1] = ' '
      || s.[pos + len -1] = '\t'
    in
    quote_len_loop s
      ~quote
      ~sep
      ~pos
      ~end_pos:(len + pos)
      ~should_escape:trailling_ws len

(** Line handling *)
let rec line_spec_loop ~quote ~sep esc_acc size = function
  | [] when esc_acc = [] -> [],0
  | []  -> List.rev esc_acc,(size -1) (* We overshot our count by one comma*)
  | h::t ->
      let len = String.length h in
      begin match quote_len h ~quote ~sep ~len ~pos:0 with
      | None ->
          line_spec_loop ~quote ~sep
            ((false,h)::esc_acc) (size + len + 1) t
      | Some qlen ->
          line_spec_loop ~quote ~sep
            ((true,h)::esc_acc) (size + qlen + 3) t
      end

let line_spec ~quote ~sep line =
  line_spec_loop ~quote ~sep [] 0 line

let field_blit ~quote ~dst ~pos = function
  | true,h ->
      dst.[pos] <- quote;
      let len = String.length h in
      let qpos =
        quote_blit ~quote ~src:h ~src_pos:0 ~dst ~dst_pos:(pos+1) ~len
      in
      dst.[qpos] <- quote;
      qpos + 1
  | false,h ->
      let len = String.length h in
      String.blit ~dst_pos:pos ~src_pos:0 ~dst ~src:h ~len;
      pos + len

(** Tables *)
let rec line_blit_loop ~quote ~sep ~dst ~pos = function
  | [] -> pos
  | [v] ->
      field_blit ~quote:'"' ~dst ~pos v
  | v::((_::_) as t) ->
      let pos = field_blit ~quote:'"' ~dst ~pos v in
      dst.[pos] <- sep;
      line_blit_loop ~quote ~sep ~dst ~pos:(pos + 1) t

let rec output_lines_loop ~quote ~sep ~buff oc = function
  | [] -> ()
  | h::t ->
      let spec,len = line_spec_loop ~quote ~sep [] 0 h in
      let buff = if String.length buff < len then
        String.create (2*len)
      else
        buff
      in
      ignore (line_blit_loop ~quote ~sep ~dst:buff ~pos:0 spec:int);
      output oc buff 0 len;
      output_string oc "\r\n";
      output_lines_loop ~quote ~sep ~buff oc t

let line_to_string ?(quote='"') ?(sep=',') l =
  let spec,len = line_spec_loop ~quote ~sep [] 0 l in
  let res = String.create len in
  ignore (line_blit_loop ~quote ~sep ~dst:res ~pos:0 spec:int);
  res

let maybe_escape_field ?(quote='"') ?(sep=',') s =
  let len = String.length s in
  match quote_len s ~quote ~sep ~len ~pos:0 with
  | None -> s
  | Some qlen ->
      let res = String.create (qlen+2) in
      res.[0] <- quote;
      res.[qlen+1] <- quote;
      ignore
        (quote_blit ~quote ~src:s ~src_pos:0 ~dst:res ~dst_pos:1 ~len :int);
      res

let escape_field ?(quote='"') s =
  let len = String.length s in
  match quote_len s ~quote ~sep:',' ~len ~pos:0 with
  | None ->
      let res = String.create (len+2) in
      res.[0] <- quote;
      res.[len+1] <- quote;
      String.blit ~src_pos:0 ~dst_pos:1 ~len ~src:s ~dst:res;
      res
  | Some qlen ->
      let res = String.create (qlen+2) in
      res.[0] <- quote;
      res.[qlen+1] <- quote;
      ignore
        (quote_blit ~quote ~src:s ~src_pos:0 ~dst:res ~dst_pos:1 ~len :int);
      res

let output_lines ?(quote='"') ?(sep=',') oc l =
  output_lines_loop ~quote ~sep ~buff:(String.create 256) oc l
