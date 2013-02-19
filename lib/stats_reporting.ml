open Core.Std
module Cycles = Time_stamp_counter.Cycles

type field = int with sexp, bin_io
type type_desc = string with sexp, bin_io
type name = string with sexp, bin_io

let debug = false

type header = {
  snapshot : Cycles.snapshot;
  mutable msg : string;
} with bin_io

type t =
| New_field of field * string * type_desc * name
| Int_datum of field * int * Cycles.t
| Float_datum of field * float * Cycles.t
with sexp, bin_io

type report = {
  mutable data : Bigstring.t;
  mutable used : int;
  mutable previous : (Bigstring.t * int) list;
}


let allow_for_header = 256
let field_counter_ref = ref 1


let lazy_header = lazy {
  snapshot = Cycles.get_snapshot ();
  msg = "";
}

let report = {
  data = Bigstring.create 10000000;
  used = allow_for_header;
  previous = [];
}


let expand_report_buffer () =
  let buf = Bigstring.create (Bigstring.length report.data) in
  report.previous <- (report.data, report.used) :: report.previous;
  report.data <- buf;
  report.used <- 0

let write_to_report_buffer t =
  let writer = bin_writer_t in
  let len  = bin_size_t t in
  if len + report.used > (Bigstring.length report.data)
  then expand_report_buffer ();
  report.used <- writer.Bin_prot.Type_class.write report.data ~pos:(report.used) t


let create_field  ~desc ~type_desc ~name =
  let field = !field_counter_ref in
  field_counter_ref := !field_counter_ref + 1;
  write_to_report_buffer (New_field (field, desc, type_desc, name));
  field

let add_datum field num =
  write_to_report_buffer (Int_datum (field, num, Cycles.now()))

let add_datum_float field num =
  write_to_report_buffer (Float_datum (field, num, Cycles.now()))

let resize_if_required () =
  let perc_used () =
    (report.used * 100) / (Bigstring.length report.data)
  in
  if perc_used () > 95
  then begin
    if debug then printf "Resizing stats_reporting buffer\n%!";
    expand_report_buffer ()
  end

let write_report_to_file () =
  let fn = "stats.data" in
  let fn_old = "stats.data.old" in
  begin match Sys.file_exists fn with
  | `Yes -> Unix.rename ~src:fn ~dst:fn_old
  | `No | `Unknown -> ()
  end;
  let ls = (report.data, report.used) :: report.previous in
  let ls = List.rev ls in
  ignore (Unix.with_file fn
            ~mode:[Unix.O_CREAT; Unix.O_WRONLY]
            ~f:(fun fd ->
              List.iter ls ~f:(fun (data, len) ->
                ignore (Bigstring.write fd ~len data))))


let read_msg_and_snapshot ~file =
  let size = Int64.to_int_exn (Extended_sys.file_size_exn file) in
  Unix.with_file file
    ~mode:[Unix.O_RDONLY]
    ~f:(fun fd ->
      let str = Bigstring.map_file ~shared:false fd size in
      let pos_ref = ref 0 in
      let header = bin_read_header str ~pos_ref in
      header.msg, header.snapshot)

let fold ~file ~init ~f =
  let size = Int64.to_int_exn (Extended_sys.file_size_exn file) in
  if debug then printf "File %s (%d bytes).\n%!" file size;
  Unix.with_file file
    ~mode:[Unix.O_RDONLY]
    ~f:(fun fd ->
      let str = Bigstring.map_file ~shared:false fd size in
      let pos_ref = ref allow_for_header in
      let rec loop acc =
        if !pos_ref = size
        then acc
        else begin
          let t = bin_read_t str ~pos_ref in
          let acc = f acc t in
          loop acc
        end in
      loop init)

let init ~msg =
  let header = Lazy.force lazy_header in
  let write_header () =
    let writer = bin_writer_header in
    ignore (writer.Bin_prot.Type_class.write report.data ~pos:0 header)
  in
  header.msg <- msg;
  write_header ();
  at_exit (write_report_to_file)
