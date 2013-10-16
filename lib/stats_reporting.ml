open Core.Std
module Cycles = Time_stamp_counter
module Snapshot = Time_stamp_counter.Calibrator

module Basic_types = struct
  type field = int with sexp, bin_io
  type type_desc = string with sexp, bin_io
  type name = string with sexp, bin_io

  type t =
  | New_field of field * string * type_desc * name
  | Int_datum of field * int * Cycles.t
  | Float_datum of field * float * Cycles.t
  with sexp, bin_io
end
include Basic_types


let debug = false

type header = {
  snapshot : Snapshot.t;
  mutable msg : string;
} with bin_io

type report = {
  mutable data : Bigstring.t;
  mutable used : int;
  mutable previous : (Bigstring.t * int) list;
  mutable total_previous : int;
  mutable memory_limit : int;
  mutable file_name: string;
}

let initial_report_buffer_size = 10_000_000

(* globals *)
let allow_for_header = 256
let field_counter_ref = ref 1


(* lazy globals *)
let lazy_header = lazy {
  snapshot = Snapshot.create ();
  msg = "";
}

let report = lazy {
  data = Bigstring.create initial_report_buffer_size;
  used = allow_for_header;
  previous = [];
  total_previous = 0;
  file_name = "";
  memory_limit = 0;
}

let determine_file =
  let first_time = ref true in
  (fun report ->
    let fn = report.file_name in
    let mode =
    if !first_time then begin
      first_time := false;
      let fn_old = fn ^ ".old" in
      begin match Sys.file_exists fn with
      | `Yes -> Unix.rename ~src:fn ~dst:fn_old
      | `No | `Unknown -> ()
      end;
      [Unix.O_CREAT; Unix.O_WRONLY]
    end
    else [Unix.O_APPEND; Unix.O_WRONLY] in
    fn, mode)

let write_report_to_file () =
  let report = Lazy.force report in
  let fn, mode = determine_file report in
  let ls = (report.data, report.used) :: report.previous in
  let ls = List.rev ls in
  ignore (Unix.with_file fn
            ~mode
            ~f:(fun fd ->
              List.iter ls ~f:(fun (data, len) ->
                ignore (Bigstring.write fd ~len data))))

let expand_report_buffer () =
  let report = Lazy.force report in
  let current_size = Bigstring.length report.data in
  let new_total = report.total_previous + current_size in
  if report.memory_limit > 0 &&  new_total >= report.memory_limit then begin
    write_report_to_file ();
    report.previous <- [];
    report.total_previous <- 0;
    report.used <- 0; (* re-use the data buffer *)
  end
  else begin
    let next_alloc = Int.min initial_report_buffer_size report.memory_limit in
    report.previous <- (report.data, report.used) :: report.previous;
    report.total_previous <- new_total;
    report.data <- Bigstring.create next_alloc;
    report.used <- 0
  end

let write_to_report_buffer t =
  let writer = bin_writer_t      in
  let len    = bin_size_t t      in
  let report = Lazy.force report in
  if len + report.used > (Bigstring.length report.data)
  then expand_report_buffer ();
  report.used <- writer.Bin_prot.Type_class.write report.data ~pos:(report.used) t

let create_field  ~desc ~type_desc ~name =
  let field = !field_counter_ref in
  field_counter_ref := !field_counter_ref + 1;
  write_to_report_buffer (New_field (field, desc, type_desc, name));
  field


let add_datum field num =
  write_to_report_buffer (Int_datum (field, num, Cycles.now ()))

let add_datum_float field num =
  write_to_report_buffer (Float_datum (field, num, Cycles.now ()))

module Delta = struct
  type t = {
    field : field;
    mutable first_int : int;
    mutable first_float : float;
  }

  let create field = {
    field; first_int = 0; first_float = 0.0;
  }

  let set t n = t.first_int <- n
  let add_delta t n = add_datum t.field (n - t.first_int)
  let add_delta_and_set t n = add_delta t n; set t n

  let set_float t n = t.first_float <- n
  let add_delta_float t n = add_datum_float t.field (n -. t.first_float)
  let add_delta_and_set_float t n = add_delta_float t n; set_float t n
end


let adjust_if_required () =
  let report = Lazy.force report in
  let perc_used () =
    (report.used * 100) / (Bigstring.length report.data)
  in
  if perc_used () > 95
  then begin
    if debug then printf "Resizing stats_reporting buffer\n%!";
    expand_report_buffer ()
  end



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

let init ~msg ?(memory_limit=0) ?(file_name="stats.data") () =
  let header = Lazy.force lazy_header in
  let report = Lazy.force report in
  let write_header () =
    let writer = bin_writer_header in
    ignore (writer.Bin_prot.Type_class.write report.data ~pos:0 header)
  in
  report.file_name <- file_name;
  report.memory_limit <- memory_limit;
  header.msg <- msg;
  write_header ();
  at_exit (write_report_to_file)


module Stored_data = struct
  include Basic_types

  let read_msg_and_snapshot = read_msg_and_snapshot
  let fold = fold
end
