open Core.Std

type field = int with sexp, bin_io
type type_desc = string with sexp, bin_io
type name = string with sexp, bin_io

type t =
| New_field of field * string * type_desc * name
| Int_datum of field * int * Time.t
| Float_datum of field * float * Time.t
with sexp, bin_io

type report = {
  mutable data : Bigstring.t;
  mutable used : int;
}

let header_size = 16


let report = { data = Bigstring.create 10000000;
               used = header_size; }

let field_counter_ref = ref 1

let write_header () =
  Int.bin_write_t report.data ~pos:0 report.used


let expand_report_buffer () =
  let buf = Bigstring.create ((Bigstring.length report.data) * 2) in
  begin
    Bigstring.blit ~src:report.data ~src_len:report.used ~dst:buf ();
    report.data <- buf;
  end

let report_t t =
  let writer = bin_writer_t in
  let len  = bin_size_t t in
  if len + report.used > (Bigstring.length report.data)
  then expand_report_buffer ()
  else report.used <- writer.Bin_prot.Type_class.write report.data ~pos:(report.used) t

(* let get_handle label = if (Obj.is_int (Obj.repr label))
 *   then (Obj.magic label)
 *   else failwithf "Non-int label supplied to log library" () *)

let create_field  ~desc ~type_desc ~name =
  let field = !field_counter_ref in
  field_counter_ref := !field_counter_ref + 1;
  report_t (New_field (field, desc, type_desc, name));
  field

let add_datum field num =
  report_t (Int_datum (field, num, Time.now()))

let add_datum_float field num =
  report_t (Float_datum (field, num, Time.now()))

let write_report () =
  let fn = "stats.data" in
  let fn_old = "stats.data.old" in
  begin match Sys.file_exists fn with
  | `Yes -> Unix.rename ~src:fn ~dst:fn_old
  | `No | `Unknown -> ()
  end;
  ignore (write_header ());
  ignore (Unix.with_file fn
            ~mode:[Unix.O_CREAT; Unix.O_WRONLY]
            ~f:(fun fd ->
              Bigstring.write fd ~len:report.used report.data))

let init () = begin
  at_exit (write_report);
end

let fold ~file ~init ~f =
  let size = Int64.to_int_exn (Extended_sys.file_size_exn file) in
  Unix.with_file file
    ~mode:[Unix.O_RDONLY]
    ~f:(fun fd ->
      let str = Bigstring.map_file ~shared:false fd size in
      let pos_ref = ref 0 in
      let used = Int.bin_read_t str ~pos_ref in
      pos_ref := header_size;
      let rec loop acc =
        if !pos_ref = used
        then acc
        else begin
          let t = bin_read_t str ~pos_ref in
          let acc = f acc t in
          loop acc
        end in
      loop init)





