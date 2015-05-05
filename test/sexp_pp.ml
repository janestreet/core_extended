open Core.Std
open Core_extended.Std
let engine : [`Alter | `New | `Old] ref = ref `New

let alter sexp =
  Format.pp_set_margin Format.std_formatter 80;
  Sexp.pp_hum'  Format.std_formatter sexp;
  Format.pp_print_newline Format.std_formatter ()

let pp sexp =
  match !engine with
  | `New -> Pp.to_file stdout (Sexp.format sexp)
  | `Old -> Sexp.output_hum stdout sexp
  | `Alter -> alter sexp

let spec =
  [ "-old",Arg.Unit (fun () -> engine := `Old)," Pretty print with sexp's code";
    "-alter",Arg.Unit (fun () -> engine := `Alter)," Pretty print with sexp's code"
  ]

let usage = sprintf "%s [flags] [file]..."
  (Filename.basename Sys.executable_name)

let main () =
  let is_piped = not (Unix.isatty Unix.stdin) in
  let args = ref [] in
  Arg.parse spec (fun s -> args:= s:: !args) usage ;
  match List.rev !args with
  | [] ->
      if is_piped then begin
        List.iter ~f:pp (Sexp.input_sexps stdin)
      end else begin
        Arg.usage spec usage;
        exit 1
      end
  | l ->
      List.concat_map ~f:Sexp.load_sexps l
      |! List.iter ~f:pp


let () = Exn.handle_uncaught ~exit:true main
