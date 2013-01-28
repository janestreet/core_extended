open Core.Std
open Core_extended.Std

(* BEGIN -- useful utilities *)

include struct
  open Async.Std
  let uses_async =
    Deprecated_fcommand.step (fun finished ->
      printf "uses_async\n%!";
      upon finished Shutdown.shutdown;
      never_returns (Scheduler.go ()))
end

let flag_prompt_if_missing name of_string ~doc =
  let open Deprecated_fcommand in
  flag ("-" ^ name) ~doc
    (Flag.map (optional of_string) ~f:(function
      | Some v -> v
      | None ->
        printf "enter %s: %!" name;
        match In_channel.input_line stdin with
        | None -> failwithf "abort: no value for %s" name ()
        | Some line -> of_string line
    ))

let fields_flag spec ~doc s field =
  let open Deprecated_fcommand in
  s ++ flag ("-" ^ Fieldslib.Field.name field) spec ~doc

(* END -- useful utilities *)

module Sing = struct
  module Note = struct
    type t = A | B | C | D | E | F | G with sexp
    let of_string x = t_of_sexp (Sexp.Atom x)
  end
  let command =
    Deprecated_fcommand.cmd ~summary:"sing a song"
      Deprecated_fcommand.(
        (* flags *)
        step (fun k slow -> k ~slow)
        ++ flag "-slow" ~doc:" sing slow"
          (Flag.map no_arg ~f:function
            | `Present -> true
            | `Absent -> false)
        ++ flag "-loudness" (optional int)
          ~doc:"N how loud to sing (number of decibels)"
        ++ flag "-date" (optional date) ~doc:"DATE the date"
        ++ flag "-note" (listed Note.of_string) ~doc:"NOTE a note"
        (* anonymous arguments *)
        ++ anon ("NAME" %: string)
      )
      (fun ~slow loudness date notes song ->
        (* ... your code here... *)
        print_endline (if slow then "slow" else "fast");
        printf "loudness = %s\n"
          (Option.value ~default:"none"
            (Option.map ~f:Int.to_string loudness));
        printf "date = %s\n"
          (Option.value ~default:"no date"
            (Option.map date ~f:Date.to_string));
        printf "song name = %s\n" song;
        List.iter notes ~f:(fun note ->
          print_endline
            (Sexp.to_string_hum (
              Sexp.List [Sexp.Atom "note"; Note.sexp_of_t note])))
      )
end

let revision_flag () =
  let open Deprecated_fcommand in
  flag "-revision" ~doc:"REV revision number" (required string)

module Hg_log = struct
  let command =
    Deprecated_fcommand.cmd ~summary:"show a point in hg history"
      Deprecated_fcommand.(
        revision_flag () ++
        flag "-print" no_arg ~doc:" display all changes (not just a summary)")
      (fun revision print ->
        (* ... your code here ... *)
        ignore (revision, print)
      )
end

module Hg_cat = struct
  let command =
    Deprecated_fcommand.cmd ~summary:"cat a file from hg history"
      Deprecated_fcommand.(revision_flag () ++ anon ("FILE" %: string))
      (fun revision file ->
        (* ... your code here ... *)
        ignore (revision, file)
      )
end

module Cat = struct
  open Async.Std
  let command =
    Deprecated_fcommand.cmd ~summary:"example async command: cat a file to stdout"
      Deprecated_fcommand.(anon ("FILE" %: string) ++ uses_async)
      (fun path ->
        Reader.with_file path ~f:(fun r ->
          Pipe.iter_without_pushback (Reader.pipe r) ~f:(fun chunk ->
            Writer.write (Lazy.force Writer.stdout) chunk))
        >>= fun _ ->
        return 0)
end

module Logger = struct
  open Async.Std
  let command = (* example daemon *)
    Daemon_command.group
      (Daemon_command.create
        ~lock_file:"/var/tmp/logger.lock"
        ~name:"loggerd"
        Deprecated_fcommand.(anon ("LOGFILE" %: string) ++ uses_async)
        (fun path ->
          Writer.with_file path ~f:(fun w ->
            every (sec 1.) (fun () ->
              Writer.write w (Int.to_string (Random.int 100));
              Writer.newline w);
            never () >>= fun () ->
            return 0)))
end

module Prompting = struct
  let command =
    Deprecated_fcommand.cmd ~summary:"command demonstrting prompt-if-missing flags"
      Deprecated_fcommand.(
        (* flags *)
        flag "-rev" (required string) ~doc:" print stuff"
        ++ flag_prompt_if_missing "id" string ~doc:" whatever"
      )
      (fun revision id ->
        (* ... your code here ... *)
        print_endline "MAIN STARTED";
        printf "revision = %s\n%!" revision;
        printf "id = %s\n%!" id
      )
end

module Fields = struct

  type t = {
    foo : int;
    bar : string option;
    baz : float list;
  } with fields, sexp

  let main t =
    (* ... your code here ... *)
    print_endline (Sexp.to_string_hum (sexp_of_t t))

  let command =
    Deprecated_fcommand.cmd ~summary:"example using fieldslib"
      Deprecated_fcommand.(
        Fields.fold
          ~init:(step Fn.id)
          ~foo:(fields_flag (required int)    ~doc:"N foo factor")
          ~bar:(fields_flag (optional string) ~doc:"B error bar (optional)")
          ~baz:(fields_flag (listed float)    ~doc:"X whatever (listed)"))
      (fun foo bar baz ->
        main {foo; bar; baz})

end

module Complex_anons = struct
  let command =
    Deprecated_fcommand.cmd ~summary:"command with complex anonymous argument structure"
      Deprecated_fcommand.(
        anon ("A" %: string)
        ++ anon ("B" %: string)
        ++ anon (maybe (t3
            ("C" %: string)
            ("D" %: string)
            (maybe (t3
              ("E" %: string)
              ("F" %: string)
              (many "G" string)))))
      )
      (fun a b rest -> 
        (* ... your code here... *)
        printf "A = %s\n" a;
        printf "B = %s\n" b;
        Option.iter rest ~f:(fun (c, d, rest) ->
          printf "C = %s\n" c;
          printf "D = %s\n" d;
          Option.iter rest ~f:(fun (e, f, gs) ->
            printf "E = %s\n" e;
            printf "F = %s\n" f;
            List.iter gs ~f:(fun g ->
              printf "G = %s\n" g;
            )
          )
        )
      )
end

let command =
  Core_extended.Std.Deprecated_command.group ~summary:"fcommand examples"
  [
    ("sing", Sing.command);
    ("hg",
      Deprecated_command.group ~summary:"commands sharing a flag specification" [
        ("log", Hg_log.command);
        ("cat", Hg_cat.command);
      ]);
    ("cat", Cat.command);
    ("logger", Logger.command);
    ("prompting", Prompting.command);
    ("fields", Fields.command);
    ("complex-anons", Complex_anons.command);
  ]

let () = Deprecated_command.run command

