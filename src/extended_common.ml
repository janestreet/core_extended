open Core

let run_main f =
  try
    f ();
    exit 0
  with e ->
    eprintf !"Uncaught exception:\n%{Extended_exn}\n" e;
    if Caml.Printexc.backtrace_status () then begin
      Caml.Printexc.print_backtrace stderr;
    end;
    exit 1

(* Taken from diffprint; cleanup; move to using mmap and put in sys *)
let file_content_ne f1 f2 =
  match (Extended_sys.file_kind f1),(Extended_sys.file_kind f2) with
  | Unix.S_LNK,Unix.S_LNK -> Unix.readlink f1 <> Unix.readlink f2
  | Unix.S_REG,Unix.S_REG
      when (Unix.stat f1).Unix.st_size = (Unix.stat f2).Unix.st_size
        ->
      let input_nbchar ic =
        let rec loop () =
          match In_channel.input_char ic with
          | Some (' ' | '\t' | '\n') -> loop ()
          | Some c -> Some c
          | None -> None
        in
        loop ()
      in
      protectx (In_channel.create f1, In_channel.create f2)
        ~finally:(fun (ic1,ic2) -> In_channel.close ic1; In_channel.close ic2)
        ~f:(fun (ic1,ic2) ->
          let rec loop () =
            match input_nbchar ic1,input_nbchar ic2 with
            | None,None -> false
            | x,x' when x=x' -> loop ()
            | _ -> true
          in
          loop ()
        )
  | _ -> true

let write_wrap ?(mode:[`Clobber|`Append|`Atomic|`Atomic_update]=`Clobber) ~f fname =
  match mode with
  | (`Atomic | `Atomic_update) as mode ->
      let dirname,basename = Filename.split fname in
      let tmp_file,oc = Filename.open_temp_file ~perm:0o666
        ~in_dir:dirname basename ".tmp"
      in
      protectx tmp_file
        ~f:(fun tmp_file ->
              let res = protectx oc
                ~f
                ~finally:Out_channel.close
              in
              let diff f1 f2  =
                try
                  file_content_ne f1 f2
                with _ -> true
              in
              begin match mode with
              | `Atomic_update when not (diff tmp_file fname) -> ()
              | `Atomic | `Atomic_update ->
                  Unix.link ~force:true ~target:tmp_file ~link_name:fname ()
              end;
              res)
        ~finally:Unix.unlink
  | `Clobber ->
      protectx (Out_channel.create fname) ~f ~finally:Out_channel.close
  | `Append ->
      protectx (Out_channel.create ~append:true ~perm:0o666 fname)
        ~f ~finally:Out_channel.close
