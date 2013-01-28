(* OCaml-version of ktee.c in same directory *)

(* Example usage: cat /etc/termcap | ktee.exe foo1.log | cat > foo2.log *)

open Core.Std
open Core_extended.Std
open Unix
open Linux_ext

let main () =
  let ofd = openfile Sys.argv.(1) ~mode:[O_WRONLY; O_CREAT; O_TRUNC] ~perm:0o644 in
  let tee_flags = Splice.make_flags [| |] in
  let splice_flags = Splice.make_flags [| Splice.MOVE |] in
  let rec loop () =
    match
      try
        Some (
          let tee = Or_error.ok_exn Splice.tee in
          tee ~assume_fd_is_nonblocking:true
            ~fd_in:stdin ~fd_out:stdout Int.max_value tee_flags)
      with Unix_error (EAGAIN, _, _) -> None
    with
    | None -> loop ()
    | Some len when len = 0 -> ()
    | Some len ->
        let rec splice_loop len =
          if len > 0 then
            let slen, _, _ =
              let splice = Or_error.ok_exn Splice.splice in
              splice ~fd_in:stdin ~fd_out:ofd ~len splice_flags
            in
            splice_loop (len - slen)
          else loop ()
        in
        splice_loop len
  in
  loop ()

let () =
  try main ()
  with exc -> printf "%s\n%!" (Exn.to_string exc)
