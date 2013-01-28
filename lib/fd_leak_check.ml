open Core.Std


(**
  Originally in unix as Unix.get_num_open_fds. There's no good system
   independent way to gather this information. *BSD systems should look
   in /dev/fd instead of /proc/self/fd. /dev/fd is part of SVR4 but not
   Posix...

  For a more stable implementation look at:

  https://src.chromium.org/viewvc/chrome/trunk/src/base/process_util_posix.cc?view=markup
*)
(* List of directories sorted by priority to search fd files in.*)
let fd_possible_dirs = ["/proc/self/fd";"/dev/fd"]

let fd_dir =
  Memo.unit (fun () ->
    let res =
      List.find ~f:(fun s -> Sys.is_directory s = `Yes)
        fd_possible_dirs
    in
    match res with
    | None -> failwithf "couldn't find any of %s; \
 are you sure you are running on linux or a BSD based system"
      (String.concat ~sep:"," fd_possible_dirs)
      ()
    | Some s -> s)

let rlimit_nofile () =
  match (Unix.RLimit.get `Num_file_descriptors).Unix.RLimit.cur with
  | Unix.RLimit.Infinity -> Int.max_value
  | Unix.RLimit.Limit v  ->
    Option.value (Int64.to_int v) ~default:Int.max_value

let get_num_open_fds () =
  let fd_dir = fd_dir () in
  let cnt = ref 0 in
  try
    protectx (Unix.opendir ~restart:true fd_dir)
      ~f:(fun fd ->
        while true; do
          match Unix.readdir fd with
          | "." | ".." -> ()
          | _ -> incr cnt
        done;
        assert false)
      ~finally:Unix.closedir;
  with End_of_file -> !cnt
  | Unix.Unix_error(Unix.EMFILE,_,_) -> rlimit_nofile ()

let report_open_files_num num_open_fds =
  eprintf "Running emergency file descriptor dump:\n%!";
  let fd_dir = fd_dir () in
  for fd = 0 to num_open_fds do
    try
      let target = Unix.readlink (fd_dir ^/ string_of_int fd) in
      eprintf "fd %d -> %s\n%!" fd target
    with _ -> ()
  done;
  if Sys.file_exists_exn "/usr/sbin/lsof" then begin
    eprintf "Also running lsof file descriptor dump:\n%!";
    match Unix.fork () with
    | `In_the_child -> Signal.send_i Signal.stop (`Pid (Unix.getpid ()))
    | `In_the_parent pid ->
      begin
        for fd = 3 to num_open_fds - 3 do
          try Unix.close (Obj.magic fd : Unix.File_descr.t)
          with _ -> ()
        done;
        Unix.sleep 1;
        ignore (Sys.command (Printf.sprintf "/usr/sbin/lsof -p %s 1>&2"
                               (Pid.to_string pid)));
        Signal.send_i Signal.cont (`Pid pid);
        Unix.exit_immediately 0
      end
  end

let report_open_files () =
  report_open_files_num (get_num_open_fds ())

let report_on_exn exn =
  let module U = Unix in
  match exn with
  | U.Unix_error ((U.EMFILE | U.ENFILE), _, _) -> report_open_files ()
  | _ -> ()

let run_check_at_exit = ref false
let critical = ref 0.9

let max_fds () =
  let module R = Unix.RLimit in
  match (R.get `Num_file_descriptors).R.cur with
  | R.Infinity -> max_int
  | R.Limit n ->
    match Int.of_int64 n with
    | Some n -> n
    | None -> max_int
;;

let check_fd_leak () =
  if !run_check_at_exit then
    try
      let max_fds = max_fds () in
      let thresh = Float.iround_towards_zero_exn (!critical *. float max_fds) in
      let num_open_fds = get_num_open_fds () in
      if num_open_fds > thresh then begin
        eprintf
          "at_exit: too many open files: have %d, critical %d, maximum %d\n%!"
          num_open_fds thresh max_fds;
        report_open_files_num num_open_fds
      end
    with exn ->
      eprintf "exception checking for file descriptor leak: %s\n%!"
        (Exn.to_string exn)

let () = at_exit check_fd_leak

let percent_fds_in_use () =
  float (get_num_open_fds ()) /. float (max_fds ())
;;
