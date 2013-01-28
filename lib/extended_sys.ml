open Core.Std;;

external running_byte_code :
  unit -> unit -> unit -> unit -> unit -> unit -> bool
  = "caml_running_byte_code_bc" "caml_running_byte_code_nc" "noalloc"

let running_byte_code () = running_byte_code () () () () () ()

let home () =
  (Unix.Passwd.getbyuid_exn (Unix.geteuid ())).Unix.Passwd.dir

let groups = Memo.unit
  (fun () ->
     Unix.getgroups () |! Array.to_list |!
         List.map ~f:(fun gid ->
                        (Unix.Group.getbygid_exn gid).Unix.Group.name))

let hostname = Unix.gethostname

let file_kind f = (Unix.lstat f).Unix.st_kind

let ls dir = Sys.readdir dir
  |! Array.to_list
  |! List.sort ~cmp:Extended_string.collate

let stat_time_exn f ?(follow_symlinks=true) path =
  let stat =
    (if follow_symlinks
    then Unix.stat
    else Unix.lstat) path
  in
  Time.of_float (f stat)
;;

let stat_time f ?follow_symlinks path =
  Option.try_with (fun () -> stat_time_exn f ?follow_symlinks path)
;;

let last_accessed,
    last_accessed_exn =
  let f = (fun stat -> stat.Unix.st_atime) in
  stat_time f,
  stat_time_exn f
;;

let last_modified,
    last_modified_exn =
  let f = (fun stat -> stat.Unix.st_mtime) in
  stat_time f,
  stat_time_exn f
;;

let last_changed,
    last_changed_exn =
  let f = (fun stat -> stat.Unix.st_ctime) in
  stat_time f,
  stat_time_exn f
;;

let file_size_exn ?(follow_symlinks=true) path =
  let stat =
    (if follow_symlinks
    then Unix.stat
    else Unix.lstat) path
  in
  stat.Unix.st_size
;;

let file_size ?follow_symlinks path =
  Option.try_with (fun () -> file_size_exn ?follow_symlinks path)
;;

let scroll_lock ixon =
  let stdin_fd = Unix.descr_of_in_channel stdin in
  let open Unix.Terminal_io in
  tcsetattr
    { (tcgetattr stdin_fd) with c_ixon = ixon }
    stdin_fd
    ~mode:TCSANOW
;;
