open Core;;

let groups = Memo.unit
    (fun () ->
       Unix.getgroups ()
       |> Array.to_list
       |> List.filter_map ~f:(fun gid ->
           Option.map (Unix.Group.getbygid gid)
             ~f:(fun g -> g.Unix.Group.name))
       |> (function
          | []     -> failwith "Expected at least one group."
          | groups -> groups))

let hostname = Unix.gethostname

let file_kind f = (Unix.lstat f).Unix.st_kind

let ls dir = Sys.readdir dir
  |> Array.to_list
  |> List.sort ~compare:Extended_string.collate

let stat_time_exn f ?(follow_symlinks=true) path =
  let stat =
    (if follow_symlinks
    then Unix.stat
    else Unix.lstat) path
  in
  Time.of_span_since_epoch (Time.Span.of_sec (f stat))
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
  let stdin_fd = Unix.descr_of_in_channel In_channel.stdin in
  let open Unix.Terminal_io in
  tcsetattr
    { (tcgetattr stdin_fd) with c_ixon = ixon }
    stdin_fd
    ~mode:TCSANOW
;;
