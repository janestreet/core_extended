open Core

let get_gen env_vars defaults =
  let env_var_programs = List.filter_map env_vars ~f:Sys.getenv in
  let programs = env_var_programs @ defaults in
  let rec first_valid = function
    | [] -> None
    | p::ps ->
      (* ignore options given in env vars (e.g., emacsclient -c). String.split always
         returns list of at least 1 element. *)
      let p_no_opts = String.split ~on:' ' p |> List.hd_exn in
      match Shell.which p_no_opts with
      | Some _ -> Some p
      | None -> first_valid ps
  in first_valid programs
;;

let get_editor () = get_gen ["EDITOR"; "VISUAL"] ["vim"; "emacs"; "nano"] ;;

let get_editor_exn () =
  get_editor ()
  |> Option.value_exn
    ~message:"No valid editors found! Try setting EDITOR environment variable."
;;

let get_pager () = get_gen ["PAGER"] ["less"; "more"] ;;

let page_contents ?pager ?(pager_options=[])?(tmp_name="sys_utils.page_contents") contents =
  let tmp_file = Filename.temp_file tmp_name ".txt" in
  let pager = match pager with
    | Some p -> p
    | None -> get_pager () |> Option.value_exn
      ~message:"Couldn't find pager - very weird. Try setting PAGER variable?"
  in
  Exn.protect ~f:(fun () ->
    Out_channel.with_file tmp_file ~f:(fun f -> Out_channel.output_string f contents);
    (* Shell.run doesn't work here *)
    let cmd = sprintf "%s %s %s" pager (String.concat ~sep:" " pager_options) tmp_file in
    ignore (Unix.system cmd))
    ~finally:(fun () -> Shell.rm tmp_file)

let pid_alive pid = Sys.is_directory_exn ("/proc" ^/ Pid.to_string pid)

let get_groups user =
  match Shell.run_lines "/usr/bin/groups" [user] with
  | [line] ->
    begin match String.chop_prefix line ~prefix:(user^" : ") with
    | Some groups -> String.split ~on:' ' groups
    | None -> failwithf "get_groups couldn't parse \"groups\" output:\n%s" line ()
    end
  | lines -> failwithf "get_groups expected exactly 1 line from \"groups\". Got:\n%s"
    (String.concat ~sep:"\n" lines) ()

let with_tmp ~pre ~suf f =
  let tmp_file = Filename.temp_file pre suf in
  Exn.protect ~f:(fun () -> f tmp_file)
    ~finally:(fun () -> Sys.remove tmp_file)

(* -d is supposed to make it find a smaller set of changes. *)
let diff ?(options=["-d";"-u"]) s1 s2 =
  with_tmp ~pre:"sysutils" ~suf:"diff1" (fun f1 ->
    with_tmp ~pre:"sysutils" ~suf:"diff2" (fun f2 ->
      Out_channel.write_all f1 ~data:s1;
      Out_channel.write_all f2 ~data:s2;
      Shell.run_full ~expect:[0;1] "/usr/bin/diff" (options @ ["--"; f1; f2])))

let ip_of_name name =
  Unix.Inet_addr.of_string_or_getbyname name |> Unix.Inet_addr.to_string

let getbyname_ip () = ip_of_name (Unix.gethostname ())

let ifconfig_ip_rex =
  Re2.create_exn
    "inet addr:([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})"

let ifconfig_ips () =
  Shell.run_lines "/sbin/ifconfig" []
  |> List.filter_map ~f:(fun line ->
    match Re2.find_submatches ifconfig_ip_rex line with
    | Ok [|_; Some ip|] -> Some ip
    | Ok _ | Error _ -> None)
  |> String.Set.of_list

let digest file =
  if Sys.file_exists file <> `No then
    Some (Md5.digest_file_blocking file)
  else
    None

(** Safe editions *)
let edit_one file =
  (** Get the digest of the file we are editing. This is used to test for
      modifications on the file.*)
  let md5 = digest file in
  let editor =
      match get_editor () with
      | Some v -> v
      | None   -> failwithf "Could find an not find a suitable editor" ()
  in
  let pid =
    Extended_unix.fork_exec
      editor
      [file]
  in
  let ret = Unix.waitpid pid in
  match ret with
    (** We have to discard the return value because different editors have
        different conventions... *)
  | Ok () | Error #Unix.Exit.error ->
      let new_digest = Some (Md5.digest_file_blocking file) in
      if new_digest = md5 then
        `Unchanged
      else
        `Ok
  | Error (`Signal _ as v) -> v

let rec edit_until check file =
  let relaunch () =
    match Readline.choice ["retry",`Retry;"abort",`Abort] with
    | Some `Retry -> edit_until check file
    | Some `Abort | None -> `Abort
  in
  match edit_one file with
  | `Ok          ->
      begin
        match check file with
        | None -> `Ok
        | Some v ->
            printf "There were errors while validating the file:\n%s\n" v;
            relaunch ()
      end
  | `Unchanged   -> `Unchanged
  | #Unix.Exit_or_signal_or_stop.error as e ->
      printf "The editor died unexpectedly (%s)\n"
        (Unix.Exit_or_signal_or_stop.to_string_hum (Error e));
      relaunch ()

let checked_edit ?(create=false) ~check file =
  (* Blow up early if the file is not accessible *)
  let dirname,basename = Filename.split file in
  let exists = Sys.file_exists_exn file in
  if create && not exists then
    Unix.access_exn dirname [ `Read; `Write ]
  else
    (* This will blow up if the file does not exist*)
    Unix.access_exn file [ `Read; `Write ];
  let main,ext = match String.rsplit2 ~on:'.' basename with
    | Some (main,ext) -> (main,"."^ext)
    | None -> basename,""
  in
  let original_digest = digest file in
  let tmp_file = Filename.temp_file main ext in
  let edit_status =
    try
      if exists then
        Shell.cp file tmp_file;
      edit_until check tmp_file
    with e ->
      Shell.rm tmp_file;
      raise e
  in
  match edit_status with
    | `Abort     ->
      Shell.rm tmp_file;
      `Abort
    | `Ok        ->
      if digest file <> original_digest then
        failwithf "The underlying file changed while we were editing it.\
 your version is saved as: %S" tmp_file ();
      Shell.mv tmp_file file;
      `Ok
    | `Unchanged ->
      Shell.rm tmp_file;
      `Ok



module Sexp_checked_edit (S:Sexpable) = struct
  let check file =
    try ignore (Sexp.load_sexp_conv_exn file S.t_of_sexp : S.t);
        None
    with exc -> Some (Extended_exn.to_string_hum exc)

  let check_sexps file =
    try ignore (Sexp.load_sexps_conv_exn file S.t_of_sexp : S.t list);
        None
    with exc -> Some (Extended_exn.to_string_hum exc)

  let edit       = checked_edit ~check
  let edit_sexps = checked_edit ~check:check_sexps
end



module Cpu_use = struct
  type cpu_sample = {
    jiffies : Big_int.big_int;
    time : Time.t;
  }

  type t = {
    pid : Pid.t;
    jps : float;
    (* needed because of time-skew issues (see comment in sys_utils.mli) *)
    initial_age : Time.Span.t;
    mutable age : Time.Span.t;
    mutable fds : int;
    mutable rss : Big_int.big_int;
    mutable cpu0 : cpu_sample;
    mutable cpu1 : cpu_sample;
  } [@@deriving fields]

  module P = Procfs.Process

  let sample_of_stat {P.Stat.utime; stime; _ } =
    { jiffies = Big_int.add_big_int utime stime;
      time = Time.now () }

  let fds_of_proc proc = proc.P.fds |> Option.value_map ~f:List.length ~default:0

  let get ?(pid=Unix.getpid ()) () =
    let proc0 = Procfs.with_pid_exn pid in
    let proc1 = Procfs.with_pid_exn pid in
    let jiffies_per_second = Procfs.jiffies_per_second_exn () in
    { pid;
      jps = jiffies_per_second;
      initial_age = Procfs.process_age' ~jiffies_per_second proc1;
      age = Time.Span.zero;
      fds = fds_of_proc proc1;
      rss = proc1.P.stat.P.Stat.rss;
      cpu0 = sample_of_stat proc0.P.stat;
      cpu1 = sample_of_stat proc1.P.stat;
    }

  let update_exn t =
    let proc = Procfs.with_pid_exn t.pid in
    let age =
      Time.Span.(-) (Procfs.process_age' ~jiffies_per_second:t.jps proc) t.initial_age
    in
    t.age <- age;
    t.fds <- fds_of_proc proc;
    t.rss <- proc.P.stat.P.Stat.rss;
    t.cpu0 <- t.cpu1;
    t.cpu1 <- sample_of_stat proc.P.stat

  let cpu_use {jps; cpu0={jiffies=j0;time=t0}; cpu1={jiffies=j1;time=t1}; _} =
    let proc_jps =
      Big_int.float_of_big_int (Big_int.sub_big_int j1 j0)
      /. Time.Span.to_sec (Time.diff t1 t0)
    in
    proc_jps /. jps

  (* rss is in pages. /should/ call getpagesize... but it's 4k. *)
  let resident_mem_use_in_kb t =
    Big_int.float_of_big_int t.rss *. 4.
end

module Lsb_release = struct
  type t =
    {
      distributor_id : string; (* e.g. "Red Hat", "CentOS" *)
      release        : string; (* e.g. "5.7", "6.3" on CentOs, 'testing' on debian*)
      codename       : string; (* e.g. "Final", "Lucid", etc. *)
    }
  [@@deriving sexp, fields, bin_io, compare]

  let query () =
    let q flag =
      match Shell.run_one_line "lsb_release" ["-s"; flag] with
      | Error error ->
        failwithf !"Lsb_release.query(%s): failed Shell.run_one_line: %{sexp:Error.t}"
          flag error ()
      | Ok value -> value
    in
    {
      distributor_id = q "-i";
      release        = q "-r";
      codename       = q "-c"
    }
end
