open Core.Std

let get_gen env_vars defaults =
  let env_var_programs = List.filter_map env_vars ~f:Sys.getenv in
  let programs = env_var_programs @ defaults in
  let rec first_valid = function
    | [] -> None
    | p::ps ->
      (* ignore options given in env vars (e.g., emacsclient -c). String.split always
         returns list of at least 1 element. *)
      let p_no_opts = String.split ~on:' ' p |! List.hd_exn in
      match Shell.which p_no_opts with
      | Some _ -> Some p
      | None -> first_valid ps
  in first_valid programs
;;

let get_editor () = get_gen ["EDITOR"; "VISUAL"] ["vim"; "emacs"; "nano"] ;;

let get_editor_exn () =
  get_editor ()
  |! Option.value_exn_message
    "No valid editors found! Try setting EDITOR environment variable."
;;

let get_pager () = get_gen ["PAGER"] ["less"; "more"] ;;

let page_contents ?pager ?(pager_options=[])?(tmp_name="sys_utils.page_contents") contents =
  let tmp_file = Filename.temp_file tmp_name ".txt" in
  let pager = match pager with
    | Some p -> p
    | None -> get_pager () |! Option.value_exn_message
        "Couldn't find pager - very weird. Try setting PAGER variable?"
  in
  Exn.protect ~f:(fun () ->
    Out_channel.with_file tmp_file ~f:(fun f -> Out_channel.output_string f contents);
    (* Shell.run doesn't work here *)
    let cmd = sprintf "%s %s %s" pager (String.concat ~sep:" " pager_options) tmp_file in
    ignore (Unix.system cmd))
    ~finally:(fun () -> Shell.rm tmp_file)

let pid_alive pid = Shell.test "kill" ["-0"; Int.to_string pid]

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
let diff ?(options=["-d"]) s1 s2 =
  with_tmp ~pre:"sysutils" ~suf:"diff1" (fun f1 ->
    with_tmp ~pre:"sysutils" ~suf:"diff2" (fun f2 ->
      Out_channel.write_all f1 ~data:s1;
      Out_channel.write_all f2 ~data:s2;
      Shell.run_full ~expect:[0;1] "/usr/bin/diff" (options @ ["--"; f1; f2])))

let getbyname_ip () =
  Unix.gethostname ()
  |! Unix.Inet_addr.of_string_or_getbyname
  |! Unix.Inet_addr.to_string

let ifconfig_ip_rex = Pcre.regexp "inet addr:([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})"
let ifconfig_ips () =
  Shell.run_lines "/sbin/ifconfig" []
  |! List.filter_map ~f:(fun line ->
    Option.try_with (fun () ->
      match Pcre.extract line ~rex:ifconfig_ip_rex ~full_match:false with
      | [|ip|] -> ip
      | _ -> assert false))
  |! String.Set.of_list

let digest file =
  if Sys.file_exists file <> `No then
    Digest.file file
  else
    ""

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
  let _,ret = Unix.wait (`Pid pid) in
  match ret with
    (** We have to discard the return value because different editors have
        different conventions... *)
  | Ok () | Error #Unix.Exit.error ->
      let new_digest = Digest.file file in
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
  type sample = {
    jiffies : Big_int.big_int;
    time : Time.t;
  }

  type t = {
    pid : Pid.t;
    jps : float;
    mutable s0 : sample;
    mutable s1 : sample;
  }

  let sample_exn pid =
    let module P = Procfs.Process in
    let {P.Stat.utime; stime; _} = (Procfs.with_pid_exn pid).P.stat in
    { jiffies = Big_int.add_big_int utime stime;
      time = Time.now () }

  let get ?(pid=Unix.getpid ()) () =
    { pid;
      jps = Procfs.jiffies_per_second_exn ();
      s0 = sample_exn pid;
      s1 = sample_exn pid; }

  let update_exn t =
    t.s0 <- t.s1;
    t.s1 <- sample_exn t.pid

  let cpu_use {jps; s0={jiffies=j0;time=t0}; s1={jiffies=j1;time=t1}; _} =
    let my_jps =
      Big_int.float_of_big_int (Big_int.sub_big_int j1 j0)
      /. Time.Span.to_sec (Time.diff t1 t0)
    in
    my_jps /. jps
end
