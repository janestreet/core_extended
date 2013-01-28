open Core.Std

(* Learn more about this business by consulting proc(5) *)

(* lu and ld match the proc(5) format strings %lu and %ld *)
let lu x = Big_int.big_int_of_string x
let ld x = lu x   (* bigint everything so we don't have to worry about overflows *)
let string_of_file fn = In_channel.with_file ~f:In_channel.input_all fn ;;

type bigint = Big_int.big_int with sexp ;;

module Process = struct

  module Inode = struct
    type t = Int64.t with sexp ;;
    let of_string = Int64.of_string ;;
    let to_string = Int64.to_string ;;
  end ;;

  module Limits = struct
    module Rlimit = struct
      type value = [ `unlimited | `limited of bigint ] with sexp ;;
      type t = { soft : value; hard: value } with fields, sexp ;;
    end ;;
    type t =
      {
        cpu_time          : Rlimit.t;
        file_size         : Rlimit.t;
        data_size         : Rlimit.t;
        stack_size        : Rlimit.t;
        core_file_size    : Rlimit.t;
        resident_set      : Rlimit.t;
        processes         : Rlimit.t;
        open_files        : Rlimit.t;
        locked_memory     : Rlimit.t;
        address_space     : Rlimit.t;
        file_locks        : Rlimit.t;
        pending_signals   : Rlimit.t;
        msgqueue_size     : Rlimit.t;
        nice_priority     : Rlimit.t;
        realtime_priority : Rlimit.t;
     }
    with fields, sexp ;;

    let of_string s =
      let map =
        String.split s ~on:'\n'
        |! List.map ~f:String.strip
        |! List.filter ~f:(fun line -> line <> "")
        |! List.fold ~init:String.Map.empty ~f:(fun map line ->
          match
            String.strip line
            |! String.lowercase
            |! String.split ~on:' '
            |! List.filter ~f:(fun s -> s <> "")
            |! List.rev
          with
          | ("units" | "seconds" | "bytes" | "processes" | "locks" | "signals" | "files")
            :: hard_limit :: soft_limit :: name ->
              let key = List.rev name |! String.concat ~sep:" " in
              Map.add map ~key ~data:(soft_limit, hard_limit)

          (* priorities don't have an entry in the "Units" column *)
          | hard_limit :: soft_limit :: name ->
              let key = List.rev name |! String.concat ~sep:" " in
              Map.add map ~key ~data:(soft_limit, hard_limit)

          | _ -> failwithf "Procfs.Limits.of_string bad format: %s" line ()
        )
      in
      let get key =
        let data =
          match Map.find map key with
          | Some data -> data
          | None -> failwithf "Procfs.Limits.of_string: bad key %s" key ()
        in
        let value x =
          if x = "unlimited" then `unlimited
          else `limited (Big_int.big_int_of_string x)
        in
        { Rlimit.soft = fst data |! value; hard = snd data |! value }
      in
      {
        cpu_time          = get "max cpu time";
        file_size         = get "max file size";
        data_size         = get "max data size";
        stack_size        = get "max stack size";
        core_file_size    = get "max core file size";
        resident_set      = get "max resident set";
        processes         = get "max processes";
        open_files        = get "max open files";
        locked_memory     = get "max locked memory";
        address_space     = get "max address space";
        file_locks        = get "max file locks";
        pending_signals   = get "max pending signals";
        msgqueue_size     = get "max msgqueue size";
        nice_priority     = get "max nice priority";
        realtime_priority = get "max realtime priority"
    }
  end ;;
  module Stat = struct
    type t =
      {
        comm        : string; (** The filename of the executable *)
        state       : char;   (** One  character from the string "RSDZTW" *)
        ppid        : Pid.t option;    (** The PID of the parent. *)
        pgrp        : Pid.t;    (** The process group ID of the process. *)
        session     : int;    (** The session ID of the process. *)
        tty_nr      : int;    (** The tty the process uses. *)
        tpgid       : int;    (** The process group ID of the process which currently owns
                                  the tty... *)
        flags       : bigint; (** The kernel flags word of the process. *)
        minflt      : bigint; (** The number of minor faults the process has made which have
                                  not required loading a memory page from disk. *)
        cminflt     : bigint; (** The number of minor faults that the process’s waited-for
                                  children have made. *)
        majflt      : bigint; (** The number of major faults the process has made which have
                                  required loading a page from disk. *)
        cmajflt     : bigint; (** The number of major faults that the process’s waited-for
                                  children have made. *)
        utime       : bigint; (** The number of jiffies that this process has been scheduled
                                  in user mode. *)
        stime       : bigint; (** The number of jiffies that this process has been scheduled
                                  in kernel mode. *)
        cutime      : bigint; (** The number of jiffies that this process’s waited-for
                                  children have been scheduled in user mode. *)
        cstime      : bigint; (** The number of jiffies that this process’s waited-for
                                  children have been scheduled in kernel mode. *)
        priority    : bigint; (** The standard nice value, plus fifteen.  The value is never
                                  negative in the kernel. *)
        nice        : bigint; (** The nice value ranges from 19 to -19*)
        unused      : bigint; (** placeholder for removed field *)
        itrealvalue : bigint; (** The time in jiffies before the next SIGALRM is sent to the
                                  process due to an interval timer. *)
        starttime   : bigint; (** The time in jiffies the process started after system boot.*)
        vsize       : bigint; (** Virtual memory size in bytes. *)
        rss         : bigint; (** Resident Set Size: number of pages the process has in real
                                  memory. *)
        rlim        : bigint; (** Current limit in bytes on the rss of the process. *)
        startcode   : bigint; (** The address above which program text can run. *)
        endcode     : bigint; (** The address below which program text can run. *)
        startstack  : bigint; (** The address of the start of the stack. *)
        kstkesp     : bigint; (** The current value of esp (stack pointer) *)
        kstkeip     : bigint; (** The current value of eip (instruction pointer) *)
        signal      : bigint; (** The bitmap of pending signals. *)
        blocked     : bigint; (** The bitmap of blocked signals. *)
        sigignore   : bigint; (** The bitmap of ignored signals. *)
        sigcatch    : bigint; (** The bitmap of caught signals. *)
        wchan       : bigint; (** This is  the "channel" in which the process is waiting.
                                  Address of a system call. *)
        nswap       : bigint; (** (no longer maintained) *)
        cnswap      : bigint; (** (no longer maintained) *)
        exit_signal : int;    (** Signal sent to parent when we die. *)
        processor   : int;    (** CPU number last executed on. *)
        rt_priority : bigint; (** Real-time scheduling priority. *)
        policy      : bigint; (** Scheduling policy *)
      }
    with fields, sexp ;;
    let of_string s =
      let extract_command s =
      (*
        extract_cmdline, for a stat string such as:
          "14574 (cat) R 10615 14574 10615 34820 14574 4194304 164 0..."
        returns this tuple
          "cat", "R 10615 14574 10615..."
       *)
        let i = String.index_exn s '(' in
        let j = String.rindex_exn s ')' in
        (String.sub s ~pos:(i+1) ~len:(j-(i+1)),
        String.sub s ~pos:(j+1) ~len:((String.length s)-(j+1)))
      in
      let comm, rest = extract_command s in
        let a = Array.of_list (String.split (String.strip rest) ~on:' ') in
        let d x = int_of_string x in
        let c x = x.[0] in
        { comm        = comm;
          state       = c a.(0);
          ppid        = (match d a.(1) with
                         | x when x < 1 -> None
                         | x            -> Some (Pid.of_int x));
          pgrp        = Pid.of_int (d a.(2));
          session     = d a.(3);
          tty_nr      = d a.(4);
          tpgid       = d a.(5);
          flags       = ld a.(6);
          minflt      = ld a.(7);
          cminflt     = ld a.(8);
          majflt      = ld a.(9);
          cmajflt     = ld a.(10);
          utime       = ld a.(11);
          stime       = ld a.(12);
          cutime      = ld a.(13);
          cstime      = ld a.(14);
          priority    = ld a.(15);
          nice        = ld a.(16);
          unused      = ld a.(17);
          itrealvalue = ld a.(18);
          starttime   = lu a.(19);
          vsize       = lu a.(20);
          rss         = ld a.(21);
          rlim        = lu a.(22);
          startcode   = lu a.(23);
          endcode     = lu a.(24);
          startstack  = lu a.(25);
          kstkesp     = lu a.(26);
          kstkeip     = lu a.(27);
          signal      = lu a.(28);
          blocked     = lu a.(29);
          sigignore   = lu a.(30);
          sigcatch    = lu a.(31);
          wchan       = lu a.(32);
          nswap       = lu a.(33);
          cnswap      = lu a.(34);
          exit_signal = d a.(35);
          processor   = d a.(36);
          rt_priority = lu a.(37);
          policy      = lu a.(38);
        }
    ;;
  end ;;

  module Statm = struct
    type t =
      {
        size     : bigint; (** total program size *)
        resident : bigint; (** resident set size *)
        share    : bigint; (** shared pages *)
        text     : bigint; (** text (code) *)
        lib      : bigint; (** library *)
        data     : bigint; (** data/stack *)
        dt       : bigint; (** dirty pages (unused) *)
      }
    with fields, sexp ;;
    let of_string s =
      let a = Array.of_list (String.split s ~on:' ') in
      {
        size     = lu a.(0);
        resident = lu a.(1);
        share    = lu a.(2);
        text     = lu a.(3);
        lib      = lu a.(4);
        data     = lu a.(5);
        dt       = lu a.(6);
      }
    ;;
  end ;;

  module Status = struct
    type t =
      {
        uid   : int; (** Real user ID *)
        euid  : int; (** Effective user ID *)
        suid  : int; (** Saved user ID *)
        fsuid : int; (** FS user ID *)
        gid   : int; (** Real group ID *)
        egid  : int; (** Effective group ID *)
        sgid  : int; (** Saved group ID *)
        fsgid : int; (** FS group ID *)
      }
    with fields, sexp ;;
    let of_string s =
      (* Splits "foo: 1\nbar: 2\n" into [Some ("foo"," 1"); Some ("bar"," 2"); None] *)
      let records = List.map (String.split s ~on:'\n')
        ~f:(fun x -> String.lsplit2 x ~on:':')
      in
      let _, uids = Option.value_exn
        (List.find_exn records
          ~f:(fun kv -> match kv with
              | Some ("Uid",_) -> true
              | _ -> false))
      in
      let _, gids = Option.value_exn
        (List.find_exn records
          ~f:(fun kv -> match kv with
              | Some ("Gid",_) -> true
              | _ -> false))
      in
      sscanf (String.concat ~sep:" " [String.strip uids; String.strip gids])
        "%d %d %d %d %d %d %d %d"
        (fun a b c d e f g h -> { uid = a; euid = b; suid = c; fsuid = d;
                                  gid = e; egid = f; sgid = g; fsgid = h; })
    ;;
  end ;;

  module Fd = struct
    type fd_stat =
      | Path of string
      | Socket of Inode.t
      | Pipe of Inode.t
      | Inotify
    with sexp ;;
    type t =
      {
        fd      : int;     (** File descriptor (0=stdin, 1=stdout, etc.) *)
        fd_stat : fd_stat; (** Kind of file *)
      }
    with fields, sexp ;;
  end ;;

  type t =
    {
      pid         : Pid.t;            (** Process ID *)
      cmdline     : string;           (** Command-line (not reliable). *)
      cwd         : string option;    (** Symlink to working directory. *)
      environ     : string option;    (** Process environment. *)
      exe         : string option;    (** Symlink to executed command. *)
      root        : string option;    (** Per-process root (e.g. chroot) *)
      limits      : Limits.t option;  (** Per-process rlimit settings *)
      stat        : Stat.t;           (** Status information. *)
      statm       : Statm.t;          (** Memory status information. *)
      status      : Status.t;         (** Some more assorted status information. *)
      task_stats  : Stat.t Pid.Map.t; (** Status information for each task (thread) *)
      top_command : string;           (** Show what top would show for COMMAND *)
      fds         : Fd.t list option; (** File descriptors *)
      oom_adj     : int;
      oom_score   : int;
    }
  with fields, sexp ;;

  let load_exn pid =
    let slurp f fn =
      try
        Some (f (sprintf "/proc/%s/%s" (Pid.to_string pid) fn))
      with
      | Sys_error _ -> None
      | Unix.Unix_error (Unix.EACCES, _, _) -> None
      | Unix.Unix_error (Unix.ENOENT, _, _) -> None
      | Unix.Unix_error (Unix.EINVAL, _, _) -> None
    in
    let slurp_file fn = slurp string_of_file fn in
    let slurp_link fn = slurp Unix.readlink fn in
    let slurp_dir fn = slurp Sys.readdir fn in

    let required x = Option.value_exn x in
    let require_str f = slurp_file f |! required in
    let require_int f = slurp_file f |! required |! String.strip |! Int.of_string in
    let cmdline = require_str "cmdline" in
  (*
   * Process command name varies
   *
   * cmdline is ideal but not guaranteed to be there because the kernel
   *  - may discard it in lomem situations
   *  - discard it for zombie processes
   *  - put nothing useful there for kernel processes
   *
   * The exe symlink might be useful, it's the name of the executable
   * which started the process, but permission is usually denied for
   * non-root/non-self viewers.
   *
   * The stat.command field will ALWAYS be there but is truncated
   * to 16 chars; we do here what top does: use cmdline if it is
   * populated, otherwise use stat.command.
   *)
    let stat = Stat.of_string (require_str "stat") in
    let limits =
      Option.try_with (fun () -> Limits.of_string (require_str "limits"))
    in
    let top_command =
      (if cmdline = "" then Stat.comm stat
      else
        String.tr ~target:'\x00' ~replacement:' ' cmdline)
      |! String.strip
    in
    let task_stats =
      Array.fold_right (required (slurp_dir "task"))
        ~init:Pid.Map.empty
        ~f:(fun task m ->
          Pid.Map.add m
            ~key:(Pid.of_string task)
            ~data:(Stat.of_string (require_str
              (String.concat ~sep:"/" ["task"; task; "stat"])))
        )
    in
    let fds =
      try
        Some (
          Sys.readdir (sprintf "/proc/%s/fd" (Pid.to_string pid))
          |! Array.map ~f:Int.of_string
          |! Array.filter_map ~f:(fun fd ->
              slurp Unix.readlink (sprintf "fd/%d" fd)
              |! Option.map ~f:(fun path ->
                let parse inode = (* "[123]" -> 123 *)
                  inode |!
                  String.chop_prefix_exn ~prefix:"[" |!
                  String.chop_suffix_exn ~suffix:"]" |!
                  Inode.of_string
                in
                { Fd.
                  fd = fd;
                  fd_stat =
                    match String.split ~on:':' path with
                    | "socket"::inode::[] -> Fd.Socket (parse inode)
                    | "pipe"::inode::[]   -> Fd.Pipe (parse inode)
                    | "inotify"::[]       -> Fd.Inotify
                    | _                   -> Fd.Path path;
                }
              ))
          |! Array.to_list
        )
      with
        Sys_error _ -> None
    in
    {
      pid         = pid;
      cmdline     = cmdline;
      cwd         = slurp_link "cwd";
      environ     = slurp_file "environ";
      exe         = slurp_link "exe";
      root        = slurp_link "root";
      limits      = limits;
      stat        = stat;
      statm       = Statm.of_string (require_str "statm");
      status      = Status.of_string (require_str "status");
      task_stats  = task_stats;
      top_command = top_command;
      fds         = fds;
      oom_adj     = require_int "oom_adj";
      oom_score   = require_int "oom_score";
    }
  ;;


end ;;

module Meminfo = struct
  type t =
    {
      mem_total     : bigint;
      mem_free      : bigint;
      buffers       : bigint;
      cached        : bigint;
      swap_cached   : bigint;
      active        : bigint;
      inactive      : bigint;
      swap_total    : bigint;
      swap_free     : bigint;
      dirty         : bigint;
      writeback     : bigint;
      anon_pages    : bigint;
      mapped        : bigint;
      slab          : bigint;
      page_tables   : bigint;
      nfs_unstable  : bigint;
      bounce        : bigint;
      commit_limit  : bigint;
      committed_as  : bigint;
      vmalloc_total : bigint;
      vmalloc_used  : bigint;
      vmalloc_chunk : bigint;
    }
  with fields, sexp ;;
  let load_exn () =
    let of_kb = Big_int.mult_int_big_int 1024 in
    let map =
      In_channel.read_lines "/proc/meminfo"
      |! List.fold ~init:String.Map.empty ~f:(fun map line ->
        match String.strip line
              |! String.tr ~target:':' ~replacement:' '
              |! String.split ~on:' '
              |! List.filter ~f:(fun s -> s <> "")
        with
        | key :: value :: "kB" :: [] ->
            let data = Big_int.big_int_of_string value |! of_kb in
            Map.add map ~key ~data
        | _ -> map (* ignore weird lines *)
      )
    in
    let get k =
      match Map.find map k with
      | Some v -> v
      | None -> failwithf "meminfo_exn: cannot extract field %s" k ()
    in
    { mem_total     = get "MemTotal";
      mem_free      = get "MemFree";
      buffers       = get "Buffers";
      cached        = get "Cached";
      swap_cached   = get "SwapCached";
      swap_free     = get "SwapFree";
      active        = get "Active";
      inactive      = get "Inactive";
      swap_total    = get "SwapTotal";
      dirty         = get "Dirty";
      writeback     = get "Writeback";
      anon_pages    = get "AnonPages";
      mapped        = get "Mapped";
      slab          = get "Slab";
      page_tables   = get "PageTables";
      nfs_unstable  = get "NFS_Unstable";
      bounce        = get "Bounce";
      commit_limit  = get "CommitLimit";
      committed_as  = get "Committed_AS";
      vmalloc_total = get "VmallocTotal";
      vmalloc_used  = get "VmallocUsed";
      vmalloc_chunk = get "VmallocChunk";
    }
  ;;
end ;;

module Loadavg = struct
  type t = {
    one : float;
    ten : float;
    fifteen : float;
  } with fields

  (* /proc/loadavg has just 1 line nearly all the time, but occasionally there's an extra
     blank line. just be extra forgiving and grab only the first line. *)
  let load_exn () =
    match In_channel.read_lines "/proc/loadavg" with
    | line::_rest -> begin match String.split line ~on:' ' with
      | o::t::f::_rest ->
        let one,ten,fifteen = Float.of_string o, Float.of_string t, Float.of_string f in
        {one;ten;fifteen}
      | _ -> failwithf "couldn't parse load average from line: %s" line ()
    end
    | [] -> failwith "no lines read from /proc/loadavg!"
end

let is_pid s =
  try
    let _ = Int.of_string s in
    true
  with
    Failure (_) -> false
;;

let get_all_procs () =
  Sys.readdir "/proc"
  |! Array.filter_map ~f:(fun pid ->
    (* Failures usually aren't a fatal *system* condition.
       procfs queries on Linux simply are not consistent.
       They're generally thwarted by terminating processes.
       We simply skip the proc entry on failure.
    *)
    Option.try_with (fun () -> Process.load_exn (Pid.of_string pid))
  )
  |! Array.to_list
;;

let with_pid_exn = Process.load_exn ;;

let with_pid pid = Option.try_with (fun () -> with_pid_exn pid) ;;

let with_uid uid = List.filter (get_all_procs ()) ~f:(fun p -> Process.Status.uid (Process.status p) = uid) ;;

let with_username_exn name = with_uid (Unix.Passwd.getbyname_exn name).Unix.Passwd.uid ;;

let with_username name = Option.try_with (fun () -> with_username_exn name) ;;

let get_uptime () =
  match string_of_file "/proc/uptime" |! String.split ~on:' ' with
  | secs_since_boot :: _ -> Float.of_string secs_since_boot
  | _ -> failwithf "Error parsing /proc/uptime" ()
;;

(*
 * This is a partial translation of
 *   sysinfo.c:init_Hertz_value from procps (top)
 *)
let jiffies_per_second_exn () =
  let rec sample () =
    let up1 = get_uptime () in

    let statlines = In_channel.read_lines "/proc/stat" in
    (* On modern systems the second line is always cpu0 (even uni-processors) *)
    let statline = Option.value_exn (List.nth statlines 1) in

    let user_j, nice_j, sys_j, idle_j, iowait_j =
      sscanf statline "cpu0 %Lu %Lu %Lu %Lu %Lu" (fun a b c d e -> a,b,c,d,e)
    in
    let up2 = get_uptime () in
    if ((up2 -. up1) > 0.01) then
      sample ()  (* sampling latency too high.  try again *)
    else
      let (+) = Int64.(+) in
      user_j + nice_j + sys_j + idle_j + iowait_j, ((up1 +. up2) /. 2.)
  in
  let jiffies, seconds = sample () in
  (Int64.to_float jiffies) /. seconds
;;

let jiffies_per_second () = Option.try_with jiffies_per_second_exn ;;

let meminfo_exn = Meminfo.load_exn
let meminfo () = Option.try_with meminfo_exn

let loadavg_exn = Loadavg.load_exn
let loadavg () = Option.try_with loadavg_exn

let pgrep f = List.filter (get_all_procs ()) ~f ;;

let pkill ~signal f =
  List.fold (get_all_procs ()) ~init:[] ~f:(fun a p ->
      if not (f p) then a else begin
        let pid = Process.pid p in
        let result =
          try Ok (ignore (Signal.send signal (`Pid pid))) with
          | Unix.Unix_error (e, _, _) -> Error e
          | e ->
            Exn.reraisef e
              "Procfs.pkill caught exception trying to signal process %s"
              (Pid.to_string pid) ()
        in
        (pid, result) :: a
      end)
;;

module Net = struct

  module Dev = struct
    type t =
      {
      iface : string;
      rx_bytes  : int;
      rx_packets: int;
      rx_errs   : int;
      rx_drop   : int;
      rx_fifo   : int;
      rx_frame  : int;
      rx_compressed : bool;
      rx_multicast : bool;
      tx_bytes  : int;
      tx_packets: int;
      tx_errs   : int;
      tx_drop   : int;
      tx_fifo   : int;
      tx_colls  : int;
      tx_carrier: int;
      tx_compressed : bool;
      }
      with fields;;

  let eval_mul_comp rx_tx =
    match rx_tx with
    | 0 -> false;
    | 1 -> true;
    | _ -> failwithf "Proc.Net.Dev error : value is %d, expected 0 or 1." rx_tx ()

  let of_string str =
    let s = String.strip in
    let ios str  = Int.of_string str in
    match String.split ~on:'\t' str with
    | [ iface; rx_bytes; rx_packets; rx_errs; rx_drop; rx_fifo; rx_frame; rx_compressed;
    rx_multicast; tx_bytes; tx_packets; tx_errs; tx_drop; tx_fifo; tx_colls; tx_carrier;
    tx_compressed ] ->
    Some {
      iface = s iface;
      rx_bytes = ios (s rx_bytes);
      rx_packets = ios (s rx_packets);
      rx_errs   = ios (s rx_errs);
      rx_drop   = ios (s rx_drop);
      rx_fifo   = ios (s rx_fifo);
      rx_frame  = ios (s rx_frame);
      rx_compressed = (eval_mul_comp (ios rx_compressed));
      rx_multicast = (eval_mul_comp (ios rx_multicast));
      tx_bytes  = ios (s tx_bytes);
      tx_packets = ios (s tx_packets);
      tx_errs   = ios (s tx_errs);
      tx_drop   = ios (s tx_drop);
      tx_fifo   = ios (s tx_fifo);
      tx_colls  = ios (s tx_colls);
      tx_carrier = ios (s tx_carrier);
      tx_compressed = (eval_mul_comp (ios tx_compressed));
    }
    | _ -> failwithf "Net.Dev.of_string: unsupported format: %s" str ()

    (* add interfaces () to get a list of all interfaces on box *)

  end
  module Route = struct

  type t =
    {
      iface : string;
      destination : Unix.Inet_addr.t ;
      gateway     : Unix.Inet_addr.t ;
      flags       : int;
      refcnt      : int;
      use         : int;
      metric      : int;
      mask        : Unix.Inet_addr.t;
      mtu         : int;
      window      : int;
      irtt        : int;
    }
    with fields;;

  let unix_inet_addr_of_revhex revhex_str =
    let ip =  sscanf revhex_str "%2x%2x%2x%2x" (fun a b c d -> sprintf "%d.%d.%d.%d" d c b
    a) in
    Unix.Inet_addr.of_string ip ;;

  let of_string str =
    let s = String.strip in
    match String.split ~on:'\t' str with
    | [ iface; dest; gw; flags; refcnt; use; metric; mask; mtu; window; irtt ] ->
    Some {
      iface = iface;
      destination = unix_inet_addr_of_revhex dest;
      gateway = unix_inet_addr_of_revhex gw;
      flags = Int.of_string flags;
      refcnt = Int.of_string refcnt;
      use = Int.of_string use;
      metric = Int.of_string metric;
      mask = unix_inet_addr_of_revhex mask;
      mtu    = Int.of_string mtu;
      window = Int.of_string window;
      irtt = Int.of_string (s irtt);
    }
    | _ -> failwithf "Net.Route.of_string: unsupported format: %s" str ()

  let raw_route_list () =
    let routes = In_channel.with_file "/proc/net/route" ~f:In_channel.input_lines |!
    List.tl_exn in
    List.filter_map routes ~f:(of_string)

  let default  =
    let default_route = Unix.Inet_addr.bind_any in
    fun () -> match List.filter_map ( raw_route_list () ) ~f:(fun x ->
      if ( x.destination = default_route )  then Some x.gateway else
        None
    )  with
    | [x] -> x
    | [] -> failwith "No default gateway set?"
    | unk -> failwithf "Looks like there are > 1 gateway set: %s !"
    (String.concat ~sep:", " (List.map unk ~f:Unix.Inet_addr.to_string )) ()
  ;;

 end
end

module Mount = struct
  type t =
    {
      spec    : string; (* block device special name *)
      file    : string; (* fs path prefix *)
      vfstype : string; (* ext3, nfs, etc. *)
      mntops  : string list; (* mount options -o *)
      freq    : int; (* dump frequency *)
      passno  : int; (* pass number of parallel dump *)
    }
  with fields ;;

  let of_string s =
    match String.split ~on:' ' s |! List.filter ~f:(fun s -> s <> "") with
    | [ spec; file; vfstype; mntops; freq; passno ] ->
      { spec;
        file;
        vfstype;
        mntops = String.split ~on:',' mntops;
        freq = Int.of_string freq;
        passno = Int.of_string passno
      }
    | _ -> failwithf "Mount.of_string: unsupported format: %s" s ()
end

let mounts () =
  In_channel.with_file "/proc/mounts" ~f:In_channel.input_lines
  |! List.map ~f:Mount.of_string
;;

let mounts_of_fstab () =
  In_channel.with_file "/etc/fstab" ~f:In_channel.input_lines
  (* strip comments *)
  |! List.map ~f:(fun line -> (* filter '#' *)
    match String.split ~on:'#' line with
    | [] -> "" | content :: _comment -> content)
  (* turn tabs into spaces *)
  |! List.map ~f:(String.tr ~target:'\t' ~replacement:' ')
  (* chop padding *)
  |! List.map ~f:String.strip
  |! List.filter ~f:(fun line -> line <> "")
  |! List.map ~f:Mount.of_string
;;

let supported_filesystems () =
  In_channel.with_file "/proc/filesystems" ~f:In_channel.input_lines
  |! List.map ~f:(fun line ->
    match String.split ~on:'\t' line |! List.rev with
    | vfstype :: _ -> vfstype
    | _ -> failwithf "Procfs.supported_filesystems: bad format: %s" line ())
;;

let uptime () = get_uptime () |! Time.Span.of_float ;;
