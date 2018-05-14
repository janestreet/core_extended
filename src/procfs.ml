open Core
open Sexplib_num.Std

(* Learn more about this business by consulting proc(5) *)

(* lu and ld match the proc(5) format strings %lu and %ld *)
let lu x = Big_int.big_int_of_string x
let ld x = lu x   (* bigint everything so we don't have to worry about overflows *)

(* In_channel.input_all creates a 64k buffer and string every time. Also, In_channel.t's
   are custom blocks with finalizers that are allocated on the major heap. get_all_procs
   calls this many, many times and causes terrible GC performance. This un-thread-safe
   version is a work-around. *)
let input_all_with_reused_buffer () =
  let buf_size = 65536 in
  let buf = Bytes.create buf_size in
  let buffer = Buffer.create buf_size in
  let read_all fd =
    let rec loop () =
      let len = Unix.read fd ~buf ~len:(Bytes.length buf) in
      if len > 0 then begin
        Buffer.add_subbytes buffer buf ~pos:0 ~len;
        loop ();
      end
    in
    loop ();
    let res = Buffer.contents buffer in
    Buffer.clear buffer;
    res
  in
  let input_all fn = Unix.with_file fn ~mode:[Unix.O_RDONLY] ~f:read_all in
  stage input_all
;;

let string_of_file = unstage (input_all_with_reused_buffer ())

type bigint = Big_int.big_int [@@deriving sexp] ;;

module Process = struct

  module Inode = struct
    type t = Int64.t [@@deriving sexp, bin_io] ;;
    let of_string = Int64.of_string ;;
    let to_string = Int64.to_string ;;
  end ;;

  module Limits = struct
    module Rlimit = struct
      type value = [ `unlimited | `limited of bigint ] [@@deriving sexp] ;;
      type t = { soft : value; hard: value } [@@deriving fields, sexp] ;;
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
    [@@deriving fields, sexp] ;;

    let of_string s =
      let map =
        String.split s ~on:'\n'
        |> List.map ~f:String.strip
        |> List.filter ~f:(fun line -> line <> "")
        |> List.fold ~init:String.Map.empty ~f:(fun map line ->
          match
            String.strip line
            |> String.lowercase
            |> String.split ~on:' '
            |> List.filter ~f:(fun s -> s <> "")
            |> List.rev
          with
          | ("units" | "seconds" | "bytes" | "processes" | "locks" | "signals" | "files")
            :: hard_limit :: soft_limit :: name ->
              let key = List.rev name |> String.concat ~sep:" " in
              Map.set map ~key ~data:(soft_limit, hard_limit)

          (* priorities don't have an entry in the "Units" column *)
          | hard_limit :: soft_limit :: name ->
              let key = List.rev name |> String.concat ~sep:" " in
              Map.set map ~key ~data:(soft_limit, hard_limit)

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
        { Rlimit.soft = fst data |> value; hard = snd data |> value }
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
        pgrp        : Pid.t option;    (** The process group ID of the process. *)
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
    [@@deriving fields, sexp] ;;

    (* extract_command, for a stat string such as: "14574 (cat) R 10615 14574 10615 34820
       14574 4194304 164 0..." returns this tuple "cat", "R 10615 14574 10615..." *)
    let extract_command s =
      let i = String.index_exn s '(' in
      let j = String.rindex_exn s ')' in
      (`command (String.sub s ~pos:(i+1) ~len:(j-(i+1))),
       `rest (String.sub s ~pos:(j+1) ~len:(String.length s - (j+1))))

    let of_string s =
      let `command comm, `rest rest = extract_command s in
      let a = Array.of_list (String.split (String.strip rest) ~on:' ') in
      let d x = int_of_string x in
      let c x = x.[0] in
      { comm        = comm;
        state       = c a.(0);
        ppid        = (match d a.(1) with
        | x when x < 1 -> None
        | x            -> Some (Pid.of_int x));
          (*pgrp        = Pid.of_int (d a.(2)); *)
          pgrp        =  (match (d a.(2)) with
                          | x when x < 1 -> None
                          | x            -> Some (Pid.of_int x));
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
    [@@deriving fields, sexp] ;;
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
    [@@deriving fields, sexp] ;;
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
      Scanf.sscanf (String.concat ~sep:" " [String.strip uids; String.strip gids])
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
    [@@deriving sexp, bin_io] ;;
    type t =
      {
        fd      : int;     (** File descriptor (0=stdin, 1=stdout, etc.) *)
        fd_stat : fd_stat; (** Kind of file *)
      }
    [@@deriving fields, sexp, bin_io] ;;
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
  [@@deriving fields, sexp] ;;

  let load_exn pid =
    let slurp f fn =
      try
        Some (f (sprintf !"/proc/%{Pid}/%s" pid fn))
      with
      | Sys_error _ -> None
      | Unix.Unix_error (EACCES, _, _) -> None
      | Unix.Unix_error (ENOENT, _, _) -> None
      | Unix.Unix_error (EINVAL, _, _) -> None
    in
    let slurp_file fn = slurp string_of_file fn in
    let slurp_link fn = slurp Unix.readlink fn in
    let slurp_dir fn = slurp Sys.readdir fn in

    let required x = Option.value_exn x in
    let require_str f = slurp_file f |> required in
    let require_int f = slurp_file f |> required |> String.strip |> Int.of_string in
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
      |> String.strip
    in
    let task_stats =
      Array.fold (required (slurp_dir "task"))
        ~init:Pid.Map.empty
        ~f:(fun m task ->
          Pid.Map.set m
            ~key:(Pid.of_string task)
            ~data:(Stat.of_string (require_str
              (String.concat ~sep:"/" ["task"; task; "stat"])))
        )
    in
    let fds =
      try
        Some (
          Sys.readdir (sprintf "/proc/%s/fd" (Pid.to_string pid))
          |> Array.to_list
          |> List.filter_map ~f:(fun fd_str ->
            let fd = Int.of_string fd_str in
            slurp Unix.readlink ("fd/" ^ fd_str)
              |> Option.map ~f:(fun path ->
                let parse inode = (* "[123]" -> 123 *)
                  inode |>
                  String.chop_prefix_exn ~prefix:"[" |>
                  String.chop_suffix_exn ~suffix:"]" |>
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

      (* New kernel field in CentOS 7 *)
      mem_available : bigint sexp_option;
    }
  [@@deriving fields, sexp] ;;
  let load_exn () =
    let of_kb = Big_int.mult_int_big_int 1024 in
    let map =
      In_channel.read_lines "/proc/meminfo"
      |> List.fold ~init:String.Map.empty ~f:(fun map line ->
        match String.strip line
              |> String.tr ~target:':' ~replacement:' '
              |> String.split ~on:' '
              |> List.filter ~f:(fun s -> s <> "")
        with
        | key :: value :: "kB" :: [] ->
            let data = Big_int.big_int_of_string value |> of_kb in
            Map.set map ~key ~data
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

      mem_available = Map.find map "MemAvailable";
    }
  ;;

  let mem_available t =
    (* Use the MemAvailable field in newer kernels that have it, otherwise
       make a best efforts guess *)
    match t.mem_available with
    | Some ma -> ma
    | None ->
      let (+) = Big_int.add_big_int in
      t.mem_free + t.buffers + t.cached
  ;;

end ;;

(** Parse /proc/stat because vmstat is dumb *)
module Kstat = struct
  type index_t = All | Number of int [@@deriving sexp]

  type cpu_t =
    {
      user : bigint;
      nice  : bigint;
      sys: bigint;
      idle: bigint;
      iowait: bigint option;
      irq: bigint option;
      softirq: bigint option;
      steal: bigint option;
      guest: bigint option;
    } [@@deriving fields, sexp];;

  type t =
    index_t * cpu_t

  let parse_line l =
    match l with
    | [user;nice;sys;idle;iowait;irq;softirq;steal;guest] ->
      (* > 2.6.24 *)
      {  user = Big_int.big_int_of_string user;
         nice  = Big_int.big_int_of_string nice ;
         sys  = Big_int.big_int_of_string sys;
         idle  = Big_int.big_int_of_string idle;
         iowait = Some (Big_int.big_int_of_string iowait);
         irq = Some (Big_int.big_int_of_string irq);
         softirq = Some (Big_int.big_int_of_string softirq);
         steal = Some (Big_int.big_int_of_string steal);
         guest = Some (Big_int.big_int_of_string guest)}
    | [user;nice;sys;idle;iowait;irq;softirq;steal] ->
      (* > 2.6.11 *)
      {  user = Big_int.big_int_of_string user;
         nice  = Big_int.big_int_of_string nice ;
         sys  = Big_int.big_int_of_string sys;
         idle  = Big_int.big_int_of_string idle;
         iowait = Some (Big_int.big_int_of_string iowait);
         irq = Some (Big_int.big_int_of_string irq);
         softirq = Some (Big_int.big_int_of_string softirq);
         steal = Some (Big_int.big_int_of_string steal);
         guest = None}
    | [user;nice;sys;idle;iowait;irq;softirq] ->
      (* > 2.6.0  *)
      {  user = Big_int.big_int_of_string user;
         nice  = Big_int.big_int_of_string nice ;
         sys  = Big_int.big_int_of_string sys;
         idle  = Big_int.big_int_of_string idle;
         iowait = Some (Big_int.big_int_of_string iowait);
         irq = Some (Big_int.big_int_of_string irq);
         softirq = Some (Big_int.big_int_of_string softirq);
         steal = None;
         guest = None}
    | [user; nice; sys; idle] ->
      (* < 2.5.41 ish *)
      {  user = Big_int.big_int_of_string user;
         nice  = Big_int.big_int_of_string nice ;
         sys  = Big_int.big_int_of_string sys;
         idle  = Big_int.big_int_of_string idle;
         iowait = None;
         irq = None;
         softirq = None;
         steal = None;
         guest = None }

    |_ -> failwith "No idea what this line is"


  let load_exn () =
    In_channel.read_lines "/proc/stat"
    |> List.fold ~init:[] ~f:(fun accum line ->
      match String.strip line
            |> String.split ~on:' '
            |> List.filter ~f:(fun s -> s <> "")
      with
      | "cpu" :: rest ->
        (All, (parse_line rest)) :: accum
      | cpuidx :: rest ->
        if String.is_prefix ~prefix:"cpu" cpuidx then
          let idx = String.slice cpuidx 3 (String.length cpuidx)  in
          ((Number (Int.of_string idx)), (parse_line rest)) :: accum
        else
          accum
      | _ -> accum (* ignore weird lines *)
    )


end


module Loadavg = struct
  type t = {
    one : float;
    ten : float;
    fifteen : float;
  } [@@deriving fields]

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

let get_all_procs () =
  Sys.readdir "/proc"
  |> Array.to_list
  |> List.filter_map ~f:(fun pid ->
    (* Failures usually aren't a fatal *system* condition.
       procfs queries on Linux simply are not consistent.
       They're generally thwarted by terminating processes.
       We simply skip the proc entry on failure.
    *)
    Option.try_with (fun () -> Process.load_exn (Pid.of_string pid))
  )
;;

let with_pid_exn = Process.load_exn ;;

let with_pid pid = Option.try_with (fun () -> with_pid_exn pid) ;;

let with_uid uid = List.filter (get_all_procs ()) ~f:(fun p -> Process.Status.uid (Process.status p) = uid) ;;

let with_username_exn name = with_uid (Unix.Passwd.getbyname_exn name).Unix.Passwd.uid ;;

let with_username name = Option.try_with (fun () -> with_username_exn name) ;;

let get_uptime () =
  match string_of_file "/proc/uptime" |> String.split ~on:' ' with
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
      Scanf.sscanf statline "cpu0 %Lu %Lu %Lu %Lu %Lu" (fun a b c d e -> a,b,c,d,e)
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

let process_age' ~jiffies_per_second p =
  let start_time =
    Big_int.float_of_big_int p.Process.stat.Process.Stat.starttime
    /. jiffies_per_second
  in
  Time.Span.of_sec (get_uptime () -. start_time)
;;

let process_age p =
  Option.map (jiffies_per_second ()) ~f:(fun jiffies_per_second ->
    process_age' ~jiffies_per_second p)
;;

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
      [@@deriving fields];;

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
  let interfaces () =
    In_channel.with_file "/proc/net/dev" ~f:In_channel.input_lines
    |> List.tl_exn
    |> List.tl_exn
    |> List.map ~f:(fun x ->
      let rex = Re2.create_exn "(\\w+):" in
      let matches = Re2.find_submatches_exn rex (String.lstrip x) in
      matches.(1) |> Option.value ~default:""
    )
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
    [@@deriving fields];;

  let unix_inet_addr_of_revhex revhex_str =
    let ip =
      Scanf.sscanf revhex_str "%2x%2x%2x%2x"
        (fun a b c d -> sprintf "%d.%d.%d.%d" d c b a)
    in
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
    let routes = In_channel.with_file "/proc/net/route" ~f:In_channel.input_lines |>
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

  (* This doesn't belong here at all but meh *)
  module Tcp_state = struct
    type t =
        TCP_ESTABLISHED
        | TCP_SYN_SENT
        | TCP_SYN_RECV
        | TCP_FIN_WAIT1
        | TCP_FIN_WAIT2
        | TCP_TIME_WAIT
        | TCP_CLOSE
        | TCP_CLOSE_WAIT
        | TCP_LAST_ACK
        | TCP_LISTEN
        | TCP_CLOSING
        | TCP_MAX_STATES

    let to_int = function
        | TCP_ESTABLISHED -> 1
        | TCP_SYN_SENT -> 2
        | TCP_SYN_RECV -> 3
        | TCP_FIN_WAIT1 -> 4
        | TCP_FIN_WAIT2 -> 5
        | TCP_TIME_WAIT -> 6
        | TCP_CLOSE -> 7
        | TCP_CLOSE_WAIT -> 8
        | TCP_LAST_ACK -> 9
        | TCP_LISTEN -> 10
        | TCP_CLOSING -> 11
        | TCP_MAX_STATES -> 12

    let of_int = function
        | 1 -> TCP_ESTABLISHED
        | 2 -> TCP_SYN_SENT
        | 3 -> TCP_SYN_RECV
        | 4 -> TCP_FIN_WAIT1
        | 5 -> TCP_FIN_WAIT2
        | 6 -> TCP_TIME_WAIT
        | 7 -> TCP_CLOSE
        | 8 -> TCP_CLOSE_WAIT
        | 9 -> TCP_LAST_ACK
        | 10 -> TCP_LISTEN
        | 11 -> TCP_CLOSING
        | 12 -> TCP_MAX_STATES
        | _ -> failwith "Invalid tcp status flag"

    let of_hex s = of_int (Int.of_string ("0x"^s))
    (* to_hex? to_string? *)

  end

  module Tcp = struct
    type t =
      {
        sl : int;
        local_address : Core.Unix.Inet_addr.t;
        local_port : Extended_unix.Inet_port.t;
        remote_address : Core.Unix.Inet_addr.t;
        remote_port : Extended_unix.Inet_port.t option;
        state : Tcp_state.t;
        tx_queue : int;
        rx_queue : int;
        tr:int;
        tm_when : int;
        retrnsmt: int;
        uid : int;
        timeout : int;
        inode : Process.Inode.t; (* I think this is right *)
        rest : string;
      } [@@deriving fields]

    let dehex ~int_of_string s = int_of_string ("0x"^s)
    let dehex_int   = dehex ~int_of_string:Int.of_string
    let dehex_int32 = dehex ~int_of_string:Int32.of_string

    let of_line_exn line =
      match String.tr ~target:':' ~replacement:' ' line
         |> String.split ~on:' '
         |> List.filter ~f:(fun x -> x <> "") with
          | [sl; local_address; local_port; remote_address; remote_port; st;
            tx_queue; rx_queue; tr;tm_when; retrnsmt; uid; timeout; inode;
            plus; some; other; state; i; guess; lol] ->
        {
          sl = Int.of_string sl;
          local_address= Unix.Inet_addr.inet4_addr_of_int32
              (Extended_unix.ntohl (dehex_int32 local_address));
          local_port= Extended_unix.Inet_port.of_int_exn (dehex_int local_port);
          remote_address= Unix.Inet_addr.inet4_addr_of_int32
            (Extended_unix.ntohl (dehex_int32 remote_address));

          (* This can be 0 which is technically invalid but...*)
          remote_port = Extended_unix.Inet_port.of_int (dehex_int remote_port);

          state= Tcp_state.of_hex st;
          tx_queue = dehex_int tx_queue;
          rx_queue = dehex_int rx_queue;
          tr = dehex_int tr ;
          tm_when = dehex_int tm_when;
          retrnsmt = dehex_int retrnsmt;
          uid = Int.of_string uid;
          timeout = Int.of_string timeout;
          inode = Process.Inode.of_string inode;
          rest = String.concat ~sep:" " [plus; some; other; state; i; guess; lol]
        }
      | _ ->
        failwith "Unable to parse this line!\n%!"

    let of_line line =
      try
        Some (of_line_exn line )
      with _ ->
        None

    let load_exn () =
      let lines = In_channel.read_lines "/proc/net/tcp" in
      List.fold ~init:[] ~f:(fun res line ->
          match of_line line with
          | Some data ->
            data :: res
          | None ->
            res
      )
      lines

  let%test _ = of_line "  40: 458719AC:9342 CC1619AC:0016 01 00000000:00000000 02:000296F0 00000000 12021        0      64400541 2 ffff88022c777400 20 3 0 10 -1"
  <> None

  (* Port 0 on the other side *)
  let%test _ = of_line "  31: 0100007F:177E 00000000:0000 0A 00000000:00000000 00:00000000 00000000 12021        0 778748 1 ffff8102edd1ad00 3000 0 0 2 -1"
  <> None

  let%test _ = of_line
  "  40: 458719AC:9342 CC1619AC:0016 01 00000000:00000000 02:000296F0 00000000 12021        0      64400541 2 ffff88022c777400 20 3 0 10 -1"
  = Some { sl = 40;
  local_address = (Unix.Inet_addr.of_string "172.25.135.69");
  local_port = (Extended_unix.Inet_port.of_int_exn 37698);
  remote_address = (Unix.Inet_addr.of_string "172.25.22.204");
  remote_port = Some (Extended_unix.Inet_port.of_int_exn 22);
  state = Tcp_state.TCP_ESTABLISHED;
  tx_queue = 0; rx_queue = 0; tr = 2; tm_when = 169712; retrnsmt = 0; uid = 12021;
  timeout = 0; inode = Process.Inode.of_string "64400541"; rest = "2 ffff88022c777400 20 3 0 10 -1" }

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
  [@@deriving fields] ;;

  let of_string s =
    match String.split ~on:' ' s |> List.filter ~f:(fun s -> s <> "") with
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
  |> List.map ~f:Mount.of_string
;;

let mounts_of_fstab () =
  In_channel.with_file "/etc/fstab" ~f:In_channel.input_lines
  (* strip comments *)
  |> List.map ~f:(fun line -> (* filter '#' *)
    match String.split ~on:'#' line with
    | [] -> "" | content :: _comment -> content)
  (* turn tabs into spaces *)
  |> List.map ~f:(String.tr ~target:'\t' ~replacement:' ')
  (* chop padding *)
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun line -> line <> "")
  |> List.map ~f:Mount.of_string
;;

let supported_filesystems () =
  In_channel.with_file "/proc/filesystems" ~f:In_channel.input_lines
  |> List.map ~f:(fun line ->
    match String.split ~on:'\t' line |> List.rev with
    | vfstype :: _ -> vfstype
    | _ -> failwithf "Procfs.supported_filesystems: bad format: %s" line ())
;;

let uptime () = get_uptime () |> Time.Span.of_sec ;;
