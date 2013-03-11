open Core.Std

(**
   Process and system stats
*)

module Process : sig
  module Inode : sig
    type t with sexp ;;
    val of_string : string -> t
    val to_string : t -> string
  end ;;
  module Limits : sig
    module Rlimit : sig
      type value = [ `unlimited | `limited of Int63.t ] with sexp ;;
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

    val of_string : string -> t
  end ;;
  module Stat : sig
    type t =
      {
        comm        : string; (** The filename of the executable *)
        state       : char;   (** One  character from the string "RSDZTW" *)
        ppid        : Pid.t option;  (** The PID of the parent. *)
        pgrp        : Pid.t option ;  (** The process group ID of the process. *)
        session     : int;    (** The session ID of the process. *)
        tty_nr      : int;    (** The tty the process uses. *)
        tpgid       : int;    (** The process group ID of the process which currently owns
                                  the tty... *)
        flags       : Int63.t; (** The kernel flags word of the process. *)
        minflt      : Int63.t; (** The number of minor faults the process has made which have
                                  not required loading a memory page from disk. *)
        cminflt     : Int63.t; (** The number of minor faults that the process’s waited-for
                                  children have made. *)
        majflt      : Int63.t; (** The number of major faults the process has made which have
                                  required loading a page from disk. *)
        cmajflt     : Int63.t; (** The number of major faults that the process’s waited-for
                                  children have made. *)
        utime       : Int63.t; (** The number of jiffies that this process has been scheduled
                                  in user mode. *)
        stime       : Int63.t; (** The number of jiffies that this process has been scheduled
                                  in kernel mode. *)
        cutime      : Int63.t; (** The number of jiffies that this process’s waited-for
                                  children have been scheduled in user mode. *)
        cstime      : Int63.t; (** The number of jiffies that this process’s waited-for
                                  children have been scheduled in kernel mode. *)
        priority    : Int63.t; (** The standard nice value, plus fifteen.  The value is never
                                  negative in the kernel. *)
        nice        : Int63.t; (** The nice value ranges from 19 to -19*)
        unused      : Int63.t; (** placeholder for removed field *)
        itrealvalue : Int63.t; (** The time in jiffies before the next SIGALRM is sent to the
                                  process due to an interval timer. *)
        starttime   : Int63.t; (** The time in jiffies the process started after system boot.*)
        vsize       : Int63.t; (** Virtual memory size in bytes. *)
        rss         : Int63.t; (** Resident Set Size: number of pages the process has in real
                                  memory. *)
        rlim        : Int63.t; (** Current limit in bytes on the rss of the process. *)
        startcode   : Int63.t; (** The address above which program text can run. *)
        endcode     : Int63.t; (** The address below which program text can run. *)
        startstack  : Int63.t; (** The address of the start of the stack. *)
        kstkesp     : Int63.t; (** The current value of esp (stack pointer) *)
        kstkeip     : Int63.t; (** The current value of eip (instruction pointer) *)
        signal      : Int63.t; (** The bitmap of pending signals. *)
        blocked     : Int63.t; (** The bitmap of blocked signals. *)
        sigignore   : Int63.t; (** The bitmap of ignored signals. *)
        sigcatch    : Int63.t; (** The bitmap of caught signals. *)
        wchan       : Int63.t; (** This is  the "channel" in which the process is waiting.
                                  Address of a system call. *)
        nswap       : Int63.t; (** (no longer maintained) *)
        cnswap      : Int63.t; (** (no longer maintained) *)
        exit_signal : int;    (** Signal sent to parent when we die. *)
        processor   : int;    (** CPU number last executed on. *)
        rt_priority : Int63.t; (** Real-time scheduling priority. *)
        policy      : Int63.t; (** Scheduling policy *)
      }
    with fields, sexp ;;

    val of_string : string -> t
  end ;;

  module Statm : sig
    type t =
      {
        size     : Int63.t; (** total program size *)
        resident : Int63.t; (** resident set size *)
        share    : Int63.t; (** shared pages *)
        text     : Int63.t; (** text (code) *)
        lib      : Int63.t; (** library *)
        data     : Int63.t; (** data/stack *)
        dt       : Int63.t; (** dirty pages (unused) *)
      }
    with fields, sexp ;;

    val of_string : string -> t
  end ;;

  module Status : sig
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

    val of_string : string -> t
  end ;;

  module Fd : sig
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
      oom_adj     : int;              (** OOM killer niceness [range: -17 to +15] *)
      oom_score   : int;              (** OOM "sacrifice" priority *)
    }
  with fields, sexp ;;
end ;;

module Meminfo : sig
  (** [t] corresponds to the values in /proc/meminfo.  All values in bytes. *)
  type t =
    {
      mem_total     : Int63.t;
      mem_free      : Int63.t;
      buffers       : Int63.t;
      cached        : Int63.t;
      swap_cached   : Int63.t;
      active        : Int63.t;
      inactive      : Int63.t;
      swap_total    : Int63.t;
      swap_free     : Int63.t;
      dirty         : Int63.t;
      writeback     : Int63.t;
      anon_pages    : Int63.t;
      mapped        : Int63.t;
      slab          : Int63.t;
      page_tables   : Int63.t;
      nfs_unstable  : Int63.t;
      bounce        : Int63.t;
      commit_limit  : Int63.t;
      committed_as  : Int63.t;
      vmalloc_total : Int63.t;
      vmalloc_used  : Int63.t;
      vmalloc_chunk : Int63.t;
    }
  with fields, sexp ;;
end ;;

module Kstat : sig
  type index_t = All | Number of int with sexp

  type cpu_t =
    {
      user    : Int63.t;
      nice    : Int63.t;
      sys     : Int63.t;
      idle    : Int63.t;
      iowait  : Int63.t option;
      irq     : Int63.t option;
      softirq : Int63.t option;
      steal   : Int63.t option;
      guest   : Int63.t option;
    } with fields, sexp;;

  type t =
    index_t * cpu_t

  val load_exn : unit -> t list

end
module Loadavg : sig
  (** [t] corresponds to the values in /proc/loadavg. *)
  type t = {
    one : float;
    ten : float;
    fifteen : float;
  } with fields
end

(** [get_all_procs] returns a list of all processes on the system *)
val get_all_procs : unit -> Process.t list

(** [with_pid_exn pid] returns a single process that matches pid, or raises Not_found *)
val with_pid_exn : Pid.t -> Process.t

(** [with_pid pid] returns a single process that matches pid *)
val with_pid : Pid.t -> Process.t option

(** [with_uid uid] returns all processes owned by uid *)
val with_uid : int -> Process.t list

(** [pgrep f] returns all processes for which f is true *)
val pgrep : (Process.t -> bool) -> Process.t list

(** [pkill ~signal f] sends the signal to all processes for which f returns true. It
   returns the list of processes that were signaled, and the resulting errors if any. *)
val pkill : signal:Signal.t -> (Process.t -> bool) -> (Pid.t * (unit, Unix.error) Result.t) list

(** [with_username_exn user] calls with_uid after looking up the user's uid *)
val with_username_exn : string -> Process.t list

(** [with_username user] calls with_uid after looking up the user's uid *)
val with_username : string -> Process.t list option

(** [jiffies_per_second_exn].  A jiffy "is one tick of the system timer interrupt.  It is
    not an absolute time interval unit, since its duration depends on the clock interrupt
    frequency of the particular hardware platform."

    Further reading: https://secure.wikimedia.org/wikipedia/en/wiki/Jiffy_(time)
 *)
val jiffies_per_second_exn : unit -> float
val jiffies_per_second : unit -> float option

(** [meminfo_exn] queries /proc/meminfo and fills out Meminfo.t.  All values in bytes. *)
val meminfo_exn : unit -> Meminfo.t
val meminfo : unit -> Meminfo.t option

(** [loadavg_exn] parses /proc/loadavg. *)
val loadavg_exn : unit -> Loadavg.t
val loadavg : unit -> Loadavg.t option

module Net : sig

  (*will put in some stuff from proc net *)

  module Dev : sig
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

    val interfaces : unit -> string list

    val of_string : string -> t option
  end

  module Route : sig
  type t =
    {
      iface : string; (* maybe this shouldn't be a string? *)
      destination : Unix.Inet_addr.t;
      gateway     : Unix.Inet_addr.t;
      flags       : int;
      refcnt      : int;
      use         : int;
      metric      : int;
      mask        : Unix.Inet_addr.t;
      mtu         : int;
      window      : int;
      irtt        : int;
    }
  with fields ;;

  val default : unit -> Unix.Inet_addr.t

  end

  (* This should probably be somewhere else but I don't know where. *)
  module Tcp_state : sig
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
    val to_int : t -> int
    val of_int : int -> t
  end

  (** /proc/net/tcp, or what netstat or lsof -i parses. *)
  module Tcp : sig
    type t =
      {
        sl : int;
        local_address : Core.Std.Unix.Inet_addr.t;
        local_port : Extended_unix.Inet_port.t;
        remote_address : Core.Std.Unix.Inet_addr.t;
        remote_port : Extended_unix.Inet_port.t option; (* can be 0 if there's no
        connection. *)
        state : Tcp_state.t;
        tx_queue : int;
        rx_queue : int;
        tr:int;
        tm_when : int;
        retrnsmt: int;
        uid : int;
        timeout : int;
        inode : Process.Inode.t;
        rest : string;
      } with fields

    (** These don't do any IO and should be async-ok *)
    val of_line : string -> t option
    val of_line_exn : string -> t

    (** This does IO and is not async-ok. *)
    val load_exn : unit -> t list

  end
end

module Mount : sig
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
end

val mounts : unit -> Mount.t list

val mounts_of_fstab : unit -> Mount.t list

val supported_filesystems : unit -> string list

val uptime : unit -> Time.Span.t

val process_age : Process.t -> Time.Span.t option
val process_age' : jiffies_per_second : float -> Process.t -> Time.Span.t
