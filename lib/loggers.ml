open Core.Std

open Format
open Unix
open Syslog

type 'a logger = ('a, unit, string, unit) format4 -> 'a

module type LOGGER = sig
  val set_lev : lev -> unit
  val get_lev : unit -> lev
  val may_log : lev -> bool

  val generic : lev -> 'a logger

  val debug : 'a logger
  val info : 'a logger
  val notice : 'a logger
  val warning : 'a logger
  val err : 'a logger
  val crit : 'a logger
  val alert : 'a logger
  val emerg : 'a logger
end

module type SPEC = sig val logger : lev -> string -> unit end

module MakeLogger (Spec : SPEC) : LOGGER = struct
  let log_lev = ref NOTICE
  let set_lev v = log_lev := v
  let get_lev () = !log_lev
  let may_log lev = compare_lev lev !log_lev >= 0

  (* Makes sure Spec.logger doesn't need to be re-entrant *)
  let mtx = Mutex.create ()


  let generic lev fmt =
    ksprintf (fun str ->
     if may_log lev then
        Unix_utils.wrap_block_signals (fun () ->
          Mutex.critical_section mtx
            ~f:(fun () ->
                  Spec.logger lev str))) fmt

  let debug fmt = generic DEBUG fmt
  let info fmt = generic INFO fmt
  let notice fmt = generic NOTICE fmt
  let warning fmt = generic WARNING fmt
  let err fmt = generic ERR fmt
  let crit fmt = generic CRIT fmt
  let alert fmt = generic ALERT fmt
  let emerg fmt = generic EMERG fmt
end

module type CHANNEL_SPEC = sig
  val oc : out_channel
end

module MakeChannelSpec (ChannelSpec : CHANNEL_SPEC) : SPEC = struct
  let ppf = formatter_of_out_channel ChannelSpec.oc

  let max_lev_len =
    let coll n lev = max (String.length (string_of_lev lev)) n in
    Array.fold ~f:coll ~init:0 all_levs + 1

  let string_of_unix_time mtime =
    let
        {
          tm_year = m_year; tm_mon = m_month; tm_mday = m_mday;
          tm_hour = m_hour; tm_min = m_min; tm_sec = m_sec; _
        } = localtime mtime in
    let m_sec = float m_sec +. mod_float mtime 1. in
    sprintf "%04d-%02d-%02d/%02d:%02d:%05.2f"
      (1900 + m_year) (m_month + 1) m_mday m_hour m_min m_sec

  let last_msg_ref = ref (DEBUG, "")
  let last_msg_time_ref = ref neg_infinity
  let last_msg_count_ref = ref 0

  let get_lev_spaces lev =
    let str_lev = string_of_lev lev in
    let spaces = String.make (max_lev_len - String.length str_lev) ' ' in
    str_lev, spaces

  let print_repeated () =
    let lev, _ = !last_msg_ref in
    let last_msg_time = string_of_unix_time !last_msg_time_ref in
    let str_lev, spaces = get_lev_spaces lev in
    let last_msg_count = !last_msg_count_ref in
    last_msg_count_ref := 0;
    fprintf ppf "%s  %s-%s:%slast message repeated %d times\n%!"
      last_msg_time Sys.executable_name str_lev spaces last_msg_count

  let logger lev msg =
    let now = Unix.gettimeofday () in
    let lev_msg = lev, msg in
    if lev_msg = !last_msg_ref then
      let () = incr last_msg_count_ref in
      if now -. !last_msg_time_ref > 1. then (
        last_msg_time_ref := now;
        print_repeated ())
      else ()
    else
      let time = string_of_unix_time now in
      let str_lev, spaces = get_lev_spaces lev in
      if !last_msg_count_ref > 0 then print_repeated ();
      last_msg_time_ref := now;
      last_msg_ref := lev_msg;
      fprintf ppf "%s  %s-%s:%s%s\n%!"
        time Sys.executable_name str_lev spaces msg
end

module MakeChannel (ChannelSpec : CHANNEL_SPEC) = struct
  module ChannelLogger = MakeChannelSpec (ChannelSpec)
  module Logger = MakeLogger(ChannelLogger)
  include Logger
end

module StderrChannelSpec = struct let oc = Pervasives.stderr end
module Stderr = MakeChannel (StderrChannelSpec)

module SyslogSpec : SPEC = struct
  let logger lev msg = syslog ~lev msg
end

module Syslog = MakeLogger (SyslogSpec)
module Ignore = MakeLogger (struct let logger _ _ = () end)
