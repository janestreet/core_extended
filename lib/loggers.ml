open Core.Std

open Format
open Unix
open Syslog

type 'a logger = ('a, unit, string, unit) format4 -> 'a

module type LOGGER = sig
  val set_level : Level.t -> unit
  val get_level : unit -> Level.t
  val may_log : Level.t -> bool

  val generic : Level.t -> 'a logger

  val debug : 'a logger
  val info : 'a logger
  val notice : 'a logger
  val warning : 'a logger
  val err : 'a logger
  val crit : 'a logger
  val alert : 'a logger
  val emerg : 'a logger
end

module type SPEC = sig val logger : Level.t -> string -> unit end

module MakeLogger (Spec : SPEC) : LOGGER = struct
  let log_level = ref Level.NOTICE
  let set_level v = log_level := v
  let get_level () = !log_level
  let may_log lev = Level.compare lev !log_level >= 0

  (* Makes sure Spec.logger doesn't need to be re-entrant *)
  let mtx = Mutex.create ()


  let generic lev fmt =
    ksprintf
      (fun str ->
         if may_log lev then
           Unix_utils.wrap_block_signals (fun () ->
             Mutex.critical_section mtx
               ~f:(fun () ->
                 Spec.logger lev str)))
      fmt

  let debug fmt = generic Level.DEBUG fmt
  let info fmt = generic Level.INFO fmt
  let notice fmt = generic Level.NOTICE fmt
  let warning fmt = generic Level.WARNING fmt
  let err fmt = generic Level.ERR fmt
  let crit fmt = generic Level.CRIT fmt
  let alert fmt = generic Level.ALERT fmt
  let emerg fmt = generic Level.EMERG fmt
end

module type CHANNEL_SPEC = sig
  val oc : out_channel
end

module MakeChannelSpec (ChannelSpec : CHANNEL_SPEC) : SPEC = struct
  let ppf = formatter_of_out_channel ChannelSpec.oc

  let max_lev_len =
    let coll n lev = max (String.length (Level.to_string lev)) n in
    List.fold ~f:coll ~init:0 Level.all + 1

  let string_of_unix_time mtime =
    let
        {
          tm_year = m_year; tm_mon = m_month; tm_mday = m_mday;
          tm_hour = m_hour; tm_min = m_min; tm_sec = m_sec; _
        } = localtime mtime in
    let m_sec = float m_sec +. Float.mod_float mtime 1. in
    sprintf "%04d-%02d-%02d/%02d:%02d:%05.2f"
      (1900 + m_year) (m_month + 1) m_mday m_hour m_min m_sec

  let last_msg_ref = ref (Level.DEBUG, "")
  let last_msg_time_ref = ref Float.neg_infinity
  let last_msg_count_ref = ref 0

  let get_lev_spaces lev =
    let str_lev = Level.to_string lev in
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
  let logger level msg = syslog ~level msg
end

module Syslog = MakeLogger (SyslogSpec)
module Ignore = MakeLogger (struct let logger _ _ = () end)
