(** A module for reading files.  *)

open Core.Std

(* Integer we use for file positions in this module.  Makes it easier to switch between
   regular native integers and 64 bit integers. *)
module Pos_int = Int64

let max_null_retries = 20

type lnum = Known of int | Unknown with sexp_of;;

(** The type of a linebuf. *)
type t = { mutable file: In_channel.t;      (** The channel we maintain. *)
           mutable pos: Pos_int.t;          (** Current file position. *)
           mutable lnum: lnum;              (** Current line position, if known *)
           mutable closed: bool;            (** Closed or open. *)
           mutable inode: int;              (** The inode number of the current file *)
           name: string;                    (** File name. *)
           buf: Buffer.t;
           follow_deletes: bool;            (** close and reopen the file if it is
                                                deleted and recreated. *)
           signal_on_truncate_or_delete: bool;(** raise exception File_truncated_or_deleted if
                                                the inode changed or the size of the file
                                                is less than pos*)
           close_on_eof: bool;              (** whether to close the file on EOF.
                                                Useful for NFS filesystems that
                                                don't promptly notify you when files
                                                change unless you close and reopen *)
           null_hack: [ `Off | `Retry_then_fail | `Retry ]; (** Which null_hack_mode to
                                                           use *)
           mutable null_retries: int;       (** number of times we've tried to
                                                reread this file in response to a
                                                given null reading. *)
           eprint_nulls : bool;             (** whether to print a warning message
                                                whenever we hit nulls *)
         }

type error_type = Null_retry | Too_many_nulls | Exception of string * Exn.t with sexp_of;;

type result =
  | Success of lnum * string
  | Nothing_available
  | Error of error_type
  | Fatal_error of string * Exn.t
with sexp_of;;

(** Open a linebuffer from the passed filename. *)
let create ?(pos = Pos_int.zero) ?(close_on_eof=false)
    ?(null_hack = `Off) ?(eprint_nulls = false)
    ?(follow_deletes=false) ?(signal_on_truncate_or_delete=false) fname =
  let file = In_channel.create ~binary:true fname in
  let { Unix.st_ino = inode; _ } = Unix.fstat (Unix.descr_of_in_channel file) in
  let lnum, pos =
    if Pos_int.(=) pos Pos_int.zero then
      Known 1, Pos_int.zero
    else if Pos_int.(>) pos Pos_int.zero then
      begin
        LargeFile.seek_in file pos;
        Unknown, pos
      end
    else
      invalid_argf "Linebuf.create: pos must be greater than or equal to 0, was %s"
        (Pos_int.to_string pos) ()
  in
  { file = file;
    pos = pos;
    lnum = lnum;
    name = fname;
    closed = false;
    buf = Buffer.create 0;
    close_on_eof = close_on_eof;
    follow_deletes = follow_deletes;
    signal_on_truncate_or_delete = signal_on_truncate_or_delete;
    inode = inode;
    null_hack = null_hack;
    null_retries = 0;
    eprint_nulls = eprint_nulls;
  }

(** Close the linebuf. *)
let close lbuf =
  if not lbuf.closed then
    (try In_channel.close lbuf.file with
    | Unix.Unix_error (Unix.EBADF,_, _) -> ());
  lbuf.closed <- true

let is_closed t = t.closed

exception File_truncated_or_deleted with sexp;;

let possibly_reopen lbuf =
  if lbuf.signal_on_truncate_or_delete
    && (Unix.stat lbuf.name).Unix.st_size < lbuf.pos
  then
    raise File_truncated_or_deleted;
  if lbuf.closed then begin
    lbuf.file <- open_in_bin lbuf.name;
    lbuf.closed <- false;
    LargeFile.seek_in lbuf.file lbuf.pos;
  end
;;

let reopen_if_deleted lbuf =
  try
    let {Unix.st_ino=inode; _} = Unix.stat lbuf.name in
    if lbuf.inode <> inode then begin
      try
        close lbuf;
        lbuf.file <- open_in_bin lbuf.name;
        lbuf.inode <- inode;
        lbuf.closed <- false;
        lbuf.pos <- Pos_int.zero;
        lbuf.lnum <- Known 1;
        `Success_reopening
      with exn ->
        `Error ("reopen_if_deleted: closing and reopening the file failed", exn)
    end else
      `Same_file
  with
    Unix.Unix_error (Unix.ENOENT, _, _) -> `No_such_file
  | exn -> `Error ("reopen_if_deleted: stat failed", exn)

exception Null_found with sexp;;
exception Too_many_null_retries with sexp;;

let try_read_lnum_verbose lbuf =
  try
    possibly_reopen lbuf;
    let line = input_line lbuf.file in
    begin match lbuf.null_hack with
    | `Off -> ()
    | `Retry | `Retry_then_fail as nhm ->
      if String.contains line '\000' then begin
        if lbuf.null_retries >= max_null_retries then begin
          match nhm with
          | `Retry_then_fail -> raise Too_many_null_retries
          | `Retry | `Off -> ()
        end else begin
          close lbuf;
          lbuf.null_retries <- lbuf.null_retries + 1;
          raise Null_found
        end
      end else
        lbuf.null_retries <- 0;
    end;
    lbuf.pos <- LargeFile.pos_in lbuf.file;
    let last_char =
      try LargeFile.seek_in lbuf.file (Pos_int.(-) lbuf.pos Pos_int.one); input_char lbuf.file
      with End_of_file ->
        failwith "Linebuf.try_read_lnum: unexpected EOF, file may have been truncated"
    in
    LargeFile.seek_in lbuf.file lbuf.pos;
    if last_char = '\n'
    then (* we're at the end of the line *)
      let lnum = lbuf.lnum in
      (match lbuf.lnum with
      | Unknown -> ()
      | Known i -> lbuf.lnum <- Known (i + 1));
      let line =
        if String.length line > 0 && String.nget line (-1) = '\r'
        then String.slice line 0 (-1) else line
      in
      if Buffer.length lbuf.buf > 0 then
        let oldline = Buffer.contents lbuf.buf in
        Buffer.clear lbuf.buf;
        Success (lnum,oldline ^ line)
      else
        Success (lnum,line)
    else
      begin
        Buffer.add_string lbuf.buf line;
        Nothing_available
      end
  with
  | Null_found ->
      if lbuf.eprint_nulls then eprintf "<<<<NULLFOUND:%s>>>>\n%!" lbuf.name;
      Error Null_retry
  | Too_many_null_retries -> Error Too_many_nulls
  | End_of_file ->
      begin
        try
          if lbuf.close_on_eof then close lbuf;
          if lbuf.follow_deletes || lbuf.signal_on_truncate_or_delete then
            match reopen_if_deleted lbuf with
            | `No_such_file
            | `Same_file -> Nothing_available
            | `Success_reopening ->
                if lbuf.signal_on_truncate_or_delete then raise File_truncated_or_deleted;
                Nothing_available
            | `Error (s, e) -> Error (Exception (s, e))
          else
            Nothing_available
        with
        | File_truncated_or_deleted -> raise File_truncated_or_deleted
        | exn -> Fatal_error ("error while handling end of file", exn)
      end
  | File_truncated_or_deleted -> raise File_truncated_or_deleted
  | exn -> Fatal_error ("main loop", exn)

let try_read_lnum lbuf =
  match try_read_lnum_verbose lbuf with
  | Nothing_available -> None
  | Error Null_retry ->
      None
  | Success (lnum, line) -> Some (lnum, line)
  | Error Too_many_nulls -> failwith "Too many null retries"
  | Error (Exception (e, exn)) ->
      Exn.reraisef exn "error in linebuf: %s" e ()
  | Fatal_error (e, exn) ->
      Exn.reraisef exn "fatal error in linebuf: %s" e ()

let try_read lbuf =
  Option.map (try_read_lnum lbuf) ~f:snd

let read_frequency = sec 0.01

let rec read lbuf =
  match try_read lbuf with
  | Some line -> line
  | None ->
      Time.pause read_frequency;
      read lbuf

let tail lbuf =
  let file_size = LargeFile.in_channel_length lbuf.file in
  if file_size = Pos_int.zero then () else
    begin
      lbuf.pos <- Pos_int.(-) file_size Pos_int.one;
      LargeFile.seek_in lbuf.file lbuf.pos;
      lbuf.lnum <- Unknown;
      ignore (read lbuf)
    end

let unsafe_tail lbuf =
  lbuf.pos <- LargeFile.in_channel_length lbuf.file;
  LargeFile.seek_in lbuf.file lbuf.pos;
  lbuf.lnum <- Unknown;
  ignore (try_read lbuf)

let name t =
  t.name
;;

let reset t =
  close t;
  t.file <- open_in_bin t.name;
  t.inode <- (Unix.fstat (Unix.descr_of_in_channel t.file)).Unix.st_ino;
  t.closed <- false;
  t.pos <- Pos_int.zero;
  t.lnum <- Known 1;
  LargeFile.seek_in t.file t.pos
;;
