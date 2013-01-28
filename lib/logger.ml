open Core.Std

type message = [ `Fatal | `Error | `Warn | `Info | `Debug ] * string
type size = [ `Kb of int64 | `Mb of int64 | `Unlimited ]
type 'a t = {
    dirname:string;
    basename:string;
    max_size:size;
    mode:Unix.file_perm;
    max_archives:[`Unlimited | `Max of int];
    full_callback:(string Squeue.t -> unit) option;
    message_to_string:('a -> string);
    mutable filter:('a -> bool) option;
    mutable oc:out_channel option;
    q:string Squeue.t;
  }
type default_t = message t


let queuelength = 1000000
let wasfull = queuelength - 1


let filename log =
  Filename.concat log.dirname log.basename
;;


let filter t filterfun =
  t.filter <- Some filterfun
;;


let clear_filter t =
  t.filter <- None
;;


let close log =
  match log.oc with
  | None -> ()
  | Some oc ->
    begin
      (* really try to close the log, but at least force the bad file pointer
        out of scope so we don't use it again *)
      close_out_noerr oc;
      log.oc <- None
    end
;;


let too_large log =
  try
    let size = (Unix.stat (filename log)).Unix.st_size in
    match log.max_size with
    | `Kb kb when (Int64.(/) size 1024L) >= kb -> true
    | `Mb mb when Int64.(/) (Int64.(/) size 1024L) 1024L >= mb -> true
    | _ -> false
  with
  | _ ->
    (* any error stating a file means that we should give up on ever using it *)
    true
;;


let roll log =
  let log_number log c =
    (filename log) ^ "." ^ (string_of_int c)
  in
  let filelist = (Array.to_list (Sys.readdir log.dirname)) in
  let files = List.filter_map filelist ~f:(fun f ->
    let basename = log.basename in
    if String.is_prefix f ~prefix:basename then
      Some (f, int_of_string (snd (String.rsplit2_exn f ~on:'.')))
    else None)
  in
  let files = List.sort ~cmp:(fun x y -> ascending (snd x) (snd y)) files in
  let max_archives =
    match log.max_archives with
    | `Max m -> m
    | `Unlimited -> max_int;
  in
  let rec roll files =
    match files with
    | [] -> ()
    | (_f, c)::rest ->
      begin
        roll rest;
        if c >= max_archives - 1 then Unix.unlink (log_number log c)
        else Unix.rename ~src:(log_number log c) ~dst:(log_number log (c + 1))
      end
  in
  close log;
  roll files;
  Unix.rename ~src:(filename log) ~dst:(log_number log 0);
;;


let reopen_log log =
  let filename = filename log in

  (* close the log if it happens to be open *)
  close log;

  (* roll the file if necessary *)
  if too_large log then roll log;

  (* return a file pointer to the end of the file *)
  let oc = open_out_gen [Open_append; Open_creat; Open_wronly] log.mode
             filename in
  log.oc <- Some oc;
  oc
;;


let timestamp () =
  Time.to_string (Time.now ())
;;


let level_to_string level =
  match level with
  | `Fatal -> "FATAL"
  | `Error -> "ERROR"
  | `Warn -> "warn "
  | `Info -> "info "
  | `Debug -> "debug"
;;


let log t msg =
  let really_log () =
    let success = Squeue.push_or_drop t.q (t.message_to_string msg) in
    if not success then
      match t.full_callback with
      | Some callback -> callback t.q
      | None -> ()
  in
  match t.filter with
  | None -> really_log ()
  | Some f -> if f msg then really_log ()
;;


let maybe_log topt msg =
  match topt with
  | None -> ()
  | Some t -> log t msg
;;


let create ?(max_size=`Mb 50L)
           ?(mode=0o644)
           ?(max_archives=`Max 4)
           ?full_callback
           ?filter
           ~message_to_string
           filename =
  let log = {
    dirname = Filename.dirname filename;
    basename = Filename.basename filename;
    max_size = max_size;
    mode = mode;
    max_archives = max_archives;
    full_callback = full_callback;
    message_to_string = message_to_string;
    filter = filter;
    q = Squeue.create queuelength;
    oc = None
  }
  in
  let output msg =
    let oc =
      begin
        match log.oc with
        | None -> reopen_log log
        | Some oc -> if too_large log then reopen_log log else oc
      end
    in
    output_string oc msg;
    Pervasives.flush oc;
  in
  ignore (Thread.create (fun () ->
    while true do
      try
        let (msg, length) = Squeue.lpop log.q in
        if length = wasfull then
          output "Logging queue was full - some logs potentially lost\n";
        output msg
      with
      | _ ->
        begin
          Time.pause (sec 0.1);
          close log
        end
    done) ());

  (* register a function to flush the log when the program exits *)
  at_exit (fun () ->
    while Squeue.length log.q > 0 do
      Thread.yield ()
    done);

  log
;;


let default_message_to_string ((level, msg):message) =
  let finalmsg = Printf.sprintf "%s [%s]: %s\n" (timestamp ())
    (level_to_string level) msg in
  finalmsg
;;


let default_filter (msgtype, _msg) =
  match msgtype with
  | `Debug -> false
  | _ -> true
;;


let create_default ?max_size
           ?mode
           ?max_archives
           ?full_callback
           filename =
  create ?max_size ?mode ?max_archives ~message_to_string:(default_message_to_string)
    ?full_callback ~filter:(default_filter) filename;
;;
