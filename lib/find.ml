open Core.Std

type file_info = string * Unix.stats

module Options = struct
  type error_handler =
    | Ignore
    | Print
    | Raise
    | Handle_with of (string -> unit)

  type t = {
      max_depth: int option;
      follow_links: bool;
      on_open_errors: error_handler;
      on_stat_errors: error_handler;
      filter: (file_info -> bool) option;
      skip_dir: (file_info -> bool) option
    }

  let default = {
      max_depth = None;
      follow_links = false;
      on_open_errors = Raise;
      on_stat_errors = Raise;
      filter = None;
      skip_dir = None
    }

  let ignore_errors = { default with
      on_open_errors = Ignore;
      on_stat_errors = Ignore
    }
end
module O = Options

type t = {
  options: Options.t;
  already_seen: ((int * int), unit) Hashtbl.t;  (* device num * inode *)
  mutable to_visit: (string * int) list;  (* dir to traverse and the depth it is at *)
  mutable current_dir: string;
  mutable current_handle: Unix.dir_handle;
  mutable depth: int;
  mutable closed: bool;
}


let rec open_next_dir t =
  match t.to_visit with
  | [] -> None
  | (dir_name, depth) :: rest ->
      try
        t.to_visit <- rest;
        t.current_handle <- Unix.opendir dir_name;
        t.current_dir <- dir_name;
        t.depth <- depth;
        Some ()
      with
      | e ->
          match t.options.O.on_open_errors with
          | O.Ignore -> open_next_dir t
          | O.Raise -> raise e
          | O.Handle_with f ->
              f dir_name;
              open_next_dir t
          | O.Print ->
              Printf.eprintf "unable to open %s - %s\n" dir_name (Exn.to_string e);
              open_next_dir t
;;

let closedir t =
  try
    Unix.closedir t.current_handle;
  with
  | Unix.Unix_error _ -> ()
;;

let close t =
  if not t.closed then
    begin
      t.closed <- true;
      closedir t;
      Hashtbl.clear t.already_seen;
      t.to_visit <- [];
    end
;;

(* returns the next file from the conceptual stream and updates the state of t - this
  is the only way that t should ever be updated *)
let rec next t =
  assert (not t.closed);
  let stat fn =
    try
      let stat = if t.options.O.follow_links then Unix.stat else Unix.lstat in
      Some (fn, stat fn)
    with
    | e ->
        match t.options.O.on_stat_errors with
        | O.Ignore -> None
        | O.Raise -> raise e
        | O.Handle_with f ->
            f fn;
            None
        | O.Print ->
            Printf.eprintf "unable to stat %s - %s\n" fn (Exn.to_string e);
            None
  in
  let is_new (fn,stats) =
    let uid = (stats.Unix.st_dev, stats.Unix.st_ino) in
    match Hashtbl.find t.already_seen uid with
    | Some () -> None
    | None ->
        Hashtbl.replace t.already_seen ~key:uid ~data:();
        Some (fn,stats)
  in
  let handle_dirs ((fn,stats) as info) =
    if Option.apply ~f:t.options.O.skip_dir info = Some true then
      None
    else
    (* if this is a directory we need to decide if we will be traversing into it
      later... *)
    let visit () =
      t.to_visit <- (fn, (t.depth + 1)) :: t.to_visit
    in
    if stats.Unix.st_kind = Unix.S_DIR then
      begin
        match t.options.O.max_depth with
        | None -> visit ()
        | Some max_depth ->
            if t.depth < max_depth then visit () else ()
      end;
    Some (fn, stats)
  in
  let filter file =
    match t.options.O.filter with
    | None -> Some file
    | Some f -> if f file then Some file else None
  in
  let dirent =
    try
      `Dirent (Unix.readdir t.current_handle)
    with
    | e ->
        closedir t;
        match e with
        | End_of_file -> `End_of_directory
        | e -> `Readdir_error e
  in
  match dirent with
  | `End_of_directory ->
      begin
        match open_next_dir t with
        | Some () -> next t
        | None ->
            close t;
            None
      end
  | `Readdir_error e -> raise e
  | `Dirent "." | `Dirent ".." -> next t
  | `Dirent basename ->
      let fn = Filename.concat t.current_dir basename in
        (* each function in this bind returns None if the file should be skipped, and
           Some f i if it thinks it's ok to emit - possibly updating the state or
           transforming f along the way *)
      let (>>=) = Option.bind in
      let skip =
        try
          stat fn >>= is_new >>= handle_dirs >>= filter
        with
        | e ->
            closedir t;
            raise e
      in
      match skip with
      | None -> next t
      | file -> file
;;

let create ?(options=Options.default) dir =
  let dh = Unix.opendir dir in
  {
    options = options;
    already_seen = Hashtbl.Poly.create () ~size:11;
    to_visit = [];
    current_dir = dir;
    current_handle = dh;
    depth = 0;
    closed = false;
  }
;;

let iter t ~f =
  let rec loop () =
    match next t with
    | None -> ()
    | Some file ->
        f file;
        loop ()
  in
  loop ()
;;

let fold t ~init ~f =
  let rec loop acc =
    match next t with
    | None -> acc
    | Some file -> loop (f acc file)
  in
  loop init
;;

let to_list t =
  List.rev (fold t ~init:[] ~f:(fun acc file -> file :: acc))
;;

let find_all ?options dir =
  let t = create ?options dir in
  to_list t
;;
