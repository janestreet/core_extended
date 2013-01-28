open Core.Std

type return_type =
  | Ok
  | Changed of string * string
  | Abort

type file_state = {
    path : string;
    stat : Unix.stats;
  }

let record_state filepath =
  { path = filepath;
    stat = Unix.stat filepath;
  }

(* the peer is an unlocked shadow copy of the master which will then
   atomically replace the master if the user function completes ok
   and the master has not changed since the peer was created
*)

let peer_name master_state =
  Filename.temp_file
    ~perm:master_state.stat.Unix.st_perm
    ~in_dir:(Filename.dirname master_state.path)
    (Filename.basename master_state.path)
    ("." ^ Pid.to_string (Unix.getpid ()))

let copy_master master_state =
  let peer_path = peer_name master_state
  in
  Shell.cp ~overwrite:true ~perm:0o0644 (master_state.path) (peer_path);
  peer_path

let replace_master master_state peer_path =
  let update_file = (master_state.path ^ ".update")
  in
  protectx (Unix.openfile update_file
              ~mode:[Unix.O_WRONLY;Unix.O_EXCL;Unix.O_CREAT]
              ~perm:0o0644)
    ~f:(fun _ ->
      if master_state.stat.Unix.st_mtime <> (Unix.stat(master_state.path)).Unix.st_mtime then
        Changed ( (sprintf
                     "%s changed while you were editing %s"
                     master_state.path
                     peer_path) ,
                 peer_path)
      else
        ( Unix.rename ~src:peer_path ~dst:master_state.path; Ok )
      )

      ~finally:(fun fd -> Unix.close fd; Unix.unlink update_file)

let atomic_edit ~f master_path =
  let master_state = record_state master_path
  in
  let peer_path = copy_master master_state
  in
  match (f (peer_path))
  with
  | `Ok -> replace_master master_state peer_path
  | `Abort -> Abort
