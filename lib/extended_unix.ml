open Core.Std
open Unix

external raw_fork_exec :
  stdin : File_descr.t
  -> stdout : File_descr.t
  -> stderr : File_descr.t
  -> ?working_dir : string
  -> ?setuid : int
  -> ?setgid : int
  -> ?env : (string) array
  -> string
  -> string array
  -> Pid.t
  =  "extended_ml_spawn_bc" "extended_ml_spawn"

module Env = struct
  open String.Map
  type t = string String.Map.t

  let empty = empty

  let get ()  =
    Array.fold  (Unix.environment ())
      ~init:empty
      ~f:(fun env str ->
        match String.lsplit2 ~on:'=' str with
        | Some (key,data) -> add ~key ~data env
        | None ->
          failwithf
            "extended_unix.Env.get %S is not in the form of key=value"
            str
            ())

  let add ~key ~data env =
    if String.mem key '=' then
      failwithf "extended_unix.Env.add:\
  variable to export in the environment %S contains an equal sign"
        key
        ()
    else if String.mem key '\000' then
      failwithf "extended_unix.Env.add:\
  variable to export in the environment %S contains an null character"
        key
        ()
    else if String.mem data '\000' then
      failwithf "extended_unix.Env.add:\
  value (%S) to export in the environment for %S contains an null character"
        data
        key
        ()
    else
      add ~key ~data env

  let to_string_array env =
    String.Map.to_alist env
    |! List.map ~f:(fun (k,v) -> k^"="^v)
    |! List.to_array
end

let fork_exec
    ?(stdin=Unix.stdin)
    ?(stdout=Unix.stdout)
    ?(stderr=Unix.stderr)
    ?(path_lookup=true)
    ?env
    ?working_dir
    ?setuid
    ?setgid
    prog
    args
    =
  let env = Option.map env
    ~f:(fun e ->
      let init,l = match e with
        | `Extend  l ->
          Env.get (),l
        | `Replace l ->
          Env.empty,l
      in
      List.fold_left l
        ~init
        ~f:(fun env (key,data) -> Env.add ~key ~data env)
      |! Env.to_string_array)

  and full_prog =
    if path_lookup then
      match Shell__core.which prog with
      | Some s -> s
      | None -> failwithf "fork_exec: Process not found %s"
        prog
        ()
    else
      prog
  in
  raw_fork_exec
    ~stdin
    ~stdout
    ~stderr
    ?working_dir
    ?setuid
    ?setgid
    ?env
    full_prog
    (Array.of_list (prog::args))

external seteuid : int -> unit = "extended_ml_seteuid"
external setreuid : uid:int -> euid:int -> unit = "extended_ml_setreuid"
external gettid : unit -> int = "extended_ml_gettid"

type statvfs = {
  bsize: int;                           (** file system block size *)
  frsize: int;                          (** fragment size *)
  blocks: int;                          (** size of fs in frsize units *)
  bfree: int;                           (** # free blocks *)
  bavail: int;                          (** # free blocks for non-root *)
  files: int;                           (** # inodes *)
  ffree: int;                           (** # free inodes *)
  favail: int;                          (** # free inodes for non-root *)
  fsid: int;                            (** file system ID *)
  flag: int;                            (** mount flags *)
  namemax: int;                         (** maximum filename length *)
} with sexp, bin_io

(** get file system statistics *)
external statvfs : string -> statvfs = "statvfs_stub"

(** get load averages *)
external getloadavg : unit -> float * float * float = "getloadavg_stub"

module Extended_passwd = struct
  open Passwd

  let of_passwd_line_exn s =
    match String.split s ~on:':' with
    | name::passwd::uid::gid::gecos::dir::shell::[] ->
        { name = name;
          passwd = passwd;
          uid = Int.of_string uid;
          gid = Int.of_string gid;
          gecos = gecos;
          dir = dir;
          shell = shell
        }
    | _ -> failwithf "of_passwd_line: failed to parse: %s" s ()
  ;;
  let of_passwd_line s = Option.try_with (fun () -> of_passwd_line_exn s) ;;

  let of_passwd_file_exn fn =
    Exn.protectx (In_channel.create fn)
      ~f:(fun chan ->
        List.map (In_channel.input_lines chan) ~f:of_passwd_line_exn)
      ~finally:In_channel.close
  ;;

  let of_passwd_file f = Option.try_with (fun () -> of_passwd_file_exn f) ;;
end
