open Core

module Assoc = List.Assoc

type t = (string, string) Assoc.t

let add      ~key ~data t = Assoc.add      t ~equal:String.equal key data
let find     ~key       t = Assoc.find     t ~equal:String.equal key
let find_exn ~key       t = Assoc.find_exn t ~equal:String.equal key
let mem      ~key       t = Assoc.mem      t ~equal:String.equal key
let remove   ~key       t = Assoc.remove   t ~equal:String.equal key

let import_from_sys ?default ~key t =
  match Sys.getenv key, default with
  | Some data, _ | None, Some data -> add t ~key ~data
  | _ -> t

let append_to_path ?(where=`Back) ~key ~data t =
  match find t ~key with
  | Some "" | None -> add t ~key ~data
  | Some old_path ->
    let data = match where with
      | `Front -> data ^ ":" ^ old_path
      | `Back  -> old_path ^ ":" ^ data
    in
    add t ~key ~data

let to_exec_env env = List.map env ~f:(fun (k,v) -> k ^ "=" ^ v)

let of_exec_env env =
  Array.to_list (Array.map env ~f:(String.lsplit2_exn ~on:'='))
