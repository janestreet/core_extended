open Core.Std

type t = (string, string) List.Assoc.t

(* These functions are similar to their List.Assoc counterparts *)
val add             : key:string -> data:string -> t -> t
val find            : key:string                -> t -> string option
val find_exn        : key:string                -> t -> string
val mem             : key:string                -> t -> bool
val remove          : key:string                -> t -> t

(* Attempt to import a value in from the system environment
   If no system environment value is found and a default is provided, use that instead *)
val import_from_sys : ?default:string -> key:string -> t -> t

(* Appends a new value to the end of a path value. If [where] is `Front,
   add it to the front of the path specification. if it is `Back, add it to
   the back. `Back is the default. eg:

   [ add_to_path ~where:`Back [("PATH", "/bin:/usr/bin")] "PATH" "/usr/local/bin"
   = [("PATH", "/bin:/usr/bin:/usr/local/bin")] ]

   [ add_to_path ~where:`Front [("PATH", "/bin:/usr/bin")] "PATH" "/home/rprije/bin"
   = [("PATH", "/home/rprije/bin:/bin:/usr/bin")] ] *)
val append_to_path : ?where:[`Front|`Back] -> key:string -> data:string -> t -> t

(* Converts the Env.t into a value suitable for passing to Unix.exec and friends *)
val to_exec_env : t -> string array

(* Converts the a Unix.environment style environment into a Env.t *)
val of_exec_env : string array -> t
