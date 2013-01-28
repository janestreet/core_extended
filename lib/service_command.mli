(** [command ~lock_file ~name main] creates a command group for starting,
    stopping, restarting, and checking the status of a daemon (the code
    for which lives in the argument [main]).  This family of commands
    uses a [lock_file] to ensure that two instances of the daemon don't
    run concurrently on the same machine.  The [name] parameter is used
    to name the daemon in the command help and various error messages.

    [lock_file] should be on the local file system (for example in /var/tmp)
    since file locking doesn't work over NFS.
*)
val command : lock_file:string -> name:string -> (unit -> unit) -> Command.t

