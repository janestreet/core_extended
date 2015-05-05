(** Code to test the effect of exceptions happening in strategic
    places in daemons.

    In order to use this module one defines a list
    of (mnemonic, exception) pairs

    E.G.

    [("M.f: invalid arg", Invalid_argument "foo");
     ("Z.f: sys error", Sys_error "bar");
     ("R.z: failure", Failure "baz")]

    And one passes this list to create. Then one places calls to
    Exception_check.maybe_raise <name>, in important parts of one's
    code. When the code is run, it will listen on [listen_port], and
    one can connect with netcat and type a name, which will
    cause that exception to be raised on the next call to
    [Exception_check.maybe_raise]. *)

(** create should not be called more than once *)
val create : ?listen_port:int -> (string * exn) list -> unit

(** [maybe_raise name] if the exception associated with any name in [name]
    has been triggered, then raise it, otherwise do nothing. Only the
    first exception in the list will be raised. This function is
    thread safe. *)
val maybe_raise : string list -> unit
