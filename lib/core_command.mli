(** purely functional command line parsing *)
(* NOTE: this module is intended to move to Core once it is a bit more battle hardened. *)

open Core.Std

(** composable command-line specifications *)
module Spec : sig

  (** [('main_in, 'main_out) t] is a type of composable command-line
      specifications.  Ultimately one forms a base command by
      combining a spec of type [('main, unit) t] a main function of
      type ['main].  Combinators in this library incrementally build
      up the type of main according to what command-line parameters it
      expects, so the resulting type of [main] is something like

        [arg1 -> ... -> argN -> unit]

      It may help to think of [('a, 'b) t] as a function space ['a -> 'b]
      embellished with information about
      {ul {- how to parse command line}
          {- what the command does and how to call it}
          {- how to auto-complete a partial command line}}
  *)

  type ('main_in, 'main_out) t

  (** {1 specification combinators} *)

  (** specification composition: [spec1 ++ spec2 ++ ... specN] is the way composition.
  *)
  val (++) : ('m1, 'm2) t -> ('m2, 'm3) t -> ('m1, 'm3) t

  (** [const v] inserts [v] as a hard-coded argument to the command. *)
  val const : 'a -> ('a -> 'm, 'm) t

  (** [step f] is a very useful combinator for modifying the behavior of other
      command line specifications.  Here are a couple examples of some of its many uses
      {ul
        {li {i introducing labeled arguments} {v
           step (fun m v -> m ~foo:v) ++ flag "-foo" no_arg : (foo:bool -> 'm, 'm) t
        v}}
        {li {i prompting for missing values} {v
           step (fun m user ->
             match user with
             | Some user -> m user
             | None -> print_string "enter username: "; m (read_line ())
           )
           ++ flag "-user" (optional string) ~doc:"USER who to frobnicate"
            : (string -> 'm, 'm) t
        v}}
      }

  *)
  val step : ('m1 -> 'm2) -> ('m1, 'm2) t

  (** {1 argument types} *)

  type 'a arg_type (** the type of a command line argument *)

  (** an argument type includes information about how to parse values of that type
      from the command line, and (optionally) how to auto-complete partial arguments of
      that type via bash's programmable TAB-completion.  In addition to the argument
      prefix, autocompletion also has access to any previously parsed arguments in the
      form of a heterogeneous map into which previously parsed arguments may register
      themselves by providing a Hmap.Key using the ~key argument to [arg_type].
   *)
  val arg_type :
    ?complete:(Hmap.t -> part:string -> string list)
    -> ?key:'a Hmap.Key.t
    -> (string -> 'a)
    -> 'a arg_type

  val string    : string arg_type
  val int       : int    arg_type
  val float     : float  arg_type
  val bool      : bool   arg_type
  val date      : Date.t arg_type
  val time_span : Time.Span.t arg_type
  val file      : string arg_type (* with bash autocompletion *)

  (** {1 flag specifications} *)

  type 'a flag (** a flag specification *)

  (** [flag name spec ~doc] specifies a command that, among other things, takes
      a flag named [name] on its command line.  [doc] indicates the meaning of
      the flag.

      NOTE: the [doc] for a flag which takes an argument should be of the
      form [arg_name ^ " " ^ description] where [arg_name] describes the
      argument and [description] describes the meaning of the flag.

      NOTE: flag names must not contain underscores.  Use dashes instead.
  *)
  val flag : ?aliases:string list -> string -> 'a flag -> doc:string -> ('a -> 'm, 'm) t

  (** required flags must be passed exactly once *)
  val required : 'a arg_type -> 'a flag

  (** optional flags may be passed at most once *)
  val optional : 'a arg_type -> 'a option flag

  (** [optional_with_default] flags may be passed at most once, and
      default to a given value *)
  val optional_with_default : 'a -> 'a arg_type -> 'a flag

  (** [listed] flags may be passed zero or more times *)
  val listed : 'a arg_type -> 'a list flag

  (** [no_arg] flags may be passed at most once.  The boolean returned
      is true iff the flag is passed on the command line *)
  val no_arg : bool flag

  (** [no_arg_register ~key] is like [no_arg], but registers in
      the auto-completion environment *)
  val no_arg_register : key:unit Hmap.Key.t -> bool flag

  (** [escape] flags may be passed at most once.  They cause the command
      line parser to abort and pass through all remaining command line
      arguments as the value of the flag. *)
  val escape : string list option flag

  (** {1 anonymous argument specifications} *)

  type 'a anons (** a specification of some number of anonymous arguments *)

  (** [anon spec] specifies a command that, among other things, takes
      the anonymous arguments specified by [spec]. *)
  val anon : 'a anons -> ('a -> 'm, 'm) t

  (** [(name %: typ)] specifies a required anonymous argument of type [typ].
      The [name] is mentioned in the generated help for the command. *)
  val (%:) : string -> 'a arg_type -> 'a anons

  (** [(sequence name typ)] specifies a sequence of anonymous arguments
      of type [typ]. The [name] is mentioned in the generated help for
      the command. *)
  val sequence : string -> 'a arg_type -> 'a list anons

  (** [(maybe anons)] indicates that some anonymous arguments are optional *)
  val maybe : 'a anons -> 'a option anons

  (** [(maybe_with_default default anons)] indicates an optional anonymous
      argument with a default value *)
  val maybe_with_default : 'a -> 'a anons -> 'a anons

  (** [t2], [t3], and [t4] each concatenate multiple anonymous argument
      specs into a single one. The purpose of these combinators is to allow
      for optional sequences of anonymous arguments.  Consider a command with
      usage

        main.exe FOO [BAR BAZ]

      where the second and third anonymous arguments must either both
      be there or both not be there.  This can be expressed as

        [t2 ("FOO" %: foo) (maybe (t2 ("BAR" %: bar) ("BAZ" %: baz)))]

      Sequences of 5 or more anonymous arguments can be built up using
      nested tuples

        maybe (t3 a b (t3 c d e))
  *)

  val t2 : 'a anons -> 'b anons -> ('a * 'b) anons

  val t3 : 'a anons -> 'b anons -> 'c anons -> ('a * 'b * 'c) anons

  val t4 : 'a anons -> 'b anons -> 'c anons -> 'd anons -> ('a * 'b * 'c * 'd) anons

  (** {1 various internal values} *)

  val help : (string Lazy.t -> 'm, 'm) t (* the help text for this command *)
  val path : unit -> (string list   -> 'm, 'm) t (* the subcommand path of this command *)
  val args : (string list -> 'm, 'm) t (* the arguments passed to this command *)

  (** specifies an exact usage message for a sequence of anonymous strings *)
  val ad_hoc : usage_arg:string -> string list anons
end

type t (** commands which can be built up into a hierarchy of subcommands *)

(** [basic ~summary ?readme spec main] is a basic command that executes
    a function [main] which is passed parameters parsed from the command
    line according to [spec]. [summary] is to contain a short one-line
    description of its behavior.  [readme] is to contain any longer
    description of its behavior that will go on that commands' help screen.
*)
val basic :
  summary:string
  -> ?readme:(unit -> string)
  -> ('main, unit) Spec.t
  -> 'main
  -> t

(** [group ~summary subcommand_alist] is a compound command with named
    subcommands, as found in [subcommand_alist].  [summary] is to contain
    a short one-line description of the command group.  [readme] is to
    contain any longer description of its behavior that will go on that
    command's help screen.

    NOTE: subcommand names must not contain underscores.  Use dashes instead.
*)
val group : summary:string -> ?readme:(unit -> string) -> (string * t) list -> t

(** run a command against [Sys.argv] *)
val run : ?version:string -> ?build_info:string -> ?argv:string list -> t -> unit

(** Should be used only by Core_extended.Command. *)
module Deprecated : sig
  module Spec : sig
    val no_arg : hook:(unit -> unit) -> bool Spec.flag
    val escape : hook:(string list -> unit) -> string list option Spec.flag
  end
  val summary : t -> string
  val help_recursive : 
    cmd:string
    -> with_flags:bool
    -> expand_dots:bool 
    -> t 
    -> string
    -> (string * string) list
  val run : t 
  -> cmd:string 
  -> args:string list 
  -> is_help:bool
  -> is_help_rec:bool
  -> is_help_rec_flags:bool
  -> is_expand_dots:bool
  -> unit
  val get_flag_names : t ->  string list
  (*val autocomplete : t -> ('a -> string list) option*)
  (*val autocomplete_of_extended_autocomplete : (string list -> string list) -> Hmap.t -> part:string -> string list*)
end

