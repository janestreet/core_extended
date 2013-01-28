(** command-line parsing with hierarchical sub-commands *)

open Core.Std

exception Invalid_arguments of string list

module Flag : sig

  (** type of flags to a command with accumulator type ['a] *)
  type 'a t
  val name : _ t -> string

  (** Template for flag-creation functions *)
  type ('a, 'b) create =
    string
    -> ?aliases:string list
    -> ?full_flag_required:bool
    -> ('b)
    -> doc:string
    -> 'a t

  val of_arg : Arg.t -> unit t

  (** {6 Flag handling without an accumulator} *)

  val noarg      : (unit, unit -> unit)   create
  val string     : (unit, string -> unit) create
  val int        : (unit, int -> unit)    create
  val float      : (unit, float -> unit)  create
  val bool       : (unit, bool -> unit)   create

  val gen : (string -> 'gen) -> (unit, 'gen -> unit) create

  val set_string     : (unit, string ref)        create
  val set_string_opt : (unit, string option ref) create
  val set_int        : (unit, int ref)           create
  val set_int_opt    : (unit, int option ref)    create
  val set_float      : (unit, float ref)         create
  val set_float_opt  : (unit, float option ref)  create
  val set_date       : (unit, Date.t ref)        create
  val set_date_opt   : (unit, Date.t option ref) create
  val set            : (unit, bool ref)          create
  val clear          : (unit, bool ref)          create

  val set_gen : (string -> 'gen) -> (unit, 'gen ref) create
  val set_gen_opt : (string -> 'gen) -> (unit, 'gen option ref) create

  (** {6 flag handling meant for use with immutable accumulator} *)

  val noarg_acc  : ('a, 'a -> 'a) create
  val string_acc : ('a, 'a -> string -> 'a) create
  val int_acc    : ('a, 'a -> int -> 'a) create
  val bool_acc   : ('a, 'a -> bool -> 'a) create
  val float_acc  : ('a, 'a -> float -> 'a) create
  val date_acc   : ('a, 'a -> Date.t -> 'a) create
  (** [rest f]: a flag that signals the end of flag processing.  all remaining arguments
      are passed to the [f] *)
  val rest_acc   : ('a, 'a -> string list -> 'a) create

  val gen_acc : (string -> 'gen) -> ('a, 'a -> 'gen -> 'a) create

  (** {6 flag handling meant for use with mutable accumulator} *)

  val noarg_mut  : ('a, 'a -> unit) create
  val arg_mut    : ('a, 'a -> string -> unit) create
  val string_mut : ('a, 'a -> string -> unit) create
  val int_mut    : ('a, 'a -> int -> unit) create
  val bool_mut   : ('a, 'a -> bool -> unit) create
  val float_mut  : ('a, 'a -> float -> unit) create
  val date_mut   : ('a, 'a -> Date.t -> unit) create
  (** [rest f]: a flag that signals the end of flag processing.  all remaining arguments
      are passed to the [f] *)
  val rest_mut : ('a, 'a -> string list -> unit) create

  val gen_mut : (string -> 'gen) -> ('a, 'a -> 'gen -> unit) create

  (** {2 Deprecated } This is the old deprecated interface to Flag *)
  module Action : sig
    (** ['accum]-mutating action to perform when processing a flag *)
    type 'accum t

    (** an action for a flag that takes no additional argument *)
    val noarg : ('accum -> unit) -> 'accum t

    (** an action for a flag that takes an additional string argument *)
    val arg : ('accum -> string -> unit) -> 'accum t

    (** [rest f]: an action for a flag that signals the end of flag
        processing.  all remaining arguments are passed to the [f] *)
    val rest : ('accum -> string list -> unit) -> 'accum t
      (** [rest f]: an action for a flag that signals the end of flag
            processing.  all remaining arguments are passed to the [f] *)

    (** an action for a flag that takes an additional int argument *)
    val int : ('accum -> int -> unit) -> 'accum t

    (** an action for a flag that takes an additional bool argument *)
    val bool : ('accum -> bool -> unit) -> 'accum t

    (** an action for a flag that takes an additional float argument *)
    val float : ('accum -> float -> unit) -> 'accum t
  end

  (* The [name] must not contain any underscores: dashes should be used instead.  Whether
     or not underscores should be interpreted as dashes on the command line can be
     controlled by the [allow_underscores_and_dashes] argument to [run]. *)
val create :
  ?aliases:string list
  -> ?full_flag_required:bool
  -> name:string
  -> doc:string
  -> 'a Action.t
  -> 'a t

  (** [lift t ~project] transforms a flag with accumulator type ['a]
      into a flag with a more informative accumulator type ['b]
      provided that [project x] returns a pair consisting of
        1. a ['a]-value extracted from the ['b]-value [x], and
        2. a function for rebuilding a modified ['b]-value from
           the modified ['a]-value resulting from processing the flag.

      The intended use pattern for [lift] is when ['b] is a record type
      with a field [foo] of type ['a] and [project] is
        [fun r -> (r.foo, (fun foo' -> { r with foo = foo' }))]
  *)
  val lift : 'a t -> project:('b -> 'a * ('a -> 'b)) -> 'b t

  val lift_unit : unit t -> 'any t
end

module Shared_flags : sig
  type 'a t = {
    flags : unit Flag.t list;
    get : unit -> 'a;
  }
end


(* Represents the expected command line arguments for a program. *)
type t

module Autocomplete : sig
  type t = string list -> string list
  (* An autocomplete function gets the list of command line arguments up to the
     point that TAB was pressed.  It returns the list of completion
     possibilities.

     It is used to allow bash to query suggestions from the program that uses
     the Command module.  Autocompletion works by checking some environment
     variables and suggesting completion options. If the environment
       COMMAND_OUTPUT_INSTALLATION_BASH is set,
    a bash script for the autocompletion is printed to stdout.

    The following bash function can be used by any program using the Command
    module.
{v
function js-install-command-autocompletion \{
    while (( $# > 0 )); do
        tmp=$(mktemp)
        COMMAND_OUTPUT_INSTALLATION_BASH= $1 >$tmp
        source $tmp
        rm -f $tmp
        shift
    done
\}
v}

To activate the feature in the programs foo and bar, then you need to write in
yor bashrc:

{v
js-install-command-autocompletion foo bar
v}
)

  *)

  module Bash_action : sig
    type t =
    [
        | `Alias
        | `Arrayvar
        | `Binding
        | `Builtin
        | `Command
        | `Directory
        | `Disabled
        | `Enabled
        | `Export
        | `File
        | `Function
        | `Group
        | `Helptopic
        | `Hostname
        | `Job
        | `Keyword
        | `Running
        | `Service
        | `Setopt
        | `Shopt
        | `Signal
        | `Stopped
        | `User
        | `Variable
    ]
  end

  (** [create actions command_line] gets the completion suggestions that
      bash's compgen would generate for the selected [actions] and the
      last word of the [command_line]. *)
  val bash : Bash_action.t list -> t
end

(**
    [create ~autocomplete ~summary ~usage_arg ~init ~flags ~final main]
    constructs a base command from the following data:
    {ul
        {li ['accum] a mutable accumulator type for gathering arguments }
        {li ['args] a composite argument type for the command, build from ['accum] }
        {li [autocomplete] an optional argument defining a bash autocompletion
            function for the base command. }
        {li [summary] a short description of what the command does }
        {li [readme] a longer description of what the command does }
        {li [usage_arg] an abbreviation of the arguments it expects }
        {li [init] a function that creates an mutable
              accumulator of type ['accum] }
        {li [flags] a list of command line flags together with their
              associated accumulator-mutating actions }
        {li [final] a function that constructs the final argument
              structure of type ['args] from the accumulated arguments.
              The second argument to the function is the list of all
              annonymous arguments.  This function should raise an
              exception with some explanation if it is unable to
              construct a complete value of type ['args]. }
        {li [help] an optional function that will be called to generate help
                for a command instead of the standard help }
        {li [main] the main function, parameterized by the argument structure }
    }
*)

val create : 
  ?autocomplete:Autocomplete.t
  -> ?readme:(unit -> string) 
  -> summary:string
  -> usage_arg:string
  -> init:(unit -> 'accum)
  -> flags:'accum Flag.t list
  -> final:('accum -> string list -> 'argv)
  -> ('argv -> unit)
  -> t

val create0 :
  ?autocomplete : Autocomplete.t
  -> ?readme : (unit -> string)
  -> summary : string
  -> usage_arg : string
  -> init : (unit -> 'accum)
  -> flags : ('accum Flag.t list)
  -> final : ('accum -> 'args)
  -> ('args -> unit)
  -> t

val create_no_accum :
  ?autocomplete : Autocomplete.t
  -> ?readme : (unit -> string)
  -> summary : string
  -> usage_arg : string
  -> flags : unit Flag.t list
  -> final : (string list -> 'args)
  -> ('args -> unit)
  -> t

val create_no_accum0 :
  ?autocomplete : Autocomplete.t
  -> ?readme : (unit -> string)
  -> summary : string
  -> usage_arg : string
  -> flags : unit Flag.t list
  -> (unit -> unit)
  -> t


(** [group ~summary [...; (name_i, t_i); ...]] is an aggregate command
    that dispatches to one of the named sub-commands.  A ["help"]
    sub-command will also be generated for the group.

    The name cannot contain underscores, however passing [allow_underscores=true] into run
    will parse underscores as dashes on the command line.
*)
val group
  :  summary:string
  -> ?readme:(unit -> string)
  -> (string * t) list
  -> t


type 'a with_run_flags
  =  ?version:string
  -> ?build_info:string
  (* Defaults to [Sys.argv]. *)
  -> ?argv:string list
  (* if true, unknown flags will be passed to the anon command handler *)
  -> ?allow_unknown_flags:bool
  (* if true, "-multi_arg_flag", will be handled the same as "-multi-arg-flag".  If
     false, an exception will be raised.  The default is true. *)
  -> ?allow_underscores:bool
  (* set to [true] when we expect the command to run as the result of calling a #!
     interpreter script. *)
  -> ?hash_bang_expand:bool
  -> ?post_parse:([ `Ok | `Error | `Help ] -> string list -> unit)
  -> t
  -> 'a

val run : unit with_run_flags

val get_expanded_argv : unit -> string list

val get_expanded_cmdline : unit -> string

module Version : sig
  (** Provides a ["version"] subcommand. *)
  (* Requiring [version] and [build_info] as arguments allows you to
     munge the strings before passing them to [command]. Also, passing
     in the strings instead of using Version_util directly prevents this
     module from being rebuilt constantly, I think(?). *)
  val command : ?version:string -> ?build_info:string -> unit -> t
end

(** This module is intended to help in using pa_fields to easily generate
   Command.t's when you have a record type each field of which you would
   like specified as a command line argument.

   An example is as follows:

{[module M = struct
  type t = {
    field1 : int;
    field2 : float;
    field3 : bool;
    field4 : string option;
  } with fields

  module A = Annotated_field

  let ann_fields = Fields.fold ~init:[]
    ~field1:(A.required ~doc:" documentation for field1")
    ~field2:(A.default 1.0 string_of_float ~doc:" documentation for field2")
    ~field3:(A.set ~doc:" documentation for field3")
    ~field4:(A.optional ~doc:" documentation for field4")

  let command = create
    ~summary:"summary"
    ~init:(fun () -> A.init ann_fields)
    ~usage_arg:""
    ~flags:(List.map ann_fields ~f:A.to_flag)
    ~final:(fun accum _anon_args ->
      let get of_string = A.get accum of_string in
      let get_opt of_string = A.get_opt accum of_string in
      Fields.map
        ~field1:(get int_of_string)
        ~field2:(get float_of_string)
        ~field3:(get bool_of_string)
        ~field4:(get_opt ident)
    )
    ~main:(fun _ -> assert false)]}
end
*)
module Annotated_field : sig
  type t

  (* "accum" in the sense of 'accum above *)
  type accum

  (* naming convention follows extended_arg *)
  val required :
    ?name:string
    -> t list
    -> doc:string
    -> (_, _) Fieldslib.Field.t
    -> t list

  val default :
    ?name:string
    -> 'field
    -> ('field -> string)
    -> t list
    -> doc:string
    -> (_, 'field) Fieldslib.Field.t
    -> t list

  val optional :
    ?name:string
    -> ?suppress_word_optional:bool
    -> t list
    -> doc:string
    -> (_, _ option) Fieldslib.Field.t
    -> t list

  val set :
    ?name:string
    -> t list
    -> doc:string
    -> (_, bool) Fieldslib.Field.t
    -> t list

  val clear :
    ?name:string
    -> t list
    -> doc:string
    -> (_, bool) Fieldslib.Field.t
    -> t list

  val list :
    ?name:string
    -> t list
    -> doc:string
    -> (_, _ list) Fieldslib.Field.t
    -> t list

  val init : t list -> accum

  val to_flag : t -> accum Flag.t

  val get :
    accum
    -> (string -> 'field)
    -> (_, 'field) Fieldslib.Field.t
    -> 'field

  val get_opt :
    accum
    -> (string -> 'field)
    -> (_, 'field option) Fieldslib.Field.t
    -> 'field option

  val get_list :
    accum
    -> (string -> 'field)
    -> (_, 'field list) Fieldslib.Field.t
    -> 'field list
end

module Flags_ext : sig

  type 'a setter =
  | No_arg of 'a
  | Arg of (string -> 'a)

  (* 'flag can either be a Flag.t or a Flag.t list *)
  type ('a, 'flag) t

  val flag : ( _, 'flag) t -> 'flag
  val get  : ('a, _) t -> 'a
  val is_set : (_, _) t -> bool

  (* this flag can occur at most once *)
  val create_optional :
    name:string
    -> doc:string
    -> 'a setter
    -> ('a option, unit Flag.t) t

  (* this flag can occur at most once *)
  val create :
    ?default:('a * ('a -> string))
    -> name:string
    -> doc:string
    -> 'a setter
    -> ('a, unit Flag.t) t

  (* this flag can occur 0/1 or more times *)
  val create_many :
    ?at_least_one:bool
    -> name:string
    -> doc:string
    -> 'a setter
    -> ('a list, unit Flag.t) t

  (* this flag can occur at most once *)
  (* name, doc, setter triples       *)
  val create_choice :
    ?default:('a * ('a -> string))
    -> (string * string * 'a setter) list
    -> ('a, unit Flag.t list) t

  val create_choice_optional :
    (string * string * 'a setter) list
    -> ('a option, unit Flag.t list) t

  val create_set :
    name:string
    -> doc:string
    -> (bool, unit Flag.t) t

  val create_rest :
    ?name:string
    -> doc:string
    -> unit
    -> (string list option, unit Flag.t) t
end

module Helpers : sig
  exception Found_anonymous_arguments
  val no_anons : 'a -> string list -> 'a
end

val of_core_command : Core_command.t -> t

