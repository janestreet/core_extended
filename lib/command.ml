open Core.Std
open Printf

exception Invalid_arguments of string list with sexp

(* compare command names, so that "help" is the last one *)
let subcommand_cmp a c =
  let is_help_cmd = function
    | s when String.is_prefix s ~prefix:"help" -> 1
    | _ -> 0
  in
  let is_help_cmd_diff = (is_help_cmd a) - (is_help_cmd c) in
  if is_help_cmd_diff = 0 then
    String.compare a c
  else
    is_help_cmd_diff

let subcommand_cmp_fst (a, _) (c, _) = subcommand_cmp a c

(* simple module for formatting two columns *)
module Columns = struct

  type t = (string * string) list

  let align pairs =
    match pairs with
    | [] -> []
    | (x, _) :: xs ->
      let left_col_len : int =
        List.fold_left
          (List.map ~f:(fun (a,_) -> String.length a) xs)
          ~f:(fun acc x -> max acc x)
          ~init:(String.length x)
      in
      let max_width =
        match Console.width () with
        | `Not_a_tty | `Not_available -> 80
        | `Cols cols -> cols
      in
      List.concat
        (List.map pairs ~f:(fun (cmd,desc) ->
          let desc_line_len = max 30 (max_width - 4 - left_col_len) in
          let desch,descl =
            match Extended_string.line_break desc ~len:desc_line_len with
            | h :: t -> (h, t)
            | [] -> assert false
          in
          let head = sprintf "%-*s  %s" left_col_len cmd desch in
          let tail =
            List.map descl ~f:(fun s -> sprintf "%-*s  %s" left_col_len " " s)
          in
          head :: tail))

  let sort_align pairs = align (List.sort pairs ~cmp:subcommand_cmp_fst)

end

module Help_page = struct

  let render ~summary ~usage ~readme ~choice_type ~choices =
    sprintf "\n%s\n\n  %s\n%s%s" summary usage
      (match readme with None -> "" | Some s -> sprintf "\n%s\n" (s ()))
      (if choices = [] then "" else
          sprintf "\n  === %s ===\n\n%s\n" choice_type
            (List.fold choices ~init:""
               ~f:(fun acc x -> acc ^ "  " ^ x ^ "\n")))

end

let partial_match (tbl:(string,'a) Hashtbl.t) (subcmd:string) :
    [`Exact of (string * 'a) | `Partial of (string * 'a) | `None |
        `Ambiguous of (string * 'a) list ]
    =
  match Hashtbl.find tbl subcmd with
  | Some v -> `Exact (subcmd, v)
  | None -> (* No full match, try for a partial match *)
    let possible_res =
      Hashtbl.fold tbl
        ~init:[]
        ~f:(fun ~key ~data acc ->
          if String.is_prefix key ~prefix:subcmd then
            (key, data) :: acc
          else
            acc)
    in
    match possible_res with
    | [kv] -> `Partial kv
    | []  -> `None
    | l   -> `Ambiguous l

let partial_match_opt haystack needle =
  match partial_match haystack needle with
  | `Partial v | `Exact v -> Some v
  | `Ambiguous _ | `None  -> None

let assert_no_underscores s =
  if String.exists s ~f:(fun c -> c = '_')
  then failwithf "%s contains an underscore. Use a dash instead." s ();

module Flag : sig
  (** type of flags to a command with accumulator type ['a] *)
  type 'a t
  val name : _ t -> string

  val help : _ t -> (string * string) list

  (** Template for flag-creation functions *)
  type ('a, 'b) create =
    string
    -> ?aliases:string list
    -> ?full_flag_required:bool
    -> 'b
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
  (** [rest f]: a flag that signals the end of flag processing.  all remaining arguments
      are passed to the [f] *)
  val date_acc   : ('a, 'a -> Date.t -> 'a) create
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
    type 'accum t =
    | Noarg of ('accum -> 'accum)
    | Arg of ('accum -> string -> 'accum)
    | Rest of ('accum -> string list -> 'accum)

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

  val lookup : 'a t list -> string -> 'a Action.t option

  module Poly : sig
    type 'a flag
    type t = { flag : 'a. unit -> 'a flag }
    val instantiate : t -> 'a flag
  end with type 'a flag := 'a t

  val to_spec :
    ('accum -> 'accum) ref
    -> 'accum t
    -> ('c, 'c) Core_command.Spec.t

end = struct

  module Action' = struct
    type 'a t =
      | Noarg of ('a -> 'a)
      | Arg of ('a -> string -> 'a)
      | Rest of ('a -> string list -> 'a)

    let noarg f = Noarg f
    let arg f = Arg f
    let rest f = Rest f

    let int   f = Arg (fun x s -> f x (Int.of_string   s))
    let bool  f = Arg (fun x s -> f x (Bool.of_string  s))
    let float f = Arg (fun x s -> f x (Float.of_string s))
    let date  f = Arg (fun x s -> f x (Date.of_string  s))

    let gen of_string f = Arg (fun x s -> f x (of_string  s))

    (* [project] extracts a record field together with a rebuilding
       function [inject] to fill in the remaining fields after modifying
       the field. *)
    let lift t ~project =
      match t with
      | Noarg g ->
        Noarg (fun r -> let (f, inject) = project r in inject (g f))
      | Arg g ->
        Arg (fun r x -> let (f, inject) = project r in inject (g f x))
      | Rest g ->
        Rest (fun r xs -> let (f, inject) = project r in inject (g f xs))
  end

  (* Action is a deprecated interface, that we here gussy up to be usable on the
     outside.  Action' is the primary interface here. *)
  module Action = struct
    type 'a t = 'a Action'.t =
      | Noarg of ('a -> 'a)
      | Arg of ('a -> string -> 'a)
      | Rest of ('a -> string list -> 'a)

    let cvt x a =
      a (fun acc v -> x acc v; acc)

    let noarg x = Action'.noarg (fun acc -> x acc; acc)
    let arg   x = cvt x Action'.arg
    let rest  x = cvt x Action'.rest
    let int   x = cvt x Action'.int
    let bool  x = cvt x Action'.bool
    let float x = cvt x Action'.float
  end

  type 'a t = {
    name : string;
    spec : 'a Action.t;
    doc : string;
    aliases : string list;
    full_flag_required : bool;
  }
  type 'a flag = 'a t

  let lift t ~project = { t with spec = Action'.lift t.spec ~project }

  let lift_unit t =
    let project t = (), fun _ -> t in
    lift t ~project

  let create ?(aliases=[]) ?(full_flag_required=false) ~name ~doc spec =
    assert_no_underscores name;
    {
      name = name;
      spec = spec;
      doc = doc;
      aliases = aliases;
      full_flag_required = full_flag_required;
    }

  let name v = v.name

  let lookup ts =
    let alist =
      List.concat_map ts ~f:(fun t ->
        List.map (t.name :: t.aliases) ~f:(fun v ->
          (v, t)))
    in
    match String.Table.of_alist alist with
    | `Duplicate_key flag ->
        failwithf "multiple specifications for flag %s" flag ()
    | `Ok tbl -> (fun flag ->
      match partial_match tbl flag with
      | `Exact (_, v)
      | `Partial (_, ({full_flag_required = false; _} as v)) -> Some v.spec
      | `Partial (_, ({full_flag_required = true; _} as v)) ->
        eprintf "Note: cannot abbreviate flag \"%s\".\n%!" v.name; None
      | `Ambiguous l ->
        eprintf "Note: flag \"%s\" is an ambiguous prefix: %s\n%!"
          flag (String.concat ~sep:", " (List.map ~f:fst l));
        None
      | `None -> None)
  ;;

  let help { name = name; doc = doc; aliases = aliases; _}  =
    if String.is_prefix doc ~prefix:" " then
      (name, String.lstrip doc) ::
        List.map aliases
        ~f:(fun x -> x,sprintf "same as \"%s\"" name)
    else
      let (arg, doc) =
        match String.lsplit2 doc ~on:' ' with
        | None -> (doc, "")
        | Some pair -> pair
      in
      (name ^ " " ^ arg, String.lstrip doc) ::
        List.map aliases
        ~f:(fun x -> x ^ " " ^ arg,sprintf "same as \"%s\"" name)
  ;;


  (* The creation functions are listed below *)
  type ('a,'b) create =
    string
    -> ?aliases:string list
    -> ?full_flag_required:bool
    -> 'b
    -> doc:string
    -> 'a t

  (* takes a mutable-style handling function, and returns one that has an interface like
     an immutable-style handling function *)
  let unmut handle =
    (fun acc arg -> handle acc arg; acc)
  ;;

  let of_action fix_handle action_fn =
    (fun name ?aliases ?full_flag_required handle ~doc ->
      create ?aliases ?full_flag_required ~name ~doc (action_fn (fix_handle handle)))
  ;;

  let noarg_mut x = of_action (fun h x -> h x; x) Action'.noarg x
  let arg_mut    x = of_action unmut Action'.arg   x
  let string_mut x = of_action unmut Action'.arg   x
  let int_mut    x = of_action unmut Action'.int   x
  let bool_mut   x = of_action unmut Action'.bool  x
  let float_mut  x = of_action unmut Action'.float x
  let date_mut   x = of_action unmut Action'.date  x
  let rest_mut   x = of_action unmut Action'.rest  x
  let gen_mut os x = of_action unmut (Action'.gen os)  x

  let noarg_acc  x = of_action Fn.id Action'.noarg x
  let string_acc x = of_action Fn.id Action'.arg   x
  let int_acc    x = of_action Fn.id Action'.int   x
  let bool_acc   x = of_action Fn.id Action'.bool  x
  let float_acc  x = of_action Fn.id Action'.float x
  let date_acc   x = of_action Fn.id Action'.date  x
  let rest_acc   x = of_action Fn.id Action'.rest  x
  let gen_acc os x = of_action Fn.id (Action'.gen os)  x

  let unref ref =
    (fun () x -> ref := x)
  let unref_opt ref =
    (fun () x -> ref := Some x)
  let unclos f =
    (fun () x -> f x)


  let set x = of_action (fun ref () -> ref := true) Action.noarg x
  let clear x =  of_action (fun ref () -> ref := false) Action.noarg x

  let noarg          x = of_action Fn.id      Action'.noarg x
  let string         x = of_action unclos     Action'.arg   x
  let int            x = of_action unclos     Action'.int   x
  let bool           x = of_action unclos     Action'.bool  x
  let float          x = of_action unclos     Action'.float x
  let gen         os x = of_action unclos     (Action'.gen os) x
  let set_string     x = of_action unref      Action'.arg   x
  let set_string_opt x = of_action unref_opt  Action'.arg   x
  let set_int        x = of_action unref      Action'.int   x
  let set_int_opt    x = of_action unref_opt  Action'.int   x
  let set_float      x = of_action unref      Action'.float x
  let set_float_opt  x = of_action unref_opt  Action'.float x
  let set_date       x = of_action unref      Action'.date  x
  let set_date_opt   x = of_action unref_opt  Action'.date  x
  let set_gen     os x = of_action unref      (Action'.gen os) x
  let set_gen_opt os x = of_action unref_opt  (Action'.gen os) x


  let of_arg (key, spec, doc) =
    let key = String.tr ~target:'_' ~replacement:'-' key in
    match spec with
    | Arg.Unit f -> noarg key ~doc f
    | Arg.Bool f -> bool key ~doc f
    | Arg.String f -> string key ~doc f
    | Arg.Int f -> int key ~doc f
    | Arg.Float f -> float key ~doc f
    | Arg.Set rf -> set key ~doc rf
    | Arg.Clear rf -> clear key ~doc rf
    | Arg.Set_string rf -> set_string key ~doc rf
    | Arg.Set_int rf -> set_int key ~doc rf
    | Arg.Set_float rf -> set_float key ~doc rf
    | Arg.Tuple _ -> failwith "Flag.of_arg: Arg.Tuple not supported"
    | Arg.Symbol _  -> failwith "Flag.of_arg: Arg.Symbol not supported"
    | Arg.Rest _ -> failwith "Flag.of_arg: Arg.Rest not supported"

  module Poly = struct
    type t = { flag : 'a. unit -> 'a flag }
    let instantiate t = t.flag ()
  end

  module Deprecated_spec = Core_command.Deprecated.Spec

  let to_spec flag_env_updates {name; aliases; doc; spec; _ } =
    let add_env_update new_env_update =
      let old_env_update = !flag_env_updates in
      flag_env_updates := (fun env -> new_env_update (old_env_update env))
    in
    Core_command.Spec.(
      let drop () = step (fun m _ -> m) in
      match spec with
      | Action.Noarg update ->
        drop ()
        ++ flag name ~aliases ~doc
             (Deprecated_spec.no_arg ~hook:(fun () -> add_env_update update))
      | Action.Arg update ->
        drop ()
        ++ flag name ~aliases ~doc
             (listed (arg_type (fun s -> add_env_update (fun env -> update env s); s)))
      | Action.Rest update ->
        drop ()
        ++ flag name ~aliases ~doc
             (Deprecated_spec.escape ~hook:(fun ss ->
               add_env_update (fun env -> update env ss)))
    )
end

module Shared_flags = struct
  type 'a t = {
    flags : unit Flag.t list;
    get : unit -> 'a;
  }
end

module Autocomplete_ = struct
  type t = string list -> string list

  module Bash_action = struct
    type t =
      [ `Alias | `Arrayvar | `Binding | `Builtin | `Command | `Directory
        | `Disabled | `Enabled | `Export | `File | `Function | `Group
        | `Helptopic | `Hostname | `Job | `Keyword | `Running | `Service
        | `Setopt | `Shopt | `Signal | `Stopped | `User | `Variable
      ] with sexp

    let to_string action =
      sexp_of_t action |! Sexp.to_string |! String.lowercase
  end

  let escape_for_bash string =
    let len = String.length string in
    let buffer = Buffer.create len in
    for i = 0 to len - 1 do
      begin match string.[i] with
      | '|' | '&' | ';' | '(' | ')' | '<' | '>' | ' ' | '\t' ->
          Buffer.add_char buffer '\\';
      | _ -> ()
      end;
      Buffer.add_char buffer string.[i];
    done;
    Buffer.contents buffer

  let bash =
    let create_command_line action key =
      let options =
        match action with
        | `Directory -> " -S/ "
        | _ -> " "
      in
      "compgen" ^ options ^ " -A " ^ Bash_action.to_string action ^ " " ^ key
    in
    let single_command action key =
      let command_line = create_command_line action key in
      try Shell.run_full "/bin/bash" ["-c"; command_line] with
      | _exn -> ""
    in
    let command actions key =
      let actions =
        if
          List.exists actions ~f:((=) `File)
          && not (List.exists actions ~f:((=) `Directory))
        then
          `Directory :: actions
        else actions
      in
      List.map ~f:(fun action -> single_command action key) actions
      |!  String.concat ~sep:"\n"
    in
    fun actions command_line ->
      let result =
        match List.rev command_line with
        | [] -> command actions ""
        | key :: _ -> command actions key
      in
      String.split ~on:'\n' result |! List.map ~f:escape_for_bash
  ;;
end

let maybe_dashify ~allow_underscores =
  if not allow_underscores
  then (fun s -> s)
  else (fun s ->
    String.mapi s ~f:(fun i c ->
      if i = 0 || c <> '_'
      then c
      else '-'
    ))


let is_help_flag = function
  | ("--help" | "-help" | "help" | "h" | "?" | "-?") -> true
  | _ -> false

type group = {
  summary : string;
  readme : (unit -> string) option;
  subcommands : t String.Table.t;
}

and t =
  | Core of Core_command.t * (string list -> string list) option (* autocomplete *)
  | Group of group

let group ~summary ?readme alist =
  List.iter alist ~f:(fun (name, _) -> assert_no_underscores name);
  match String.Table.of_alist alist with
  | `Ok subcommands -> Group { summary; readme; subcommands }
  | `Duplicate_key name -> failwith ("multiple subcommands named " ^ name)

let summary = function
  | Core (base, _) -> Core_command.Deprecated.summary base
  | Group grp -> grp.summary

let help ~cmd t =
  match t with
  | Core _ ->
    (* This will be dealt with by the Core_command internally
       this function is only called from within dispatch.
       during a call to dispatch, if we get to a point where our command is a Core
       (and would thus get into this branch),
       we call Core_command.Deprecated.run, which punts the entire functionality
       over to Core_command
    *)
    assert false
  | Group grp ->
      let alist =
        ("help [-r]", "explain a given subcommand (perhaps recursively)") ::
        List.map ~f:(fun (cmd, t) -> (cmd, summary t)) (Hashtbl.to_alist grp.subcommands)
      in
      Help_page.render
        ~summary:grp.summary
        ~readme:grp.readme
        ~usage:(cmd ^ " SUBCOMMAND")
        ~choice_type:"available subcommands"
        ~choices:(Columns.sort_align alist)
;;

let help_help ~cmd subcommands =
  let choices =
    List.sort ~cmp:subcommand_cmp subcommands
  in
  Help_page.render
    ~summary:("explain " ^ cmd ^ " or one of its subcommands, perhaps recursively")
    ~readme:None
    ~usage:(cmd ^ " help [-r[ecursive] [-flags] [-expand-dots]] [SUBCOMMAND]\n"
            ^ "  " ^ cmd ^ " -? [SUBCOMMAND]\n"
            ^ "  " ^ cmd ^ " -?? [SUBCOMMAND]   # (shortcut for -help -r) \n"
            ^ "  " ^ cmd ^ " -??? [SUBCOMMAND]  # (shortcut for -help -r -flags)")
    ~choice_type:"available subcommands"
    ~choices
;;

(* Implements the "help -r" feature *)
let help_recursive ~cmd ~with_flags ~expand_dots t =
  let rec help_recursive_rec ~cmd t s =
    let new_s = s ^ (if expand_dots then cmd else ".") ^ " " in
    match t with
    | Core (t, _) ->
      Core_command.Deprecated.help_recursive ~cmd ~with_flags ~expand_dots t s
    | Group grp ->
      (s ^ cmd, grp.summary) ::
        List.concat_map
          (List.sort ~cmp:subcommand_cmp_fst (Hashtbl.to_alist grp.subcommands))
          ~f:(fun (cmd', t) -> help_recursive_rec ~cmd:cmd' t new_s)
  in
  let alist = help_recursive_rec ~cmd t "" in
  let choices = Columns.align alist in
  match t with
  | Core _ ->
    (*  help_recursive is only called from within dispatch
        if we ever get to a Core variant (which would cause us to be in this branch),
        we would have gone into the | Core branch of dispatch,
        which would punt responibility over to Core_command
    *)
    assert false
  | Group grp ->
    Help_page.render
      ~summary:grp.summary
      ~readme:grp.readme
      ~usage:(cmd ^ " SUBCOMMAND")
      ~choice_type:("available subcommands" ^ if with_flags then " and flags" else "")
      ~choices

(* These refs are populated by run_internal. *)
let expanded_argv_head_rev = ref []
let expanded_argv_tail = ref []

(* This exception is raised if we try to read expanded argv before calling run_internal *)
exception Expanded_argv_not_yet_initialized with sexp

let get_expanded_argv () =
  let expanded_argv =
    List.rev_map_append !expanded_argv_head_rev !expanded_argv_tail ~f:(fun x->x)
  in
  if expanded_argv = [] then
    raise Expanded_argv_not_yet_initialized
  else
    expanded_argv
;;

let get_expanded_cmdline () = String.concat ~sep:" " (get_expanded_argv ())

module Autocomplete = struct
  let bash_autocomplete_function =
    let fname = sprintf "_jsautocom_%s" (Pid.to_string (Unix.getpid ())) in
    sprintf
"%s () {
  COMPREPLY=()
  LENV=\"\"
  i=0;
  for e in \"${COMP_WORDS[@]}\"; do
    LENV=\"$LENV COMMAND_COMP_WORDS_${i}=\\\"$e\\\"\";
    (( i++ ));
  done;
  CC=\"eval COMMAND_COMP_CWORD=\"${COMP_CWORD}\" COMMAND_COMP_POINT=\"${COMP_POINT}\" \
  $LENV ${COMP_WORDS}\"
  matching_options=`$CC`
  eval ${matching_options}
  return 0
}
complete -F %s %s" fname fname Sys.argv.(0)

  let output_bash_function () =
    print_endline bash_autocomplete_function;
    ()

  include Autocomplete_

  let print_list strings =
    let options = List.filter ~f:((<>) "") strings in
    let arr = Array.of_list options in
    Array.iteri arr ~f:(fun i contents ->
      printf "COMPREPLY[%i]='%s'; " (i + 1) contents)

  let get_environment_numeric environment =
    match Sys.getenv environment with
    | Some word ->
        if word = "" then 0
        else int_of_string word
    | None -> assert false

  let current_word () =
    get_environment_numeric "COMMAND_COMP_CWORD"

  let current_point () =
    get_environment_numeric "COMMAND_COMP_POINT" - String.length Sys.argv.(0)

  let external_completion ~autocomplete ~key ~command_line =
    let completion_line =
      match autocomplete with
      | None -> Autocomplete_.bash [`File] [key]
      | Some f -> f command_line
    in
    print_list completion_line

  let filter_matching_prefixes_and_print
      ~autocomplete ~key ~command_line strings =
    match List.filter strings ~f:(String.is_prefix ~prefix:key) with
    | [] -> external_completion ~autocomplete ~key ~command_line
    | lst -> print_list lst

  let rec autocomplete t command_line =
    match t with
    | Core (base, autocomplete) ->
      let flags = Core_command.Deprecated.get_flag_names base in
      (match List.rev command_line with
      | [] -> print_list flags
      | key :: _ ->
        if key = "" || key.[0] <> '-' then
          external_completion ~autocomplete ~key ~command_line
        else filter_matching_prefixes_and_print ~autocomplete ~key ~command_line flags)
    | Group grp ->
      match command_line with
      | [key] ->
        Hashtbl.keys grp.subcommands
        |! filter_matching_prefixes_and_print ~autocomplete:None ~key ~command_line
      | [] -> (* We are at the root and all the options are requested *)
          Hashtbl.keys grp.subcommands |! print_list
      | key :: argv ->
        match Hashtbl.find grp.subcommands key with
        | None -> ()
        | Some t -> autocomplete t argv

  let rec truncate_command_line ~current_word ~current_point command_line =
    (* Printf.fprintf stderr "Word: %d, Point: %d" current_word current_point; *)
    if current_word = 1 then begin
      match command_line with
      | h :: _ ->
          let current_point =
            (* We might be off due to the spaces ... is there a better way? *)
            min (String.length h) (max 0 current_point)
          in
          [String.sub h ~pos:0 ~len:current_point]
      | [] -> [""]
    end else begin
      assert (current_word > 0);
      match command_line with
      | h :: command_line ->
          let len = String.length h in
          let current_word = pred current_word in
          let current_point = current_point - (len + 1) in
          h :: truncate_command_line ~current_word ~current_point command_line
      | [] -> []
    end

  let autocomplete t command_line =
    let current_word = current_word () in
    let current_point = current_point () in
    let command_line = truncate_command_line ~current_word ~current_point command_line in
    autocomplete t command_line

  (* We clear the setting of environment variable associated with command-line
     completion so that subprocesses don't see them. *)
  let getenv_and_clear var =
    let value = Sys.getenv var in
    if is_some value then Unix.unsetenv var;
    value
  ;;

  let rec execution_mode' index =
    match getenv_and_clear ("COMMAND_COMP_WORDS_" ^ string_of_int index) with
    | None -> []
    | Some command -> command :: execution_mode' (index + 1)
  ;;

  let execution_mode () =
    match getenv_and_clear "COMMAND_OUTPUT_INSTALLATION_BASH" with
    | Some _ -> `print_bash_autocomplete_function
    | None ->
      let command_list = execution_mode' 0 in
      match command_list with
      | [] -> `run_main
      | _ :: partial_command_line -> `doing_auto_completion partial_command_line
  ;;
end

let of_core_command t = Core (t, None)

let create ?autocomplete ?readme ~summary ~usage_arg ~init ~flags ~final main =
  let c =
    Core_command.basic ~summary ?readme
      Core_command.Spec.(
        let flag_env_updates = ref Fn.id in
        let flags =
          List.fold flags ~init:(step Fn.id) ~f:(fun flags t ->
            let flag = Flag.to_spec flag_env_updates t in
            flags ++ flag)
        in
        flags
        ++ step (fun m anons ->
          let env = init () in
          let env = !flag_env_updates env in
          let argv = final env anons in
          m argv
        )
        ++ anon (ad_hoc ~usage_arg)
      )
      main
  in
  Core (c, autocomplete)

let create0 ?autocomplete ?readme ~summary ~usage_arg ~init ~flags ~final main =
  let final accum anonargs =
    match anonargs with
    | [] -> final accum
    | _ :: _ as lst ->
      printf "Error: expected 0 anonymous arguments, got %i\n%!" (List.length lst);
      exit 1
  in
  create ?autocomplete ?readme ~summary ~usage_arg ~init ~flags ~final main

let create_no_accum ?autocomplete ?readme ~summary ~usage_arg ~flags ~final main =
  let init () = () in
  let final _ anonargs = final anonargs in
  create ?autocomplete ?readme ~summary ~usage_arg ~init ~flags ~final main

let create_no_accum0 ?autocomplete ?readme ~summary ~usage_arg ~flags main =
  let init () = () in
  let final _ = () in
  create0 ?autocomplete ?readme ~summary ~usage_arg ~init ~flags ~final main

INCLUDE "version_defaults.mlh"
module Version = struct
  type command = t

  type t = {
    command : command;
    flags : unit Flag.t list;
  }

  let print_version ?(version = DEFAULT_VERSION) () =
    print_endline version

  let print_build_info ?(build_info = DEFAULT_BUILDINFO) () =
    print_endline build_info

  let poly_flags ?version ?build_info () = [
    { Flag.Poly.flag = fun () ->
      Flag.noarg_acc "-version"
        (fun _ -> print_version ?version (); exit 0)
        ~doc:" Print the version of this build and exit" };
    { Flag.Poly.flag = fun () ->
      Flag.noarg_acc "-build-info"
        (fun _ -> print_build_info ?build_info (); exit 0)
        ~doc:" Print build info as sexp and exit" };
  ]

  let flags ?version ?build_info () =
    List.map ~f:Flag.Poly.instantiate (poly_flags ?version ?build_info ())

  let command ?version ?build_info () =
    let summary = "Print version information" in
    let usage_arg = "[-version | -build-info]" in
    let init () = () in
    let flags = flags ?version ?build_info () in
    let final () _anons = () in
    let main () =
      eprintf "(no option given - printing version)\n%!";
      print_version ?version ();
      exit 0
    in
    create ~summary ~usage_arg ~init ~flags ~final main

  let add ?version ?build_info unversioned =
    let command =
        match unversioned with
        | Core _ -> failwith "You have used a Core_command in a Command basic stub. \
                              Please convert fully to Core_command"
        | Group grp ->
          group ~summary:grp.summary
            (("version", command ?version ?build_info ())
              :: String.Table.to_alist grp.subcommands)
    in
    { command; flags = flags ?version ?build_info () }
end

let run_internal versioned ~allow_unknown_flags:_ ~allow_underscores ~cmd
      ~argv ?post_parse =
  let maybe_dashify = maybe_dashify ~allow_underscores in
  expanded_argv_head_rev := [cmd];
  expanded_argv_tail := argv;
  let rec loop t ~is_help ~is_help_rec ~is_help_rec_flags ~is_expand_dots cmd argv =
    let update_expanded_argv l1 l2 =
      expanded_argv_head_rev := l1 @ !expanded_argv_head_rev;
      expanded_argv_tail := l2
    in
    let post_parse_call ~is_ok =
      match post_parse with
      | None -> ()
      | Some f ->
        let status_code =
          match is_ok, is_help with
          | false, _ -> `Error
          | true, true -> `Help
          | true, false -> `Ok
        in
        f status_code (get_expanded_argv ())
    in
    match t with
    | Core (t, _) -> fun () ->
        Core_command.Deprecated.run
          t ~cmd ~args:argv ~is_help ~is_help_rec ~is_help_rec_flags ~is_expand_dots
    | Group grp ->
      let execute_group (subcmd, rest) =
        match partial_match grp.subcommands (maybe_dashify subcmd) with
        | `Exact (full_subcmd, t)
        | `Partial (full_subcmd, t) ->
          update_expanded_argv [full_subcmd] rest;
          loop t ~is_help ~is_help_rec ~is_help_rec_flags ~is_expand_dots
            (cmd ^ " " ^ full_subcmd) rest
        | `Ambiguous l ->
          post_parse_call ~is_ok:false;
          eprintf "%s\n%s%!"
            (sprintf "subcommand %s is an ambiguous prefix: %s"
              subcmd (String.concat ~sep:", " (List.map ~f:fst l)))
            (help ~cmd t);
          exit 1
        | `None ->
          post_parse_call ~is_ok:false;
          eprintf "%s\n%s%!" ("unknown subcommand " ^ subcmd ^ " for command " ^ cmd)
            (help ~cmd t);
          exit 1
      in
      match argv with
      | [] ->
        post_parse_call ~is_ok:is_help;
        (fun () ->
          if is_help then
            (printf "%s" (
              if is_help_rec then
                help_recursive ~cmd ~with_flags:is_help_rec_flags
                  ~expand_dots:is_expand_dots t
              else
                help ~cmd t);
              exit 0)
          else
            (eprintf "%s%!" (help ~cmd t);
             eprintf ("\n\nA subcommand for %s is required\n%!") cmd;
             exit 1))
      | arg :: rest ->
        begin
          match Flag.lookup versioned.Version.flags (maybe_dashify arg) with
          | None -> ()
          | Some (Flag.Action.Rest _ | Flag.Action.Arg _) -> assert false
          | Some (Flag.Action.Noarg f) -> f ()
        end;
        match arg,rest with
        | ( "??"  | "-??" ), rest ->
          update_expanded_argv ["-recursive";"-help"] rest;
          loop t ~is_help:true ~is_help_rec:true ~is_help_rec_flags ~is_expand_dots cmd rest
        | ( "???" | "-???" ), rest ->
          update_expanded_argv ["-flags";"-recursive";"-help"] rest;
          loop t ~is_help:true ~is_help_rec:true ~is_help_rec_flags:true ~is_expand_dots
            cmd rest
        | flag, rest when is_help_flag flag ->
          update_expanded_argv ["-help"] rest;
          if is_help then begin
            post_parse_call ~is_ok:true;
            (fun () ->
              printf "%s" (help_help ~cmd (Hashtbl.keys grp.subcommands));
              exit 0)
          end
          else
            loop t ~is_help:true ~is_help_rec ~is_help_rec_flags ~is_expand_dots cmd rest
        | ("-r" | "-recursive"), rest when is_help ->
          update_expanded_argv ["-recursive"] rest;
          loop t ~is_help ~is_help_rec:true ~is_help_rec_flags ~is_expand_dots cmd rest
        | "-flags", rest when is_help ->
          update_expanded_argv ["-flags"] rest;
          loop t ~is_help ~is_help_rec ~is_help_rec_flags:true ~is_expand_dots cmd rest
        | "-expand-dots", rest when is_help ->
          update_expanded_argv ["-expand-dots"] rest;
          loop t ~is_help ~is_help_rec ~is_help_rec_flags ~is_expand_dots:true cmd rest
        | subcmd, rest -> execute_group (subcmd, rest)
  in
  loop versioned.Version.command ~is_help:false ~is_help_rec:false
    ~is_help_rec_flags:false ~is_expand_dots:false cmd argv
;;

(*
    The #! protocol for stand-alone scripts groups together all embedded
    flags as one.  If the first line of a #! script reads

        #! /path/to/my/command -flag1 -flag2 -flag3

    and then we call the script as

        > script arg1 arg2

    then the argument vector passed to /path/to/my/command will be

        ["-flag1 -flag2 -flag3"; "arg1"; "arg2"]

    So we need to pull apart the first argument into three.  Likely, this
    will only happen when the first argument is a flag (starts with '-')
    and contains a space.
*)
let hash_bang_expand_arg = function
  | (first :: rest) as same ->
      if String.is_prefix first ~prefix:"-" then
        String.split first ~on:' ' @ rest
      else
        same
  | other -> other
;;

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

let (run : unit with_run_flags)
    ?version ?build_info
    ?argv
    ?(allow_unknown_flags=false)
    ?(allow_underscores=true)
    ?(hash_bang_expand=false)
    ?post_parse
    t =
  match t with
  | Core (c, _) -> Core_command.run ?version ?build_info ?argv c
  | Group _ as t ->
    let t = Version.add ?version ?build_info t in
    match Autocomplete.execution_mode () with
    | `print_bash_autocomplete_function ->
      Autocomplete.output_bash_function ();
      exit 0
    | `doing_auto_completion partial_command_line ->
      Autocomplete.autocomplete t.Version.command partial_command_line;
      exit 0
    | `run_main ->
      let argv = Option.value argv ~default:(Array.to_list Sys.argv) in
      match argv with
      | [] -> failwith "no command name passed in" (* I think this is impossible *)
      | cmd :: argv ->
        let cmd = Filename.basename cmd in
        let argv = if hash_bang_expand then hash_bang_expand_arg argv else argv in
        run_internal t ~allow_unknown_flags ~allow_underscores ~cmd ~argv ?post_parse ()
;;

module Annotated_field = struct
  type t = {
    field_name : string;
    flag_name : string;
    doc : string;
    value : [
      | `Optional of string option
      | `Default of string
      | `Specified of string option
      | `List of string list
      (* [`Switch v] is for boolean fields with default value [v]
         where passing the flag (with no arguments) sets it to [not v] *)
      | `Switch of bool
    ]
  }

  type accum = t list ref

  module F = Fieldslib.Field

  let with_names name field ~doc ~value = {
    field_name = field.F.name;
    flag_name = Option.value name ~default:field.F.name;
    doc = doc;
    value = value;
  }

  let required ?name t_list ~doc field =
    let t =
      with_names name field
      ~doc:(doc ^ " (required)")
      ~value:(`Specified None)
    in
    t :: t_list
  ;;

  let default ?name default to_string t_list ~doc field =
    let default_s = to_string default in
    let t =
      with_names name field
        ~doc:(sprintf "%s (default=%s)" doc default_s)
        ~value:(`Default default_s)
    in
    t :: t_list
  ;;

  let optional ?name ?(suppress_word_optional=false) t_list ~doc field =
    let t =
      with_names name field
        ~doc:(doc ^ (if suppress_word_optional then "" else " (optional)"))
        ~value:(`Optional None)
    in
    t :: t_list
  ;;

  let set ?name t_list ~doc field =
    let t =
      with_names name field
        ~doc:(doc ^ " (default=false)")
        ~value:(`Switch false)
    in
    t :: t_list
  ;;

  let clear ?name t_list ~doc field =
    let t =
      with_names name field
        ~doc:(doc ^ " (default=true)")
        ~value:(`Switch true)
    in
    t :: t_list
  ;;

  let list ?name t_list ~doc field =
    let t = with_names name field ~doc ~value:(`List []) in
    t :: t_list
  ;;

  let init t_list = ref t_list

  let alter_value t_list ~name ~f =
    match List.find t_list ~f:(fun t -> t.flag_name = name) with
    | None -> t_list
    | Some t ->
        let new_t = {t with value = f t.value} in
        new_t :: List.filter t_list ~f:(fun t -> t.flag_name <> name)
  ;;

  let to_flag t =
    let err_specified_more_than_once () =
      failwithf "%s specified more than once" t.flag_name ()
    in
    let create flag_creator handler =
      let name = "-" ^ String.tr ~target:'_' ~replacement:'-' t.flag_name in
      flag_creator name ?aliases:None ?full_flag_required:None handler ~doc:t.doc
    in
    match t.value with
    | `Switch default -> create Flag.noarg_mut (fun accum ->
      accum := alter_value !accum ~name:t.flag_name ~f:(function
        | `Switch _ -> `Specified (Some (string_of_bool (not default)))
        | _ -> err_specified_more_than_once ()))
    | `Specified None -> create Flag.arg_mut (fun accum s ->
      accum := alter_value !accum ~name:t.flag_name ~f:(function
        | `Specified None -> `Specified (Some s)
        | _ -> err_specified_more_than_once ()))
    | `Optional None -> create Flag.arg_mut (fun accum s ->
      accum := alter_value !accum ~name:t.flag_name ~f:(function
        | `Optional None -> `Specified (Some s)
        | _ -> err_specified_more_than_once ()))
    | `Default _ -> create Flag.arg_mut (fun accum s ->
      accum := alter_value !accum ~name:t.flag_name ~f:(function
        | `Default _ -> `Specified (Some s)
        | _ -> err_specified_more_than_once ()))
    | `List [] -> create Flag.arg_mut (fun accum s ->
      accum := alter_value !accum ~name:t.flag_name ~f:(function
        | `List l -> `List (s :: l)
        | _ -> assert false
      ))
    | `Optional (Some _)
    | `Specified (Some _)
    | `List (_ :: _)
      -> failwith "did you call to_flag more than once?"
  ;;

  let get accum of_string field =
    let field_name = field.F.name in
    let err_unspecified () = failwithf "%s not specified" field_name () in
    match List.find !accum ~f:(fun t -> t.field_name = field_name) with
    | None -> err_unspecified ()
    | Some t ->
      begin match t.value with
      | `Switch b -> of_string (Bool.to_string b)
      | `Specified (Some s) | `Default s -> of_string s
      | `Optional None -> of_string "None"
      | `Optional (Some s) -> of_string ("Some " ^ s)
      | `Specified None -> err_unspecified ()
      | `List _ -> failwith "use get_list"
      end
  ;;

  let get_opt accum of_string field =
    let field_name = field.F.name in
    let err_unspecified () = failwithf "%s not specified" field_name () in
    match List.find !accum ~f:(fun t -> t.field_name = field_name) with
    | None -> err_unspecified ()
    | Some t ->
      begin match t.value with
      | `Optional x | `Specified x -> Option.map x ~f:of_string
      | _ -> invalid_arg "Annotated_field.get_opt"
      end
  ;;

  let get_list accum of_string field =
    let field_name = field.F.name in
    let err_unspecified () = failwithf "%s not specified" field_name () in
    match List.find !accum ~f:(fun t -> t.field_name = field_name) with
    | None -> err_unspecified ()
    | Some t ->
      begin match t.value with
      | `List x -> List.map x ~f:of_string
      | _ -> invalid_arg "Annotated_field.get_list"
      end
  ;;
end


module Flags_ext = struct
  type 'a setter =
  | No_arg of 'a
  | Arg of (string -> 'a)

  let is_no_arg = function No_arg _ -> true | _ -> false

  type ('a, 'flag) t = {
    flag : 'flag;
    get : unit -> 'a;
    is_set : unit -> bool;
  }

  let flag t = t.flag
  let get t = t.get ()
  let is_set t = t.is_set ()

  (* ******************************************************** *)
  (* Flags that can occur at most once                        *)

  let create_internal ~default ~name ~doc setter =
    let name = "-" ^ name in
    if default = None && is_no_arg setter then
      failwithf
        "Flags_ext: Flag %s is a no_arg flag without a default (doesn't make sense)"
        name ();
    let acc = ref None in
    let flag =
      let fail () = failwithf "Flag %s should only be specified once" name () in
      match setter with
      | No_arg final  ->
        Flag.noarg name ~doc (fun () ->
          match !acc with
          | None ->
            acc := Some final
          | Some _ -> fail ())
      | Arg of_string ->
        Flag.string name ~doc (fun str ->
          match !acc with
          | None ->
            acc := Some (of_string str)
          | Some _ -> fail ())
    in
    let get () =
      match default with
      | Some default ->  Option.value !acc ~default
      | None ->
        Option.value_exn_message
          (sprintf "Required argument %s was not specified" name)
          (!acc)
    in
    { flag = flag;
      get = get;
      is_set = fun () -> is_some !acc;
    }

  let create ?default ~name ~doc setter =
    let (default, doc) = match default with
      | None                -> None  , sprintf "%s (required)" doc
      | Some (v, to_string) ->
        Some v,
        if is_no_arg setter then doc (* corresponds to set or clear *)
        else sprintf "%s (default=%s)" doc (to_string v)
    in
    create_internal ~default ~name ~doc setter

  let create_optional ~name ~doc setter =
    let doc = sprintf "%s (optional)" doc in
    (* optional arguments have a default of None *)
    let default = Some None in
    let setter = match setter with
      | No_arg final  -> No_arg (Some final)
      | Arg of_string -> Arg (fun s-> Some (of_string s))
    in
    create_internal ~default ~name ~doc setter

  let create_rest ?(name="--") ~doc () =
    let x = ref None in
    let flag = Flag.rest_acc name ~doc (fun () s's ->
      x := Some s's
    )
    in
    { flag;
      get = (fun () -> !x);
      is_set = (fun () -> Option.is_some !x)
    }

  (* ************************************************************************** *)
  (* Repeatable flags                                                           *)

  let create_many ?(at_least_one=false) ~name ~doc setter =
    let name = "-" ^ name in
    let doc = sprintf "%s (%d or more)" doc (if at_least_one then 1 else 0) in
    let acc = ref [] in
    let flag =
      match setter with
      | Arg of_string ->
        Flag.string name ~doc (fun str -> acc := (of_string str) :: !acc)
      (* This case only really makes sense when [x] is [()]. *)
      | No_arg x ->
        Flag.noarg name ~doc (fun () -> acc := x :: !acc)
    in
    let get () =
      if at_least_one && !acc = [] then
        failwithf "Flag %s must be specified at least once" name ();
      !acc
    in
    { flag = flag;
      get = get;
      is_set = fun () -> not (List.is_empty !acc);
    }


  (* ******************************************************** *)
  (* Choice (1 of n) flags that can occur atmost once         *)
  let create_choice_internal ?default spec_list =
    let acc = ref None in
    let names = List.map spec_list ~f:(fun (name, _, _) ->  name) in
    let names_string = String.concat ~sep:"," names in
    let make_flag (name, doc, setter) = match setter with
      | No_arg v ->
        Flag.noarg name ~doc (fun () ->
          match !acc with
          | Some _ -> failwithf "Only one of  %s can be specified." names_string ();
          | None -> acc := Some v)
      | Arg of_string ->
        Flag.string name ~doc (fun str ->
          match !acc with
          | Some _ -> failwithf "Only one of  %s can be specified." names_string ();
          | None -> acc := Some (of_string str))
    in
    let flags = List.map spec_list ~f:make_flag in
    let get () = match (!acc, default) with
      | Some v, _      -> v
      | None  , Some v -> v
      | None  , None    ->
        failwithf "At least one of %s must be specified" names_string ()
    in
    { flag = flags;
      get = get;
      is_set = fun () -> is_some !acc;
    }

  let foreach ls ~f =
    let rec loop = function
      | [] -> []
      | [x] -> [f x ~first:false ~last:true]
      | x :: xs -> f x ~first:false ~last:false :: loop xs
    in
    let loop0 = function
      | [] -> []
      | [x] -> [f x ~first:true ~last:true]
      | (x :: xs) -> f x ~first:true ~last:false :: loop xs
    in
    loop0 ls

  let make_choice_indicator_string first last =
    match (first, last) with
    | true , _     -> ""
    | false, false -> "(OR) "
    | false, true  -> "(OR) "

  let create_choice ?default spec_list =
    let (default, spec_list) =  match default with
      | Some (v, to_string) ->
        (Some v,
         foreach spec_list
           ~f:(fun (name, doc, setter) ~first ~last ->
             let (arg, desc) = String.lsplit2_exn doc ~on:' ' in
             let doc = sprintf "%s %s%s (default=%s)"
               arg
               (make_choice_indicator_string first last)
               desc
               (to_string v) in
             ("-" ^ name, doc, setter)))
      | None ->
        (None,
         foreach spec_list
           ~f:(fun (name, doc, setter) ~first ~last ->
             let (arg, desc) = String.lsplit2_exn doc ~on:' ' in
             let doc = sprintf "%s %s%s"
               arg
               (make_choice_indicator_string first last)
               desc
             in
             ("-" ^ name, doc, setter)))
    in
    create_choice_internal ?default spec_list

  let create_choice_optional spec_list =
    let spec_list =
      foreach spec_list
        ~f:(fun (name, doc, setter) ~first ~last ->
          let (arg, desc) = String.lsplit2_exn doc ~on:' ' in
          let doc = sprintf "%s %s%s (optional)"
            arg
            (make_choice_indicator_string first last)
            desc
          in
          let setter = match setter with
            | Arg of_string -> Arg (fun s -> Some (of_string s))
            | No_arg v -> No_arg (Some v)
          in
          ("-" ^ name, doc, setter))
    in
    create_choice_internal ~default:None spec_list

  let create_set ~name ~doc = create
    ~default:(false,string_of_bool)
    ~name ~doc
    (No_arg true)
end


module Helpers = struct
  exception Found_anonymous_arguments with sexp
  let no_anons c anons =
    match anons with
    | [] -> c
    | _  ->
      Printf.eprintf "No anonymous arguments expected\n%!";
      raise Found_anonymous_arguments
  ;;
end

