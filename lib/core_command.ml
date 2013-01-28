open Core.Std

let unwords xs = String.concat ~sep:" " xs
let unlines xs = String.concat ~sep:"\n" xs
let unparagraphs xs = String.concat ~sep:"\n\n" xs

exception Failed_to_parse_command_line of string

let die fmt = Printf.ksprintf (fun msg () -> raise (Failed_to_parse_command_line msg)) fmt

let flag_with_dash s = if String.is_prefix ~prefix:"-" s then s else "-" ^ s

let help_screen_compare a b =
  match (a, b) with
  | (_, "help") -> -1
  | (_, "-help") -> -1
  | ("help", _) -> 1
  | ("-help", _) -> 1
  | _ -> String.compare a b

module Format : sig
  type t = { name : string; doc : string; aliases : string list; }
  val sort : t list -> t list
  val to_string : t list -> string
end = struct
  type t = { name : string; doc : string; aliases : string list; }
  let sort ts =
    List.sort ts ~cmp:(fun {name = a; _} {name = b; _} -> help_screen_compare a b)
  let to_string ts =
    let n =
      List.fold ts ~init:0
        ~f:(fun acc t -> Int.max acc (String.length t.name))
    in
    let extend x =
      let slack = n - String.length x in
      if Int.equal slack 0 then x else x ^ String.make slack ' '
    in
    String.concat
      (List.map ts ~f:(fun t ->
        let row k v = ["  "; extend k; "  "; v; "\n"] in
        String.concat
          (List.concat
            (row t.name t.doc :: begin
              match t.aliases with
              | [] -> []
              | [x] -> [row "" (sprintf "(alias: %s)" x)]
              | xs  -> [row "" (sprintf "(aliases: %s)" (String.concat ~sep:", " xs))]
            end))))
end

(* universal maps are used to pass around values between different bits
   of command line parsing code without having a huge impact on the
   types involved

    1. passing values from parsed args to command-line autocomplete functions
    2. passing special values to a base commands that request them in their spec
        * expanded subcommand path
        * args passed to the base command
        * help text for the base command
*)
module Env = Hmap

module Completer = struct
  type t = (Env.t -> part:string -> string list) option
  let run_and_exit t env ~part : never_returns =
    Option.iter t ~f:(fun completions ->
      List.iter ~f:print_endline (completions env ~part));
    exit 0
end

module Arg_type = struct

  type 'a arg_type = {
    parse : string -> 'a option;
    complete : Completer.t;
    key : 'a Env.Key.t option;
  }

  let arg_type ?complete ?key of_string =
    let parse x = Option.try_with (fun () -> of_string x) in
    {parse; key; complete}

  let string    = arg_type Fn.id
  let int       = arg_type Int.of_string
  let float     = arg_type Float.of_string
  let bool      = arg_type Bool.of_string
  let date      = arg_type Date.of_string
  let time_span = arg_type Time.Span.of_string

  let file =
    arg_type Fn.id ~complete:(fun _ ~part ->
      let prog = "bash" in
      let args = [| "bash"; "-c"; "compgen -f $0"; part |] in
      Unix.execvp ~prog ~args)

end

module Flag = struct

  type action =
    | No_arg of (Env.t -> Env.t)
    | Arg    of (string -> Env.t -> Env.t) * Completer.t
    | Rest   of (string list -> unit)

  type t = {
    name : string;
    aliases : string list;
    action : action;
    doc : string;
    check_available_if_required : unit -> unit;
  }

  module Deprecated = struct
    (* flag help in the format of the old command. used for injection *)
    let help {name; doc; aliases; _}  =
      if String.is_prefix doc ~prefix:" " then
        (name, String.lstrip doc) ::
        List.map aliases ~f:(fun x ->
          (x, sprintf "same as \"%s\"" name))
      else
        let (arg, doc) =
          match String.lsplit2 doc ~on:' ' with
          | None -> (doc, "")
          | Some pair -> pair
        in
        (name ^ " " ^ arg, String.lstrip doc) ::
          List.map aliases ~f:(fun x ->
            (x ^ " " ^ arg, sprintf "same as \"%s\"" name))
  end

  let align {name; doc; aliases; _} =
    let (name, doc) =
      match String.lsplit2 doc ~on:' ' with
      | None | Some ("", _) -> (name, String.strip doc)
      | Some (arg, doc) -> (name ^ " " ^ arg, doc)
    in
    {Format.name; doc; aliases}

  module Spec = struct

    type 'a state = { action : action; read : unit -> 'a; }

    type 'a t = string -> 'a state

    let arg_flag name arg_type read write =
      { read = read;
        action =
          let update arg env =
            match arg_type.Arg_type.parse arg with
            | None -> die "failed to parse %s value %S" name arg ()
            | Some arg ->
              write arg;
              match arg_type.Arg_type.key with
              | None -> env
              | Some key -> Env.add env key arg
          in
          Arg (update, arg_type.Arg_type.complete); }

    let write_option name v arg =
      match !v with
      | None -> v := Some arg
      | Some _ -> die "flag %s passed more than once" name ()

    let required_value ?default arg_type name =
      let v = ref None in
      let read () =
        match !v with
        | Some v -> v
        | None ->
          match default with
          | Some v -> v
          | None -> die "missing required flag: %s" name ()
      in
      let write arg = write_option name v arg in
      arg_flag name arg_type read write

    let required arg_type name =
      required_value arg_type name

    let optional_with_default default arg_type name =
      required_value ~default arg_type name

    let optional arg_type name =
      let v = ref None in
      let read () = !v in
      let write arg = write_option name v arg in
      arg_flag name arg_type read write

    let no_arg_general ~deprecated_hook ~key name =
      let v = ref false in
      let read () = !v in
      let write () =
        if !v then
          die "flag %s passed more than once" name ()
        else
          v := true
      in
      let action env =
        let env = Option.fold key ~init:env ~f:(fun env key -> Hmap.add env key ()) in
        write ();
        env
      in
      let action =
        match deprecated_hook with
        | None -> action
        | Some f -> (fun x -> f (); action x)
      in
      { read; action = No_arg action }

    let no_arg name =
      no_arg_general ~deprecated_hook:None ~key:None name

    let no_arg_register ~key name =
      no_arg_general ~deprecated_hook:None ~key:(Some key) name

    let listed arg_type name =
      let v = ref [] in
      let read () = List.rev !v in
      let write arg = v := arg :: !v in
      arg_flag name arg_type read write

    let escape_general ~deprecated_hook _name =
      let cell = ref None in
      let action = (fun cmd_line -> cell := Some cmd_line) in
      let read () = !cell in
      let action =
        match deprecated_hook with
        | None -> action
        | Some f -> fun x -> f x; action x
      in
      { action = Rest action; read }

    let escape name = escape_general ~deprecated_hook:None name

    module Deprecated = struct

      let no_arg ~hook name = no_arg_general ~deprecated_hook:(Some hook) ~key:None name
      let escape ~hook      = escape_general ~deprecated_hook:(Some hook)

    end

  end

end

module Path : sig
  type t
  val empty : t
  val root : t
  val add : t -> subcommand:string -> t
  val commands : t -> string list
  val to_string : t -> string
  val to_string_dots : t -> string
  val pop_help : t -> t
  val key : t Env.Key.t
end = struct
  type t = string list
  let empty = []
  let root = [Filename.basename Sys.argv.(0)]
  let add t ~subcommand = subcommand :: t
  let commands t = List.rev t
  let to_string t = unwords (commands t)
  let key = Env.Key.create ()
  let pop_help = function
    | "help" :: t -> t
    | _ -> assert false
  let to_string_dots t =
    let t =
      match t with
      | [] -> []
      | last :: init -> last :: List.map init ~f:(Fn.const ".")
    in
    to_string t
end

module Anon = struct

  module Grammar : sig
    type t
    val zero : t
    val one : string -> t
    val many : string -> t
    val maybe : t -> t
    val concat : t list -> t
    val usage : t -> string
    val ad_hoc : usage:string -> t
  end = struct

    type s = {usage : string; number : [`Fixed | `Variable] }

    type t = s option

    let usage = function
      | None -> ""
      | Some s -> s.usage

    let zero = None

    let one name = Some { usage = name; number = `Fixed }

    let many name =
      Some { usage = sprintf "[%s ...]" name; number = `Variable }

    let maybe = function
      | None -> None (* strange, but not non-sense *)
      | Some s ->
        Some { usage = sprintf "[%s]" s.usage; number = `Variable }

    let concat2 t1 t2 =
      match (t1, t2) with
      | (None, t) | (t, None) -> t
      | (Some s1, Some s2) ->
        let combined_usage = s1.usage ^ " " ^ s2.usage in
        match s1.number with
        | `Variable ->
            failwithf "the grammar %s for anonymous arguments \
              is non-sensical because there is the possibility for \
              arguments (%s) following a variable number of \
              arguments (%s)" combined_usage s2.usage s1.usage ()
        | `Fixed ->
            Some {
              usage = combined_usage;
              number = s2.number;
            }

    let rec concat = function
      | [] -> zero
      | t :: ts -> concat2 t (concat ts)

    let ad_hoc ~usage = Some { usage ; number = `Variable }
  end

  module Parser : sig
    type 'a t
    val one : name:string -> 'a Arg_type.arg_type -> 'a t
    val maybe : 'a t -> 'a option t
    val sequence : name:string -> 'a Arg_type.arg_type -> 'a list t
    val final_value : 'a t -> 'a
    val consume : 'a t -> string -> (Env.t -> Env.t) * 'a t
    val complete : 'a t -> Env.t -> part:string -> never_returns
    module For_opening : sig
      val return : 'a -> 'a t
      val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    end
  end = struct

    type 'a t =
      | Done of 'a
      | Test of (more:bool -> 'a t)
      | More of string (* name of the expected argument *)
          * (string -> (Env.t -> Env.t) * 'a t)
          * Completer.t

    let return a = Done a

    let rec (>>=) t f =
      match t with
      | Done a -> f a
      | Test g -> Test (fun ~more -> g ~more >>= f)
      | More (name, g, complete) ->
        let g x =
          let (upd, t) = g x in
          (upd, t >>= f)
        in
        More (name, g, complete)

    let one_assuming_more ~name {Arg_type.complete; parse = of_string; key} =
      More (name, (fun anon ->
        match of_string anon with
        | None -> die "failed to parse %s value %S" name anon ()
        | Some v ->
          let update env =
            Option.fold key ~init:env ~f:(fun env key -> Env.add env key v)
          in
          (update, Done v)), complete)

    let one ~name arg_type =
      Test (fun ~more ->
        if more then
          one_assuming_more ~name arg_type
        else
          die "missing anonymous argument: %s" name ())

    let maybe t =
      Test (fun ~more ->
        if more then t >>= fun a -> Done (Some a) else return None)

    let sequence ~name arg_type =
      let rec loop acc =
        Test (fun ~more ->
          if more then
            one_assuming_more ~name arg_type >>= fun v -> loop (v :: acc)
          else
            return (List.rev acc))
      in
      loop []

    let rec final_value = function
      | Done a -> a
      | Test f -> final_value (f ~more:false)
      | More (name, _, _) -> die "missing anonymous argument: %s" name ()

    let rec consume t arg =
      match t with
      | Done _ -> die "too many anonymous arguments" ()
      | Test f -> consume (f ~more:true) arg
      | More (_, f, _) -> f arg

    let rec complete t env ~part =
      match t with
      | Done _ -> die "too many anonymous arguments" ()
      | Test f -> complete (f ~more:true) env ~part
      | More (_, _, comp) -> Completer.run_and_exit comp env ~part

    module For_opening = struct
      let return = return
      let (>>=) = (>>=)
    end
  end

  open Parser.For_opening

  module Spec = struct

    type 'a t = {
      p : 'a Parser.t;
      grammar : Grammar.t;
    }

    let zero = {
      p = return ();
      grammar = Grammar.zero;
    }

    let t2 t1 t2 = {
      p = begin
        t1.p >>= fun a1 ->
        t2.p >>= fun a2 ->
        return (a1, a2)
      end;
      grammar = Grammar.concat [t1.grammar; t2.grammar];
    }

    let t3 t1 t2 t3 = {
      p = begin
        t1.p >>= fun a1 ->
        t2.p >>= fun a2 ->
        t3.p >>= fun a3 ->
        return (a1, a2, a3)
      end;
      grammar = Grammar.concat [t1.grammar; t2.grammar; t3.grammar];
    }

    let t4 t1 t2 t3 t4 = {
      p = begin
        t1.p >>= fun a1 ->
        t2.p >>= fun a2 ->
        t3.p >>= fun a3 ->
        t4.p >>= fun a4 ->
        return (a1, a2, a3, a4)
      end;
      grammar = Grammar.concat [t1.grammar; t2.grammar; t3.grammar; t4.grammar];
    }

    let (%:) name arg_type = {
      p = Parser.one ~name arg_type;
      grammar = Grammar.one name;
    }

    let maybe t = {
      p = Parser.maybe t.p;
      grammar = Grammar.maybe t.grammar;
    }

    let maybe_with_default default t =
      let t = maybe t in
      { t with p = t.p >>= fun v -> return (Option.value ~default v) }

    let sequence name arg_type = {
      p = Parser.sequence ~name arg_type;
      grammar = Grammar.many name;
    }

    let ad_hoc ~usage_arg = {
      p = Parser.sequence ~name:"THIS WILL NEVER BE PRINTED" Arg_type.string;
      grammar = Grammar.ad_hoc ~usage:usage_arg
    }
  end

end

type args = Nil | Cons of string * args | Complete of string

let rec list_of_args = function
  | Nil -> []
  | Cons (x, xs) -> x :: list_of_args xs
  | Complete x -> [x]

let rec args_ends_in_complete = function
  | Complete _ -> true
  | Nil -> false
  | Cons (_, args) -> args_ends_in_complete args

module Key_type = struct
  type t = Subcommand | Flag
  let to_string = function
    | Subcommand -> "subcommand"
    | Flag       -> "flag"
end

let lookup_expand map prefix key_type =
  match String.Map.find map prefix with
  | Some data -> (prefix, data)
  | None ->
    let alist = String.Map.to_alist map in
    match List.filter alist ~f:(fun (key, _) -> String.is_prefix key ~prefix) with
    | [(key, data)] -> (key, data)
    | [] ->
      die "unknown %s %s" (Key_type.to_string key_type) prefix ()
    | matches ->
      let matching_keys = List.map ~f:fst matches in
      die "%s %s is an ambiguous prefix: %s"
        (Key_type.to_string key_type) prefix (String.concat ~sep:", " matching_keys) ()

let lookup_expand_with_aliases map prefix key_type =
  let map =
    String.Map.of_alist
      (List.concat_map (String.Map.data map) ~f:(fun flag ->
        let {Flag.name; aliases; _} = flag in
        (name, flag) :: List.map aliases ~f:(fun alias -> (alias, flag))))
  in
  match map with
  | `Ok map -> lookup_expand map prefix key_type
  | `Duplicate_key flag -> failwithf "multiple flags named %s" flag ()

module Base = struct

  type t = {
    summary : string;
    readme : (unit -> string) option;
    flags : Flag.t String.Map.t;
    anons : Env.t -> (unit -> unit) Anon.Parser.t;
    usage : Anon.Grammar.t;
  }

  module Deprecated = struct
    let subcommand_cmp_fst (a, _) (c, _) =
      help_screen_compare a c

    let flags_help ?(display_help_flags = true) t =
      let flags = String.Map.data t.flags in
      let flags =
        if display_help_flags then flags else
          List.filter flags ~f:(fun f -> f.Flag.name <> "-help")
      in
      List.concat_map ~f:Flag.Deprecated.help flags
  end

  let summary t = t.summary

  let formatted_flags t = Format.sort (List.map (String.Map.data t.flags) ~f:Flag.align)

  let help_text ~path t =
    unparagraphs
      (List.filter_opt [
        Some t.summary;
        Some ("  " ^ Path.to_string path ^ " " ^ Anon.Grammar.usage t.usage);
        Option.map t.readme ~f:(fun thunk -> thunk ());
        Some "=== flags ===";
        Some (Format.to_string (formatted_flags t));
      ])

  let path_key = Env.Key.create ()
  let args_key = Env.Key.create ()
  let help_key = Env.Key.create ()

  let run t ~path ~args =
    let help_text = lazy (help_text ~path t) in
    let env = Env.empty in
    let env = Env.add env path_key path in
    let env = Env.add env args_key (list_of_args args) in
    let env = Env.add env help_key help_text in
    let rec loop env anons = function
      | Nil ->
        List.iter (String.Map.data t.flags) ~f:(fun flag ->
          flag.Flag.check_available_if_required ());
        Anon.Parser.final_value anons
      | Cons (arg, args) ->
        if String.is_prefix arg ~prefix:"-" then begin
          let flag = arg in
          let (flag, {Flag.action; _}) =
            lookup_expand_with_aliases t.flags flag Key_type.Flag
          in
          match action with
          | Flag.No_arg f ->
            let env = f env in
            loop env anons args
          | Flag.Arg (f, comp) ->
            begin match args with
            | Nil -> die "missing argument for flag %s" flag ()
            | Cons (arg, rest) ->
              let env = f arg env in
              loop env anons rest
            | Complete part ->
              never_returns (Completer.run_and_exit comp env ~part)
            end
          | Flag.Rest f ->
            if args_ends_in_complete args then exit 0;
            f (list_of_args args);
            loop env anons Nil
        end else begin
          let (env_upd, anons) = Anon.Parser.consume anons arg in
          let env = env_upd env in
          loop env anons args
        end
      | Complete part ->
        if String.is_prefix part ~prefix:"-" then begin
          List.iter (String.Map.keys t.flags) ~f:(fun name ->
            if String.is_prefix name ~prefix:part then print_endline name);
          exit 0
        end else
          never_returns (Anon.Parser.complete anons env ~part);
    in
    match (Result.try_with (fun () -> loop env (t.anons env) args)) with
    | Ok thunk -> thunk ()
    | Error exn ->
      print_endline (Lazy.force help_text);
      match exn with
      | Failed_to_parse_command_line msg -> prerr_endline msg; exit 1
      | _ -> raise exn


  module Spec = struct

    type ('a, 'b) t = {
      f : Env.t -> ('a -> 'b) Anon.Parser.t;
      usage : Anon.Grammar.t;
      flags : Flag.t list;
    }

    open Anon.Parser.For_opening

    let (++) t1 t2 = {
      f = (fun env ->
        t1.f env >>= fun f1 ->
        t2.f env >>= fun f2 ->
        return (fun x -> f2 (f1 x))
      );
      flags = t2.flags @ t1.flags;
      usage = Anon.Grammar.concat [t1.usage; t2.usage];
    }

    let step f = {
      f = (fun _env -> return f);
      flags = []; usage = Anon.Grammar.zero;
    }

    let const v = step (fun m -> m v)

    let path = {
      f = (fun env -> return (fun m -> m (Option.value_exn (Env.find env path_key))));
      flags = []; usage = Anon.Grammar.zero;
    }

    let args = {
      f = (fun env -> return (fun m -> m (Option.value_exn (Env.find env args_key))));
      flags = []; usage = Anon.Grammar.zero;
    }

    let help = {
      f = (fun env -> return (fun m -> m (Option.value_exn (Env.find env help_key))));
      flags = []; usage = Anon.Grammar.zero;
    }

    include Arg_type

    include struct
      open Anon.Spec
      type 'a anons = 'a t
      let (%:) = (%:)
      let maybe = maybe
      let maybe_with_default = maybe_with_default
      let sequence = sequence
      let zero = zero
      let t2 = t2
      let t3 = t3
      let t4 = t4

      let ad_hoc = ad_hoc

      let anon spec = {
        f = (fun _env ->
          spec.p >>= fun v ->
          return (fun k -> k v)
        );
        flags = [];
        usage = spec.grammar;
      }
    end

    include struct
      open Flag.Spec
      type 'a flag = 'a t
      let escape = escape
      let listed = listed
      let no_arg = no_arg
      module Deprecated = Deprecated
      let no_arg_register = no_arg_register
      let optional = optional
      let optional_with_default = optional_with_default
      let required = required

      let flag ?(aliases = []) name mode ~doc =
        let {read; action} = mode name in
        let check_available_if_required () = ignore (read ()) in
        {
          f = (fun _env -> return (fun k -> k (read ())));
          flags = [{ Flag.name; aliases; doc; action; check_available_if_required }];
          usage = Anon.Grammar.zero;
        }
    end

  end

end

type t =
  | Base of Base.t
  | Group of group

and group = {
  summary : string;
  readme : (unit -> string) option;
  subcommands : t String.Map.t
}

let assert_no_underscores flag_or_subcommand =
  if String.exists flag_or_subcommand ~f:(fun c -> c = '_') then
    failwithf "%s contains an underscore. Use a dash instead." flag_or_subcommand ()

let get_summary = function
  | Base base -> Base.summary base
  | Group {summary; _} -> summary

let command_menu subs =
  let subs = String.Map.to_alist subs in
  unparagraphs [
    "=== subcommands ===";
    Format.to_string
      (Format.sort
        (List.map subs ~f:(fun (name, summary) ->
          {Format.name; aliases = []; doc = summary})))
  ]

let group_help ~path ~summary ~readme subs =
  unparagraphs (List.filter_opt [
    Some summary;
    Some (String.concat ["  "; Path.to_string path; " SUBCOMMAND"]);
    Option.map readme ~f:(fun f -> f ());
    Some (command_menu subs);
  ])

let extend_map_exn map key_type ~key data =
  if String.Map.mem map key then
    failwithf "there is already a %s named %s" (Key_type.to_string key_type) key ();
  String.Map.add map ~key ~data

module Bailout_dump_flag = struct
  let add base ~name ~aliases ~text ~text_summary =
    let flags = base.Base.flags in
    let flags =
      extend_map_exn flags Key_type.Flag ~key:name
        { Flag.name; aliases;
          check_available_if_required = Fn.id;
          action = Flag.No_arg (fun env -> print_endline (text env); exit 0);
          doc = sprintf " print %s and exit" text_summary; }
    in
    {base with Base.flags}
end

let basic ~summary ?readme {Base.Spec.usage; flags; f} main =
  let anons env =
    let open Anon.Parser.For_opening in
    f env >>= fun k -> return (fun () -> k main)
  in
  let flags =
    match
      String.Map.of_alist
        (List.map flags ~f:(fun ({Flag.name; Flag.aliases; _} as flag) ->
          assert_no_underscores name;
          let name = flag_with_dash name in
          let aliases = List.map aliases ~f:(fun s ->
            assert_no_underscores s;
            flag_with_dash s)
          in
          (name, { flag with Flag.aliases; Flag.name })))
    with
    | `Duplicate_key flag -> failwithf "multiple flags named %s" flag ()
    | `Ok map -> map
  in
  let base = {Base.summary; readme; usage; flags; anons} in
  let base =
    Bailout_dump_flag.add base ~name:"-help" ~aliases:["-?"]
      ~text_summary:"this help text"
      ~text:(fun env -> Lazy.force (Option.value_exn (Env.find env Base.help_key)))
  in
  Base base


let help_subcommand ~summary ~readme subs =
  basic ~summary:"explain a given subcommand (perhaps recursively)"
    Base.Spec.(
      flag "-recursive" no_arg ~doc:" show subcommands of subcommands, etc."
      ++ flag "-flags" no_arg ~doc:" show flags as well"
      ++ flag "-expand-dots" no_arg ~doc:" expand sub-commands"
      ++ path
      ++ anon (maybe ("SUBCOMMAND" %: string))
    )
    (fun recursive show_flags expand_dots path cmd_opt ->
      let path_minus_help = Path.pop_help path in
      let path = if recursive then path_minus_help else Path.empty in
      let string_of_path = if expand_dots then Path.to_string else Path.to_string_dots in
      let rec gather_group path acc subs =
        let subs = if recursive then String.Map.remove subs "help" else subs in
        let alist = String.Map.to_alist subs in
        let alist =
          List.sort alist ~cmp:(fun a b -> help_screen_compare (fst a) (fst b))
        in
        List.fold alist ~init:acc ~f:(fun acc (subcommand, t) ->
          let path = Path.add path ~subcommand in
          let key = string_of_path path in
          let doc = get_summary t in
          let acc = Fqueue.enqueue acc {Format.name = key; doc; aliases = []} in
          if recursive then gather path acc t else acc)
      and
        gather path acc = function
          | Group {subcommands; _} -> gather_group path acc subcommands
          | Base base ->
            if show_flags then begin
              List.fold
                (List.filter (Base.formatted_flags base) ~f:(fun fmt ->
                  fmt.Format.name <> "-help"))
                ~init:acc ~f:(fun acc fmt ->
                  let path = Path.add path ~subcommand:fmt.Format.name in
                  let fmt = {fmt with Format.name = string_of_path path} in
                  Fqueue.enqueue acc fmt)
            end else
              acc
      in
      let menu =
        let q = Fqueue.empty in
        let q =
          match cmd_opt with
          | None ->
            Fqueue.enqueue (gather_group path q subs)
            { Format.name = "help [-r]";
              doc = "explain a given subcommand (perhaps recursively)";
              aliases = [] }
          | Some cmd ->
            if cmd = "help" then (* this is "help help" *) q
            else (* can special case help help if we want *)
            match String.Map.find subs cmd with
            | None ->
              die "unknown subcommand %s for command %s" cmd (Path.to_string path) ()
            | Some t -> gather path q t
        in
        Fqueue.to_list q
      in
      let text =
        unparagraphs (List.filter_opt [
          Some summary;
          Some (String.concat ["  "; Path.to_string path_minus_help; " SUBCOMMAND"]);
          Option.map readme ~f:(fun f -> f ());
          Some (if show_flags then "=== subcommands and flags ===" else "=== subcommands ===");
          Some (Format.to_string menu);
        ])
      in
      print_endline text)

let group ~summary ?readme alist =
  List.iter alist ~f:(fun (name, _) -> assert_no_underscores name);
  let subcommands =
    match String.Map.of_alist alist with
    | `Ok subs -> subs
    | `Duplicate_key name -> failwithf "multiple subcommands named %s" name ()
  in
  Group {summary; readme; subcommands}

INCLUDE "version_defaults.mlh"
module Version_info = struct

  let command ~version ~build_info =
    basic ~summary:"Print version information"
      Base.Spec.(
        flag "-version" no_arg ~doc:" print the version of this build"
        ++ flag "-build-info" no_arg ~doc:" print build info for this build"
      )
      (fun version_flag build_info_flag ->
        begin match
          if build_info_flag then `Build_info else
          if version_flag then `Version else begin
            eprintf "(no option given - printing version)\n%!";
            `Version
          end
        with
        | `Version    -> print_endline version
        | `Build_info -> print_endline build_info
        end;
        exit 0)

  let add
      ?(version = DEFAULT_VERSION)
      ?(build_info = DEFAULT_BUILDINFO)
      unversioned =
    match unversioned with
    | Base base ->
      let base =
        Bailout_dump_flag.add base ~name:"-version" ~aliases:[]
          ~text_summary:"the version of this build" ~text:(Fn.const version)
      in
      let base =
        Bailout_dump_flag.add base ~name:"-build-info" ~aliases:[]
          ~text_summary:"info about this build" ~text:(Fn.const build_info)
      in
      Base base
    | Group group ->
      let subcommands =
        extend_map_exn group.subcommands Key_type.Subcommand ~key:"version"
          (command ~version ~build_info)
      in
      Group {group with subcommands}

end

(* clear the setting of environment variable associated with command-line
   completion so that subprocesses don't see them. *)
let getenv_and_clear var =
  let value = Sys.getenv var in
  if Option.is_some value then Unix.unsetenv var;
  value

let dump_autocomplete_function () =
  let fname = sprintf "_jsautocom_%s" (Pid.to_string (Unix.getpid ())) in
  printf
"function %s {
  export COMP_CWORD
  COMP_WORDS[0]=%s
  COMPREPLY=($(\"${COMP_WORDS[@]}\"))
}
complete -F %s %s
%!" fname Sys.argv.(0) fname Sys.argv.(0)

let args_of_list = function
  | [] -> failwith "missing executable name"
  | _cmd :: args ->
    match getenv_and_clear "COMMAND_OUTPUT_INSTALLATION_BASH" with
    | Some _ -> dump_autocomplete_function (); exit 0
    | None ->
      match
        Option.bind (getenv_and_clear "COMP_CWORD") (fun i ->
          Option.try_with (fun () -> Int.of_string i))
      with
      | None ->
        List.fold_right args ~init:Nil
          ~f:(fun arg args -> Cons (arg, args))
      | Some i ->
        let args = List.take (args @ [""]) i in
        List.fold_right args ~init:Nil ~f:(fun arg args ->
          match args with
          | Nil -> Complete arg
          | _ -> Cons (arg, args))

let get_args () = Array.to_list Sys.argv |! args_of_list

 let rec dispatch t ~path ~args =
  match t with
  | Base base -> Base.run base ~path ~args
  | Group {summary; readme; subcommands = subs} ->
    match args with
    | Nil ->
      let subs = String.Map.map subs ~f:get_summary in
      eprintf "%s\n%!" (command_menu subs);
      die "missing subcommand for command %s" (Path.to_string path) ()
    | Cons (sub, rest) ->
      let (sub, rest) =
        match (sub, rest) with
        | ("-help", Nil) ->
          let subs = String.Map.map subs ~f:get_summary in
          print_endline (group_help ~path ~summary ~readme subs);
          exit 0
        | ("-help", Cons (sub, rest)) -> (sub, Cons ("-help", rest))
        | _ -> (sub, rest)
      in
      let (sub, t) = lookup_expand subs sub Key_type.Subcommand in
      dispatch t ~path:(Path.add path ~subcommand:sub) ~args:rest
    | Complete part ->
      List.iter (String.Map.keys subs) ~f:(fun name ->
        if String.is_prefix name ~prefix:part then print_endline name);
      exit 0

let rec run ?version ?build_info ?argv t =
  let t = Version_info.add t ?version ?build_info in
  let t = match t with
  | Base _ -> t
  | Group {summary; readme; subcommands} ->
    let subcommands =
      extend_map_exn subcommands Key_type.Subcommand ~key:"help"
        (help_subcommand ~summary ~readme subcommands)
    in
    Group {summary; readme; subcommands}
  in
  try
    let args =
      match argv with
      | Some x -> args_of_list x
      | None -> get_args ()
    in
    dispatch t ~path:Path.root ~args
  with
  | Failed_to_parse_command_line msg ->
    prerr_endline msg;
    exit 1

module Spec = struct
  include Base.Spec
  let path () = step (fun m path -> m (Path.commands path)) ++ path
end

module Deprecated = struct

  module Spec = Spec.Deprecated

  let rec args_of_list = function
  | [] -> Nil
  | f :: r -> Cons (f, args_of_list r)

  let summary = get_summary

  let get_flag_names = function
    | Base base -> base.Base.flags |! String.Map.keys
    | Group _ -> assert false

  let help_recursive ~cmd ~with_flags ~expand_dots t s =
    let rec help_recursive_rec ~cmd t s =
      let new_s = s ^ (if expand_dots then cmd else ".") ^ " " in
      match t with
      | Base base ->
        let base_help = s ^ cmd, summary (Base base) in
        if with_flags then
          base_help ::
            List.map ~f:(fun (flag, h) -> (new_s ^ flag, h))
              (List.sort ~cmp:Base.Deprecated.subcommand_cmp_fst
                (Base.Deprecated.flags_help ~display_help_flags:false base))
        else
          [base_help]
      | Group {summary; subcommands; _ } -> (s ^ cmd, summary) :: (List.concat
        (List.map
          (List.sort ~cmp:Base.Deprecated.subcommand_cmp_fst (String.Map.to_alist subcommands))
          ~f:(fun (cmd', t) -> help_recursive_rec ~cmd:cmd' t new_s)))
    in
    help_recursive_rec ~cmd t s

  let run t ~cmd ~args ~is_help ~is_help_rec ~is_help_rec_flags ~is_expand_dots =
    let path_strings = String.split cmd ~on: ' ' in
    let path = List.fold path_strings ~init:Path.empty
      ~f:(fun p subcommand -> Path.add p ~subcommand)
    in
    let args = if is_expand_dots then "-expand-dots" :: args else args in
    let args = if is_help_rec_flags then "-flags" :: args else args in
    let args = if is_help_rec then "-r" :: args else args in
    let args = if is_help then "help" :: args else args in
    let args = args_of_list args in
    let t = match t with
    | Base _ -> t
    | Group {summary; readme; subcommands} ->
      let subcommands =
        extend_map_exn subcommands Key_type.Subcommand ~key:"help"
          (help_subcommand ~summary ~readme subcommands)
      in
      Group {summary; readme; subcommands}
    in
    dispatch t ~path ~args

(*
  let autocomplete _ = None
  let autocomplete_of_extended_autocomplete f _ ~part:_ = f (list_of_args (get_args ()))
*)

end

