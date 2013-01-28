open Core.Std

module Anons_grammar : sig
  type t
  val empty : t
  val atom : string -> t
  val many : string -> t
  val maybe : t -> t
  val concat : t -> t -> t
  val usage_arg : t -> string
end = struct

  type number_of_anons = Fixed | Variable

  type t = (string * number_of_anons) option

  let empty = None

  let atom name = Some (name, Fixed)

  let many name = Some (Printf.sprintf "[%s ...]" name, Variable)

  let maybe t =
    match t with
    | None -> None (* strange, but not meaningless *)
    | Some (usage, _) -> Some (Printf.sprintf "[%s]" usage, Variable)

  let usage_arg = function
    | None -> ""
    | Some (usage_arg, _) -> usage_arg

  let concat anons1 anons2 =
    match anons1, anons2 with
    | None, anons | anons, None -> anons
    | Some (grammar1, num1), Some (grammar2, num2) ->
      match num1 with
      | Fixed -> Some (grammar1 ^ " " ^ grammar2, num2)
      | Variable ->
        failwithf "you may not specify any more anonymous arguments \
          (e.g. %s) after optional anonymous arguments (e.g. %s)"
          grammar2 grammar1 ()

end

type ('a, 'b) t = {
  f : ('a Lazy.t * string list -> 'b Lazy.t * string list);
  flags : unit Command.Flag.t list;
  anons : Anons_grammar.t;
}

let id x = x
let cmp f g x = f (g x)

let (++) t1 t2 = {
  f = cmp t2.f t1.f;
  flags = t2.flags @ t1.flags;
  anons = Anons_grammar.concat t1.anons t2.anons;
}

let step f = {
  f = (fun (thunk, anons) -> (lazy (f (Lazy.force thunk)), anons));
  flags = [];
  anons = Anons_grammar.empty;
}

let const x = step (fun k -> k x)

let empty () = step Fn.id

let either name = step (fun f x1 x2 ->
  match (x1, x2) with
  | None,   None   -> f None
  | Some x, None
  | None,   Some x -> f (Some x)
  | Some _, Some _ ->
    failwithf "Please specify at most one %s argument" name ())

type 'a parse = string -> 'a

let string : string parse = Fn.id
let int    : int    parse = Int.of_string
let float  : float  parse = Float.of_string
let date   : Date.t parse = Date.of_string
let sexp   : Sexp.t parse = Sexp.of_string

let parse_aux ~name ~of_string arg =
  match Result.try_with (fun () -> of_string arg) with
  | Ok v -> v
  | Error exn ->
    failwithf "failed to parse %s value %S -- %s" name arg (Exn.to_string exn) ()

module Flag = struct

  type 'a state = {
    action : unit Command.Flag.Action.t;
    read : unit -> 'a;
  }

  type 'a t = string -> 'a state

  let arg_flag name of_string read write =
    { read = read;
      action =
        Command.Flag.Action.arg (fun () arg ->
          write (parse_aux ~name ~of_string arg));
    }

  let write_option name v arg =
    match !v with
    | None -> v := Some arg
    | Some _ -> failwithf "flag %s passed more than once" name ()

  let required_value ?default of_string name =
    let v = ref None in
    let read () =
      match !v with
      | Some v -> v
      | None ->
        match default with
        | Some v -> v
        | None -> failwithf "missing required flag %s" name ()
    in
    let write arg = write_option name v arg in
    arg_flag name of_string read write

  let required of_string name =
    required_value of_string name

  let optional_with_default default of_string name =
    required_value ~default of_string name

  let optional of_string name =
    let v = ref None in
    let read () = !v in
    let write arg = write_option name v arg in
    arg_flag name of_string read write

  let no_arg name =
    let v = ref `Absent in
    let read () = !v in
    let write () =
      match !v with
      | `Absent -> v := `Present
      | `Present -> failwithf "flag %s passed more than once" name ()
    in
    { read; action = Command.Flag.Action.noarg write }

  let listed of_string name =
    let v = ref [] in
    let read () = List.rev !v in
    let write arg = v := arg :: !v in
    arg_flag name of_string read write

  let map ~f t name =
    let s = t name in
    { s with read = fun () -> f (s.read ()) }

  let no_arg_bool name =
    map no_arg name ~f:(function
      | `Present -> true
      | `Absent -> false)

  let capture_remaining_command_line _ =
    let cell = ref None in
    let action = Command.Flag.Action.rest (fun () cmd_line -> cell := Some cmd_line) in
    let read () = !cell in
    { action; read }

  let flag name mode ~doc =
    let state = mode name in
    {
      f = (fun (k, anons) ->
        let v = state.read () in
        (lazy (Lazy.force k v), anons));
      flags = [Command.Flag.create ~name ~doc state.action];
      anons = Anons_grammar.empty;
    }

end

include struct
  open Flag
  let capture_remaining_command_line = capture_remaining_command_line
  let flag = flag
  let listed = listed
  let no_arg = no_arg
  let no_arg_bool = no_arg_bool
  let optional = optional
  let optional_with_default = optional_with_default
  let required = required
end

module Anons = struct

  type 'a t = {
    m : (string list -> 'a * string list);
    grammar : Anons_grammar.t;
  }

  let return a  = fun anons -> (a, anons)
  let (>>=) m f = fun anons -> let (a, anons) = m anons in f a anons
  let map ~f t = { t with m = t.m >>= fun a -> return (f a) }

  let parse (name, of_string) = function
    | [] -> failwithf "missing anonymous argument %s" name ()
    | anon :: anons -> (parse_aux ~name ~of_string anon, anons)

  let (%:) name of_string = {
    m = parse (name, of_string);
    grammar = Anons_grammar.atom name;
  }

  let zero = {
    m = return ();
    grammar = Anons_grammar.empty;
  }

  let (+) = Anons_grammar.concat

  let t2 t1 t2 = {
    m = begin
      t1.m >>= fun a1 ->
      t2.m >>= fun a2 ->
      return (a1, a2)
    end;
    grammar = t1.grammar + t2.grammar;
  }

  let t3 t1 t2 t3 = {
    m = begin
      t1.m >>= fun a1 ->
      t2.m >>= fun a2 ->
      t3.m >>= fun a3 ->
      return (a1, a2, a3)
    end;
    grammar = t1.grammar + (t2.grammar + t3.grammar);
  }

  let t4 t1 t2 t3 t4 = {
    m = begin
      t1.m >>= fun a1 ->
      t2.m >>= fun a2 ->
      t3.m >>= fun a3 ->
      t4.m >>= fun a4 ->
      return (a1, a2, a3, a4)
    end;
    grammar = t1.grammar + (t2.grammar + (t3.grammar + t4.grammar));
  }

  let maybe t = {
    m = (function
      | [] -> (None, [])
      | anons -> let (a, anons) = t.m anons in (Some a, anons));
    grammar = Anons_grammar.maybe t.grammar;
  }

  let maybe_with_default default t =
    map (maybe t) ~f:(function
      | None -> default
      | Some v -> v)

  let evermore a =
    let rec loop acc anons =
      match anons with
      | [] -> (List.rev acc, [])
      | _ ->
        let (v, anons) = parse a anons in
        loop (v :: acc) anons
    in
    loop []

  let many name of_string = {
    m = evermore (name, of_string);
    grammar = Anons_grammar.many name;
  }

  let anon t = {
    f = (fun (k, anons) ->
      let (v, remaining_anons) = t.m anons in
      (lazy (Lazy.force k v), remaining_anons)
    );
    flags = [];
    anons = t.grammar;
  }

end

include struct
  open Anons
  let anon = anon
  let (%:) = (%:)
  let many = many
  let maybe = maybe
  let maybe_with_default = maybe_with_default
  let t2 = t2
  let t3 = t3
  let t4 = t4
  let zero = zero
end

let cmd ~summary ?readme ?autocomplete ?(global_flags = []) t main =
  let flags = t.flags @ global_flags in
  let flag_names = List.map ~f:Command.Flag.name flags in
  Option.iter (List.find_a_dup ~compare:String.compare flag_names) ~f:(fun dup ->
    failwithf "Duplicate flag name: %S" dup ());
  Command.create ~summary ?readme ?autocomplete
    ~usage_arg:(Anons_grammar.usage_arg t.anons)
    ~init:Fn.id
    ~flags
    ~final:(fun () anons ->
      let (thunk, remaining_anons) = t.f (lazy main, anons) in
      match remaining_anons with
      | [] -> thunk
      | _ -> failwithf "%d too many anonymous arguments" (List.length remaining_anons) ()
    )
    Lazy.force

