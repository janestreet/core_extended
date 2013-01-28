open Core.Std

(* The type of things that can be put into a trie.  We are nice to consumers of the
   library by only requiring them to produce an iter function.  But this means that our
   internal code is more unpleasant (and more full of refs) because we don't really know
   when iteration will end *)
module type Key = sig
  type t

  module Part : sig
    type t with sexp
    val hash    : t -> int
    val compare : t -> t -> int
  end

  val iter  : t -> f:(Part.t -> unit) -> unit
end

module type S = sig
  module Key : sig
    type t
  end

  module Part : sig
    type t

    include Hashable with type t := t
  end

  type t = Node of (bool * t) Part.Table.t 

  val create : unit -> t

  (* [contains t key] returns true if [key] has previously been added to [t] and not
     subsequently removed *)
  val contains : t -> Key.t -> bool

  (* [add t key] add [key] to the [t] *)
  val add : t -> Key.t -> unit

  (* [remove t key] removes [key] from [t] *)
  val remove : t -> Key.t -> unit

  (* [render_as_regexp t f] renders the trie as an optimized regular expression *)
  val render_as_regexp : 
       t 
    -> capture_parts:bool
    -> to_quoted_string:(Part.t -> string) 
    -> string
end

module Make(T : Key) = struct
  module Key = struct
    type t = T.t
  end

  module Part = struct
    module Z = struct
      include T.Part
    end

    include Z
    include (Hashable.Make (Z) : Hashable.S with type t := t)
  end

  type t = Node of (bool * t) Part.Table.t 

  let create () = Node (Part.Table.create ())

  let add (Node t) key =
    let t              = ref t in
    let set_terminator = ref (fun () -> ()) in
    let last_part      = ref None in
    T.iter key ~f:(fun part -> 
      last_part := Some part;
      match Part.Table.find !t part with
      | Some (_,Node next) -> t := next
      | None ->
        let reified_t = !t in
        let next      = Part.Table.create () in
        Part.Table.replace reified_t ~key:part ~data:(false, Node next);
        set_terminator := (fun () ->
          Part.Table.replace reified_t ~key:part ~data:(true, Node next));
        t := next);
    !set_terminator ();
  ;;

  let contains (Node t) key =
    let t          = ref t in
    let terminated = ref false in
    with_return (fun return ->
      T.iter key ~f:(fun part -> 
        match Part.Table.find !t part with
        | None -> return.return false
        | Some (is_terminator, Node next) ->
          terminated := is_terminator;
          t := next);
      !terminated)
  ;;

  let remove (Node t) key =
    let t                = ref t in
    let delete           = ref (fun () -> ()) in
    let clear_terminator = ref (fun () -> ()) in
    with_return (fun return ->
      T.iter key ~f:(fun part -> 
        match Part.Table.find !t part with
        | None -> return.return ()
        | Some (_,Node next) ->
          if Part.Table.length !t = 1 then begin
            !delete ();
            return.return ()
          end else begin
            let reified_t = !t in
            delete := (fun () -> Part.Table.remove reified_t part);
            clear_terminator := (fun () ->
              match Part.Table.find reified_t part with
              | None -> assert false
              | Some (_,Node next) -> 
                Part.Table.replace !t ~key:part ~data:(false,Node next));
            t := next
          end);
      !clear_terminator ())

  module Regexp = struct
    type t =
      | Token of Part.t
      | Alt of t list
      | Seq of t * t
      | Maybe of t
    with sexp

    (* not tail recursive *)
    let rec of_t (Node t) =
      Alt (List.map (Part.Table.to_alist t) 
        ~f:(fun (c, (terminates, rest)) -> 
          if terminates then
            Seq (Token c, Maybe (of_t rest))
          else
            Seq (Token c, of_t rest)))

    let render t ~capture_parts ~to_quoted_string =
      let open_group =
        if capture_parts then "("
        else "(?:"
      in
      let rec render = function
        | Token c -> (to_quoted_string c)
        | Alt [] -> ""
        | Alt [t] -> render t
        | Alt ts -> 
            let sub_expressions = List.map ts ~f:render in
            if List.for_all sub_expressions ~f:(fun s -> String.length s = 1) then
              "[" ^ String.concat ~sep:"" sub_expressions ^ "]"
            else
              open_group ^ String.concat ~sep:"|" (List.map ~f:render ts) ^ ")"
        | Seq (t, t') -> render t ^ render t'
        | Maybe (Alt []) -> ""
        | Maybe t -> open_group ^ render t ^ ")?"
      in
      render t
  end
  let render_as_regexp t ~capture_parts ~to_quoted_string = 
    Regexp.render (Regexp.of_t t) ~capture_parts ~to_quoted_string
end

module String_trie = Make(struct
  type t = String.t

  module Part = struct
    include Char
  end

  let equal = String.equal
  let iter = String.iter
end)

