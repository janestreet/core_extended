open Core.Std

module type Key = sig
  type t

  module Part : sig
    type t with sexp
    val hash  : t -> int
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

module Make(T : Key) : S
  with type Key.t = T.t
  with type Part.t = T.Part.t

module String_trie : S
  with type Key.t = String.t
  with type Part.t = Char.t
