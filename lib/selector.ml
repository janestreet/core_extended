open Core.Std
open Sexplib.Type



module type Selector = sig
  type selector
  type value

  val eval : selector -> value -> bool
end

module Date_selector = struct
  type t =
    | GT of Date.t
    | LT of Date.t
    | Between of Date.t * Date.t
    | On of Date.t
    with sexp
  type selector = t
  type value = Date.t

  let t_of_sexp sexp =
    match sexp with
    | Atom _ as d -> On (Date.t_of_sexp d)
    | List [Atom ">"; Atom _ as d]      -> GT (Date.t_of_sexp d)
    | List [Atom ">="; Atom _ as d]     -> GT (Date.add_days (Date.t_of_sexp d) (-1))
    | List [Atom "<"; Atom _ as d]      -> LT (Date.t_of_sexp d)
    | List [Atom "<="; Atom _ as d]     -> LT (Date.add_days (Date.t_of_sexp d) (1))
    | List [Atom _ as d1; Atom _ as d2] ->
      Between ((Date.t_of_sexp d1), (Date.t_of_sexp d2))
    | _ -> t_of_sexp sexp

  let eval t d =
    match t with
    | GT gtd           -> Date.(>) d gtd
    | LT ltd           -> Date.(<) d ltd
    | Between (d1, d2) -> Date.(>=) d d1 && Date.(<=) d d2
    | On ond           -> Date.(=) d ond
end

module String_selector = struct
  module Regexp : sig
    type t with sexp

    val of_regexp : string -> t
    val to_string : t -> string
    val matches : t -> string -> bool
    val to_regexp : t -> Pcre.regexp
  end = struct
    type t = string * Pcre.regexp

    let of_regexp s = s, Pcre.regexp s

    let t_of_sexp sexp =
      let fail () = of_sexp_error "expected string bounded with / on both sides" sexp in
      match sexp with
      | List _ -> of_sexp_error "expected Atom" sexp
      | Atom s ->
        if String.length s < 2 then fail ()
        else if s.[0] = '/' && s.[String.length s - 1] = '/' then
          let s = String.sub s ~pos:1 ~len:(String.length s - 2) in
          of_regexp s
        else fail ()

    let sexp_of_t (s, _) = Sexp.Atom ("/" ^ s ^ "/")
    let to_string (s, _) = s
    let to_regexp (_, p) = p
    let matches (_, rex) s = Pcre.pmatch ~rex s
  end

  type t =
    | Equal of string list
    | Matches of Regexp.t list
    | Mixed of [ `Regexp of Regexp.t | `Literal of string ] list
    with of_sexp
  type selector = t
  type value = String.t

  let t_of_sexp sexp =
    let parse_atom a =
      match a with
      | List _ -> assert false
      | Atom s ->
        if String.length s >= 1 && s.[0] = '/' then `Regexp (Regexp.t_of_sexp a)
        else `Literal s
    in
    try
      match sexp with
      | Atom _ as a ->
        begin match parse_atom a with
        | `Regexp r -> Matches [r]
        | `Literal s -> Equal [s]
        end
      | List l ->
        Mixed
          (List.map l ~f:(fun sexp ->
            match sexp with
            | List _ -> of_sexp_error "expected Atom" sexp
            | Atom _ as a -> parse_atom a))
    with
    | e -> try t_of_sexp sexp with _ -> raise e

  let eval t s =
    match t with
    | Equal el -> Option.is_some (List.find el ~f:(fun e -> e = s))
    | Matches ml -> Option.is_some (List.find ml ~f:(fun rex -> Regexp.matches rex s))
    | Mixed ml ->
      Option.is_some (List.find ml ~f:(function
        | `Regexp rex -> Regexp.matches rex s
        | `Literal l -> l = s))
end

module String_list_selector = struct
  type t = string list with sexp
  type selector = t
  type value = string

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s -> [s]
    | _ -> t_of_sexp sexp

  let eval t s =
    match List.find t ~f:(fun m -> m = s) with
    | None   -> false
    | Some _ -> true
end
