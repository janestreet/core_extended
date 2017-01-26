(** Translating English into useful data structures *)

open! Core

(** [parse_int s]: Like standard int_of_string, but try to convert the first
    twenty english numbers (eg. "one", "two", ... "twenty"). Only lowercase strings are
    parsed.

    [parse_int "thirteen"]
    [- : int = 13]

    [parse_int "5296"]
    [- : int = 5296]

    [parse_int "twenty five"]
    [Exception: (Failure int_of_string).]
*)
val parse_int : string -> int

(** [parse_date s]: Convert a date in plain english (eg. "yesterday," "tomorrow," etc.) to
    a [Date.t]. Case is ignored in the input.

    [parse_date "Yesterday"]
    [parse_date "6 8 2010"]
    [parse_date "4 days hence"]
    [parse_date "24 weekdays ago"]

    You can specify the optional argument to interpret "today", "yesterday" etc. relative
    to the given date, otherwise they are relative to [Date.today ~zone:Time.Zone.local].
*)
val parse_date : ?relative_to:Date.t -> string -> Date.t

(* just uses [parse_date] for the date part *)
val parse_time : string -> Time.t
