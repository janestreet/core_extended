open Core.Std

module Extended_date : sig
  (**
     Shortcut for calling Core_extended.Unix.strftime without having to
     create a Time.t and convert it to a Unix.tm.

     [format "%Y-%m-%d" t] will return "YYYY-MM-DD"
  *)
  val format : ?ofday:Time.Ofday.t -> string -> Date.t -> string
end

module Extended_span : sig
  (**
     Convert a time span to a human-readable string, e.g. "1:23:45.778"
     (versus "1.396h" from [Time.Span.to_string]).
  *)
  val to_string_hum : Time.Span.t -> string
end
