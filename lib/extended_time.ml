open Core.Std

module Extended_date = struct
  let format ?(ofday=Time.Ofday.start_of_day) s t = 
    Time.format (Time.of_local_date_ofday t ofday) s
end

module Extended_span = struct
  let to_string_hum (t : Time.Span.t) =
    let sign_str = 
      match Float.sign (t :> float) with
      | Float.Sign.Neg -> "-"
      | Float.Sign.Zero | Float.Sign.Pos -> ""
    in
    let rest = 
      match Float.classify (t :> float) with
      | Float.Class.Subnormal | Float.Class.Zero -> "0:00:00.000"
      | Float.Class.Infinite -> "inf"
      | Float.Class.Nan -> "nan"
      | Float.Class.Normal ->
          let parts = Time.Span.to_parts t in
          let module P = Time.Span.Parts in
          sprintf "%d:%02d:%02d.%03d" parts.P.hr parts.P.min parts.P.sec parts.P.ms
    in
    sign_str ^ rest
end
