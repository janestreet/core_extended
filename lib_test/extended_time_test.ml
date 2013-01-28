open OUnit
open Core.Std
module Time = Core_extended.Std.Time

let test =
  "extended_time.Extended_span" >:::
    [ "to_string_hum" >::
        (fun () ->
          let t secs str =
            (string_of_float secs) @?
            (Time.Extended_span.to_string_hum (sec secs) = str)
          in
          t 0. "0:00:00.000";
          t 0.075 "0:00:00.075";
          t 3.075 "0:00:03.075";
          t 163.075 "0:02:43.075";
          t 3763.075 "1:02:43.075";
          t 432163.075 "120:02:43.075";
          t (-. 432163.075) "-120:02:43.075";
        );
    ]
