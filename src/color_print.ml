open Core

let ansi_regexp = Memo.unit (fun () -> Re2.create_exn "\027\\[.*?m")

let ansi_capture_regexp = Memo.unit (fun () -> Re2.create_exn "(\027\\[.*?m)")
let normal_capture_regexp = Memo.unit (fun () -> Re2.create_exn "(\027\\[0m)")

let normal str =
  Re2.rewrite_exn (ansi_regexp ()) ~template:"" str

let add_after_ansi str ~code =
  Re2.rewrite_exn (ansi_capture_regexp ()) ~template:("\\1" ^ code) str
let add_after_normal str ~code =
  Re2.rewrite_exn (normal_capture_regexp ()) ~template:("\\1" ^ code) str

let ansi_code ~code = "\027[" ^ code ^ "m"

let normal_code    = ansi_code ~code:"0"
let bold_code      = ansi_code ~code:"1"
let underline_code = ansi_code ~code:"4"
let red_code       = ansi_code ~code:"31"
let green_code     = ansi_code ~code:"32"
let yellow_code    = ansi_code ~code:"33"
let blue_code      = ansi_code ~code:"34"
let magenta_code   = ansi_code ~code:"35"
let cyan_code      = ansi_code ~code:"36"
let inverse_code   = ansi_code ~code:"7"

let float_to_int x ~max =
    if x <= 0. then 0
    else if x >= 1. then max
    else Float.iround_exn ~dir:`Nearest (x *. Float.of_int max)

let gray_code ~brightness =
  let brightness = float_to_int brightness ~max:23 in
  ansi_code ~code:("38;5;" ^ Int.to_string (brightness + 232))
let rgbint_code ~r ~g ~b =
  ansi_code ~code:("38;5;" ^ Int.to_string (16 + r*36 + g*6 + b))
let rgb_code ~r ~g ~b =
  let r = float_to_int r ~max:5 in
  let g = float_to_int g ~max:5 in
  let b = float_to_int b ~max:5 in
  rgbint_code ~r ~g ~b

type color = [
| `Black | `Gray | `Light_gray | `White
| `Dark_red | `Red | `Pink | `Light_pink
| `Orange | `Amber
| `Dark_yellow | `Gold | `Yellow | `Khaki | `Wheat
| `Chartreuse | `Green_yellow
| `Dark_green | `Green | `Light_green | `Bright_green
| `Spring_green | `Medium_spring_green
| `Dark_cyan | `Sea_green | `Cyan | `Turquoise | `Pale_turquoise
| `Dodger_blue | `Deep_sky_blue
| `Dark_blue | `Blue | `Light_slate_blue | `Light_steel_blue
| `Blue_violet | `Violet
| `Dark_magenta | `Purple | `Magenta | `Orchid | `Plum
| `Rose | `Deep_pink
] [@@deriving sexp, bin_io]

let color_code ~(color:color) =
  let (r,g,b) =
    match (color:color) with
    | `Black -> (0,0,0) | `Gray -> (2,2,2) | `Light_gray -> (3,3,3) | `White -> (5,5,5)
    | `Dark_red -> (2,0,0) | `Red -> (5,0,0) | `Pink -> (5,2,2) | `Light_pink -> (5,3,3)
    | `Orange -> (5,2,0) | `Amber -> (5,3,0)
    | `Dark_yellow -> (2,2,0) | `Gold -> (3,3,0) | `Yellow -> (5,5,0) | `Khaki -> (5,5,2) | `Wheat -> (5,5,3)
    | `Chartreuse -> (2,5,0) | `Green_yellow -> (3,5,0)
    | `Dark_green -> (0,2,0) | `Green -> (0,5,0) | `Light_green -> (2,5,2) | `Bright_green -> (3,5,3)
    | `Spring_green -> (0,5,2) | `Medium_spring_green -> (0,5,3)
    | `Dark_cyan -> (0,2,2) | `Sea_green -> (0,3,3) | `Cyan -> (0,5,5) | `Turquoise -> (2,5,5) | `Pale_turquoise -> (3,5,5)
    | `Dodger_blue -> (0,2,5) | `Deep_sky_blue -> (0,3,5)
    | `Dark_blue -> (0,0,2) | `Blue -> (0,0,5) | `Light_slate_blue -> (2,2,5) | `Light_steel_blue -> (3,3,5)
    | `Blue_violet -> (2,0,5) | `Violet -> (3,0,5)
    | `Dark_magenta -> (2,0,2) | `Purple -> (3,0,3) | `Magenta -> (5,0,5) | `Orchid -> (5,2,5) | `Plum -> (5,3,5)
    | `Rose -> (5,0,2) | `Deep_pink -> (5,0,3)
  in
  rgbint_code ~r ~g ~b


let wrap ?(override=false) str ~code =
  code
  ^ (if override
     then add_after_ansi str ~code
     else add_after_normal str ~code)
  ^ normal_code

let bold      ?override str = wrap ?override str ~code:bold_code
let underline ?override str = wrap ?override str ~code:underline_code
let red       ?override str = wrap ?override str ~code:red_code
let green     ?override str = wrap ?override str ~code:green_code
let yellow    ?override str = wrap ?override str ~code:yellow_code
let blue      ?override str = wrap ?override str ~code:blue_code
let magenta   ?override str = wrap ?override str ~code:magenta_code
let cyan      ?override str = wrap ?override str ~code:cyan_code
let inverse   ?override str = wrap ?override str ~code:inverse_code

let gray  ?override str ~brightness = wrap ?override str ~code:(gray_code ~brightness)
let rgb   ?override str ~r ~g ~b    = wrap ?override str ~code:(rgb_code ~r ~g ~b)
let color ?override str ~color      = wrap ?override str ~code:(color_code ~color)

let wrap_print ?override ~code fmt =
  Printf.ksprintf (fun str -> print_string (wrap ?override str ~code)) fmt

let bold_printf      ?override fmt = wrap_print ?override ~code:bold_code      fmt
let underline_printf ?override fmt = wrap_print ?override ~code:underline_code fmt
let inverse_printf   ?override fmt = wrap_print ?override ~code:inverse_code   fmt
let red_printf       ?override fmt = wrap_print ?override ~code:red_code       fmt
let yellow_printf    ?override fmt = wrap_print ?override ~code:yellow_code    fmt
let green_printf     ?override fmt = wrap_print ?override ~code:green_code     fmt
let blue_printf      ?override fmt = wrap_print ?override ~code:blue_code      fmt
let magenta_printf   ?override fmt = wrap_print ?override ~code:magenta_code   fmt
let cyan_printf      ?override fmt = wrap_print ?override ~code:cyan_code      fmt

let gray_printf ?override ~brightness fmt = wrap_print ?override ~code:(gray_code ~brightness) fmt
let rgb_printf ?override ~r ~g ~b     fmt = wrap_print ?override ~code:(rgb_code ~r ~g ~b)     fmt
let color_printf ?override ~color     fmt = wrap_print ?override ~code:(color_code ~color)     fmt

let wrap_sprint ?override ~code fmt =
  Printf.ksprintf (fun str -> wrap ?override str ~code) fmt

let bold_sprintf      ?override fmt = wrap_sprint ?override ~code:bold_code      fmt
let underline_sprintf ?override fmt = wrap_sprint ?override ~code:underline_code fmt
let inverse_sprintf   ?override fmt = wrap_sprint ?override ~code:inverse_code   fmt
let red_sprintf       ?override fmt = wrap_sprint ?override ~code:red_code       fmt
let yellow_sprintf    ?override fmt = wrap_sprint ?override ~code:yellow_code    fmt
let green_sprintf     ?override fmt = wrap_sprint ?override ~code:green_code     fmt
let blue_sprintf      ?override fmt = wrap_sprint ?override ~code:blue_code      fmt
let magenta_sprintf   ?override fmt = wrap_sprint ?override ~code:magenta_code   fmt
let cyan_sprintf      ?override fmt = wrap_sprint ?override ~code:cyan_code      fmt

let gray_sprintf ?override ~brightness fmt = wrap_sprint ?override ~code:(gray_code ~brightness) fmt
let rgb_sprintf ?override ~r ~g ~b     fmt = wrap_sprint ?override ~code:(rgb_code ~r ~g ~b)     fmt
let color_sprintf ?override ~color     fmt = wrap_sprint ?override ~code:(color_code ~color)     fmt
