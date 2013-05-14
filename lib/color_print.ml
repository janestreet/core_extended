open Core.Std


let ansi_regexp = Memo.unit (fun () -> Pcre.regexp "\027\\[.*?m")
let normal str =
  Pcre.replace ~rex:(ansi_regexp ()) ~templ:"" str

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
] with sexp, bin_io

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


let wrap str ~code = code ^ (normal str) ^ normal_code

let bold      str = wrap str ~code:bold_code
let underline str = wrap str ~code:underline_code
let red       str = wrap str ~code:red_code
let green     str = wrap str ~code:green_code
let yellow    str = wrap str ~code:yellow_code
let blue      str = wrap str ~code:blue_code
let magenta   str = wrap str ~code:magenta_code
let cyan      str = wrap str ~code:cyan_code
let inverse   str = wrap str ~code:inverse_code

let gray  str ~brightness = wrap str ~code:(gray_code ~brightness)
let rgb   str ~r ~g ~b    = wrap str ~code:(rgb_code ~r ~g ~b)
let color str ~color      = wrap str ~code:(color_code ~color)

(* Note that this always flushes after the terminating normal_code.
   This is probably not necessary, but it helps ensure that the code that turns
   off the special formatting actually does get output *)
let wrap_print ~code fmt =
  Printf.kfprintf (fun oc -> fprintf oc "%s%!" normal_code) stdout
    ("%s" ^^ fmt) code

let boldprintf      fmt = wrap_print ~code:bold_code      fmt
let underlineprintf fmt = wrap_print ~code:underline_code fmt
let inverseprintf   fmt = wrap_print ~code:inverse_code   fmt
let redprintf       fmt = wrap_print ~code:red_code       fmt
let yellowprintf    fmt = wrap_print ~code:yellow_code    fmt
let greenprintf     fmt = wrap_print ~code:green_code     fmt
let blueprintf      fmt = wrap_print ~code:blue_code      fmt
let magentaprintf   fmt = wrap_print ~code:magenta_code   fmt
let cyanprintf      fmt = wrap_print ~code:cyan_code      fmt

let grayprintf ~brightness fmt = wrap_print ~code:(gray_code ~brightness) fmt
let rgbprintf ~r ~g ~b     fmt = wrap_print ~code:(rgb_code ~r ~g ~b)     fmt
let colorprintf ~color     fmt = wrap_print ~code:(color_code ~color)     fmt
