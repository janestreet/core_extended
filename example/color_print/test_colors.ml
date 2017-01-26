open Core
open Core_extended.Std

let () =
  printf "Printing lots of ansi color and formatting to test Color_print module\n";

  printf "Color cube:\n";
  let i = ref 0 in
  for r = 0 to 5 do
    for g = 0 to 5 do
      for b = 0 to 5 do
        let floatify x = Float.of_int x /. 5. in
        let r = floatify r in
        let g = floatify g in
        let b = floatify b in
        if !i mod 3 = 0
        then Color_print.rgb_printf ~r ~g ~b "%s" "X"
        else if !i mod 3 = 1
        then printf "%s" (Color_print.rgb_sprintf ~r ~g ~b "%s" "X")
        else printf "%s" (Color_print.rgb ~r ~g ~b "X");
        incr i;
      done;
      printf "\n";
    done;
    printf "\n";
  done;

  printf "Grayscale:\n";
  for x = 0 to 23 do
    let floatify x = Float.of_int x /. 23. in
    let x = floatify x in
    Color_print.gray_printf ~brightness:x "X";
  done;
  printf "\n";
  printf "\n";

  printf "Mixing formats and overriding:\n";
  printf "%s\n" (
    Color_print.magenta_sprintf "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s"
      (Color_print.red "red")
      ("magenta")
      (Color_print.bold "boldmagenta")
      (Color_print.bold (Color_print.bold "boldmagenta"))
      (Color_print.bold (Color_print.underline "underlineboldmagenta"))
      (Color_print.bold (Color_print.inverse "inverseboldmagenta"))
      (Color_print.inverse (Color_print.underline "underlineinversemagenta"))
      (Color_print.inverse (Color_print.blue "inverseblue"))
      (Color_print.inverse ~override:true (Color_print.blue "inverseblue"))
      (Color_print.green (Color_print.blue "blue"))
      (Color_print.green ~override:true (Color_print.blue "green"))
  );

  printf "%s\n" (
    Color_print.magenta_sprintf ~override:true "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s"
      (Color_print.red "magenta")
      ("magenta")
      (Color_print.bold "boldmagenta")
      (Color_print.bold (Color_print.bold "boldmagenta"))
      (Color_print.bold (Color_print.underline "underlineboldmagenta"))
      (Color_print.bold (Color_print.inverse "inverseboldmagenta"))
      (Color_print.inverse (Color_print.underline "underlineinversemagenta"))
      (Color_print.inverse (Color_print.blue "inversemagenta"))
      (Color_print.inverse ~override:true (Color_print.blue "inversemagenta"))
      (Color_print.green (Color_print.blue "magenta"))
      (Color_print.green ~override:true (Color_print.blue "magenta"))
  );

  printf "%s\n" (Color_print.normal (Color_print.red (Color_print.bold "normal")));

  ()
;;

