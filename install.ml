#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"core_extended"
  [ oasis_lib "core_extended"
  ; oasis_lib "selector"
  ; file "META" ~section:"lib"
  ]
