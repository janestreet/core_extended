open Core.Std
open Core_extended.Std
open OUnit

(*
  This test checks that no new link dependencies have been added
*)
let libs pgm =
  Shell.run_lines "ldd" [pgm]
  |! List.map ~f:(String.strip)
  |! List.filter
      ~f:(fun s -> not (String.is_prefix ~prefix:"/lib64/ld-linux" s))
  |! List.map
      ~f:(fun s -> match String.lsplit2 s ~on:'.' with
          | None ->
              assert_failure
                (sprintf
                   "ldd_test:%s does not seem to be a valid library name"
                   s)
          | Some (v,_) -> v)

let whitelist = ["libpcre"]

let core_hello = ref "core_hello.exe"
let core_extended_hello = ref "core_extended_hello.exe"
let args =
  ["--core-hello",Arg.Set_string core_hello,"PGM hello world program linked against core";
   "--core-extended-hello",Arg.Set_string core_extended_hello,"PGM hello world program linked against core_extended"
  ]

let check_exe f =
  if not (Sys.file_exists_exn f) then
    assert_failure
      (sprintf "could not find "^f)

let test =
  "Ldd_test" >::
    (fun () ->
       check_exe !core_hello;
       check_exe !core_extended_hello;
       let base_libs = libs !core_hello @ whitelist
       and ext_libs = libs  !core_extended_hello in
       let added_libs = List.filter ext_libs
         ~f:(fun l -> not (List.mem base_libs l))
       in
       if added_libs <> [] then
         assert_failure
           (sprintf "Core_extended links in new external libraries %s"
              (String.concat ~sep:" " added_libs)));
