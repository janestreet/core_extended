(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules ->
    dep  ["ocaml"; "ocamldep"; "mlh"] ["src/config.mlh"];

    flag ["mlh"; "ocaml"; "ocamldep"] (S[A"-ppopt"; A"-Isrc/"]);
    flag ["mlh"; "ocaml"; "compile"]  (S[A"-ppopt"; A"-Isrc/"]);
    flag ["mlh"; "ocaml"; "doc"]      (S[A"-ppopt"; A"-Isrc/"]);

    flag ["c"; "compile"] & S[A"-I"; A"src"; A"-package"; A"core"; A"-thread"];

    List.iter
      (fun tag ->
         pflag ["ocaml"; tag] "pa_ounit_lib"
           (fun s -> S[A"-ppopt"; A"-pa-ounit-lib"; A"-ppopt"; A s]))
      ["ocamldep"; "compile"; "doc"];

    let hack = "ugly_hack_to_workaround_ocamlbuild_nightmare" in
    mark_tag_used hack;
    dep [hack] [hack];

    let lib_kernel_mods =
      [ "generator"
      ; "observer"
      ; "jck"
      ; "janecheck_intf"
      ; "std"
      ]
    in

    let add_exts l exts =
      List.concat (List.map (fun fn ->
        let fn = "janecheck_kernel/src/" ^ fn in
        List.map (fun ext -> fn ^ ext)  exts)
        l)
    in

    rule hack
      ~prod:hack
      ~deps:(add_exts lib_kernel_mods [".cmx"; ".cmi"; ".cmo"])
      (fun _ _ ->
         let to_remove =
           add_exts lib_kernel_mods [ ".cmx"
                                    ; ".cmi"
                                    ; ".cmo"
                                    ; ".ml"
                                    ; ".mli"
                                    ; ".ml.depends"
                                    ; ".mli.depends"
                                    ; ".o"
                                    ]
         in
         Seq
           [ Seq (List.map rm_f to_remove)
           ; Echo ([], hack) ])
| _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
