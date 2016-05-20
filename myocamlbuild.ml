(* OASIS_START *)
(* OASIS_STOP *)

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let setup_preprocessor_deps = function
  | After_rules ->
    dep ["pp_deps_for_src"] ["src/config_ext.h"];
  | _ -> ()

let dispatch = function
  | After_rules ->
    flag ["c"; "compile"] & S[A"-I"; A"src"; A"-package"; A"core"; A"-thread"];

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

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    setup_preprocessor_deps hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch hook;
    dispatch_default hook)
