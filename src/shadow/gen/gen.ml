module List = ListLabels
open MoreLabels

module Smap = Map.Make(String)

let () =
  let ocaml_where, oc =
    match Sys.argv with
    | [|_; "-ocaml-where"; ocaml_where; "-o"; fn|] ->
      (ocaml_where, open_out fn)
    | _ ->
      failwith "bad command line arguments"
  in

  let (^/) = Filename.concat in
  let files =
    let dirs =
      [ "compiler-libs"
      ; "ocamlcommon"
      ; "ocamlbytecomp"
      ; "ocamloptcomp"
      ; "ocamltoplevel"
      ; "ocamlopttoplevel"
      ]
    in
    List.concat (List.map dirs ~f:(fun dir ->
      let basenames =
        try Array.to_list (Sys.readdir (ocaml_where ^/ dir))
        with Sys_error _ -> []
      in
      List.map basenames ~f:(fun b -> dir ^/ b)))
  in

  let all_exposed_modules =
    List.filter files ~f:(fun fn -> Filename.check_suffix fn ".cmi")
    |> List.map ~f:(fun fn ->
      String.capitalize_ascii (Filename.chop_extension (Filename.basename fn)))
  in

  let module_to_lib =
    List.filter files ~f:(fun fn -> Filename.check_suffix fn ".cma")
    |> List.fold_left ~init:Smap.empty ~f:(fun acc fn ->
      let lib_mod =
        try
          Scanf.sscanf fn "ocaml%s.cma" (fun s -> "Ocaml_" ^ s)
        with _ ->
          String.capitalize_ascii (Filename.chop_extension fn)
      in
      let units = Read_cma.units (ocaml_where ^/ fn) in
      List.fold_left units ~init:acc ~f:(fun acc unit ->
        Smap.add acc ~key:unit ~data:lib_mod))
  in

  (* If we keep the alias, we can't use -linkall... *)
  Printf.fprintf oc "module Do_not_use_directly = struct end\n";
  List.sort all_exposed_modules ~cmp:String.compare
  |> List.iter ~f:(fun m ->
    let repl =
      match Smap.find m module_to_lib with
      | lib -> Printf.sprintf ", use %s.%s instead" lib m
      | exception Not_found -> ""
    in
    Printf.fprintf oc
      "module %s = Do_not_use_directly [@@deprecated \"Accessing this module directly is deprecated%s\"]\n"
      m repl)
