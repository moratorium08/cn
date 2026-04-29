module StdList = Stdlib.List
module Int64Set = Data_point.Int64Set

let read_whole_file path : string =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s


let parse_results_json (path : string) : (int * Int64Set.t option) list =
  let json = Yojson.Safe.from_file path in
  let results =
    match json with
    | `Assoc kvs ->
      (match StdList.assoc_opt "results" kvs with
       | Some (`List rs) -> rs
       | _ -> failwith "fp_runner: missing 'results' array in JSON")
    | _ -> failwith "fp_runner: top-level JSON is not an object"
  in
  StdList.map
    (fun entry ->
       match entry with
       | `Assoc fields ->
         let q_idx =
           match StdList.assoc_opt "q" fields with
           | Some (`Int n) -> n
           | _ -> failwith "fp_runner: entry missing integer 'q'"
         in
         let addrs =
           match StdList.assoc_opt "addrs" fields with
           | Some `Null -> None
           | Some (`List items) ->
             let set =
               StdList.fold_left
                 (fun acc item ->
                    match item with
                    | `Int n -> Int64Set.add (Int64.of_int n) acc
                    | `Intlit s -> Int64Set.add (Int64.of_string s) acc
                    | _ -> failwith "fp_runner: addr entry is not an integer")
                 Int64Set.empty
                 items
             in
             Some set
           | _ -> failwith "fp_runner: entry missing 'addrs'"
         in
         (q_idx, addrs)
       | _ -> failwith "fp_runner: result entry is not an object")
    results


let run
      ~(cc : string)
      ~(output_dir : string)
      ~(cn_runtime_prefix : string)
      ~(func_name : string)
      ~(tag : string)
      (input : Fp_codegen.input)
  : Fp_table.t
  =
  let safe_func =
    Stdlib.String.map (fun c -> if Char.equal c '/' then '_' else c) func_name
  in
  let stem = Printf.sprintf "bi_abd_fp_%s_%s" safe_func tag in
  let src_path = Filename.concat output_dir (stem ^ ".c") in
  let obj_path = Filename.concat output_dir (stem ^ ".o") in
  let exe_path = Filename.concat output_dir (stem ^ ".out") in
  let json_path = Filename.concat output_dir (stem ^ ".json") in
  let input = { input with output_json_path = json_path } in
  let src = Fp_codegen.emit input in
  let oc = open_out src_path in
  output_string oc src;
  close_out oc;
  Pp.debug 4 (lazy (Pp.item "fp harness emitted" (Pp.string src_path)));
  let include_flag = "-I" ^ Filename.concat cn_runtime_prefix "include" in
  let lib_path = Filename.concat cn_runtime_prefix "libcn_exec.a" in
  let cflags =
    String.concat " " [ "-g"; "-O0"; "-Wno-error" ]
  in
  let compile_cmd =
    Printf.sprintf
      "%s %s -c %s %s -o %s 2>&1"
      cc
      cflags
      include_flag
      (Filename.quote src_path)
      (Filename.quote obj_path)
  in
  Pp.debug 5 (lazy (Pp.item "fp harness compile" (Pp.string compile_cmd)));
  if Sys.command compile_cmd <> 0 then (
    Printf.eprintf "fp_runner: harness compilation failed (%s)\n" src_path;
    Fp_table.empty)
  else (
    let link_cmd =
      Printf.sprintf
        "%s %s %s -o %s %s -lm 2>&1"
        cc
        cflags
        include_flag
        (Filename.quote exe_path)
        (Filename.quote obj_path
         ^ " "
         ^ Filename.quote lib_path)
    in
    Pp.debug 5 (lazy (Pp.item "fp harness link" (Pp.string link_cmd)));
    if Sys.command link_cmd <> 0 then (
      Printf.eprintf "fp_runner: harness link failed (%s)\n" exe_path;
      Fp_table.empty)
    else (
      let run_cmd = Filename.quote exe_path ^ " 2>&1" in
      Pp.debug 5 (lazy (Pp.item "fp harness run" (Pp.string run_cmd)));
      let exit_code = Sys.command run_cmd in
      if exit_code <> 0 then (
        Printf.eprintf
          "fp_runner: harness exited %d, partial results may be missing\n"
          exit_code);
      if not (Sys.file_exists json_path) then (
        Printf.eprintf "fp_runner: no JSON output at %s\n" json_path;
        Fp_table.empty)
      else (
        let results =
          try parse_results_json json_path with
          | e ->
            Printf.eprintf
              "fp_runner: JSON parse failed: %s\n  contents: %s\n"
              (Printexc.to_string e)
              (try read_whole_file json_path with _ -> "<unreadable>");
            []
        in
        Pp.debug
          4
          (lazy
            (let open Pp in
             item
               "fp harness results"
               (int (StdList.length results) ^^^ !^"entries")));
        Fp_table.of_results results)))
