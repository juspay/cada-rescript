(* lib/rescript_differ.ml *)
open Printf

(* Simplified data type - only detailed_changes *)
type detailed_changes = {
  module_name: string;
  (* Functions *)
  added_functions: (string * string) list;
  modified_functions: (string * string * string) list;
  deleted_functions: (string * string) list;
  (* Types *)
  added_types: (string * string) list;
  modified_types: (string * string * string) list;
  deleted_types: (string * string) list;
  (* Externals *)
  added_externals: (string * string) list;
  modified_externals: (string * string * string) list;
  deleted_externals: (string * string) list;
} [@@deriving yojson]

module StringMap = Map.Make(String)

(* Parse ReScript file using ReScript CLI *)
let parse_rescript_via_cli filepath =
  try
    (* Use ReScript's format command to validate and parse *)
    let cmd = Printf.sprintf "rescript format %s 2>/dev/null" filepath in
    let ic = Unix.open_process_in cmd in
    let formatted_content = really_input_string ic (in_channel_length ic) in
    let exit_code = Unix.close_process_in ic in
    
    if exit_code = Unix.WEXITED 0 then
      (* If formatting succeeded, the file is valid ReScript *)
      (* Now extract declarations from the formatted content *)
      let lines = String.split_on_char '\n' formatted_content in
      let functions = ref [] in
      let types = ref [] in
      let externals = ref [] in
      
      List.iteri (fun line_num line ->
        let trimmed = String.trim line in
        if String.length trimmed > 4 && String.sub trimmed 0 4 = "let " then
          functions := (Printf.sprintf "func_%d" line_num, line) :: !functions
        else if String.length trimmed > 5 && String.sub trimmed 0 5 = "type " then
          types := (Printf.sprintf "type_%d" line_num, line) :: !types
        else if String.length trimmed > 9 && String.sub trimmed 0 9 = "external " then
          externals := (Printf.sprintf "ext_%d" line_num, line) :: !externals
      ) lines;
      
      Some (!functions, !types, !externals)
    else
      None
  with
  | _ -> None

(* Parse ReScript/OCaml file using standard parser *)
let parse_implementation content _filename =
  try
    let lexbuf = Lexing.from_string content in
    (* Simple parsing without setting filename *)
    let ast = Parse.implementation lexbuf in
    Some ast
  with
  | _ -> None

(* Extract functions from Parsetree *)
let extract_functions_from_ast ast =
  let open Parsetree in
  List.fold_left (fun acc item ->
    match item.pstr_desc with
    | Pstr_value (_, bindings) ->
        List.fold_left (fun acc binding ->
          match binding.pvb_pat.ppat_desc with
          | Ppat_var {txt = name; _} ->
              let code = Format.asprintf "%a" Pprintast.structure_item item in
              (name, code) :: acc
          | _ -> acc
        ) acc bindings
    | _ -> acc
  ) [] ast

(* Extract type declarations from Parsetree *)
let extract_types_from_ast ast =
  let open Parsetree in
  List.fold_left (fun acc item ->
    match item.pstr_desc with
    | Pstr_type (_, type_declarations) ->
        List.fold_left (fun acc decl ->
          let name = decl.ptype_name.txt in
          let code = Format.asprintf "%a" Pprintast.structure_item item in
          (name, code) :: acc
        ) acc type_declarations
    | _ -> acc
  ) [] ast

(* Extract external declarations from Parsetree *)
let extract_externals_from_ast ast =
  let open Parsetree in
  List.fold_left (fun acc item ->
    match item.pstr_desc with
    | Pstr_primitive value_desc ->
        let name = value_desc.pval_name.txt in
        let code = Format.asprintf "%a" Pprintast.structure_item item in
        (name, code) :: acc
    | _ -> acc
  ) [] ast

(* Helper functions *)
let extract_module_name filename =
  Filename.basename filename 
  |> Filename.remove_extension
  |> String.capitalize_ascii

let read_file filepath =
  let ic = open_in filepath in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let parse_file filepath =
  try
    let content = read_file filepath in
    let module_name = extract_module_name filepath in
    match parse_implementation content filepath with
    | Some ast ->
        let functions = extract_functions_from_ast ast in
        let types = extract_types_from_ast ast in
        let externals = extract_externals_from_ast ast in
        Some (module_name, functions, types, externals)
    | None -> 
        (* Try ReScript CLI parser *)
        match parse_rescript_via_cli filepath with
        | Some (functions, types, externals) ->
            Some (module_name, functions, types, externals)
        | None -> None
  with
  | exn -> 
    eprintf "Error parsing %s: %s\n" filepath (Printexc.to_string exn);
    None

(* Compare declarations *)
let compare_declarations old_items new_items =
  let old_map = List.fold_left (fun acc (name, code) ->
    StringMap.add name code acc
  ) StringMap.empty old_items in
  
  let new_map = List.fold_left (fun acc (name, code) ->
    StringMap.add name code acc
  ) StringMap.empty new_items in

  let added = List.filter (fun (name, _) ->
    not (StringMap.mem name old_map)
  ) new_items in

  let deleted = List.fold_left (fun acc (name, code) ->
    if not (StringMap.mem name new_map) then
      (name, code) :: acc
    else acc
  ) [] old_items in

  let modified = List.fold_left (fun acc (name, new_code) ->
    match StringMap.find_opt name old_map with
    | Some old_code when old_code <> new_code ->
        (name, old_code, new_code) :: acc
    | _ -> acc
  ) [] new_items in

  (added, deleted, modified)

(* Get all changes with code *)
let get_all_changes_with_code old_module new_module =
  let (_, old_functions, old_types, old_externals) = old_module in
  let (new_name, new_functions, new_types, new_externals) = new_module in
  
  let (added_functions, deleted_functions, modified_functions) = 
    compare_declarations old_functions new_functions in
  let (added_types, deleted_types, modified_types) = 
    compare_declarations old_types new_types in
  let (added_externals, deleted_externals, modified_externals) = 
    compare_declarations old_externals new_externals in

  {
    module_name = new_name;
    added_functions;
    modified_functions;
    deleted_functions;
    added_types;
    modified_types;
    deleted_types;
    added_externals;
    modified_externals;
    deleted_externals;
  }

(* Get entire module as changes (for new/deleted modules) *)
let get_entire_module_as_changes (module_name, functions, types, externals) is_added =
  if is_added then
    {
      module_name;
      added_functions = functions;
      modified_functions = [];
      deleted_functions = [];
      added_types = types;
      modified_types = [];
      deleted_types = [];
      added_externals = externals;
      modified_externals = [];
      deleted_externals = [];
    }
  else
    {
      module_name;
      added_functions = [];
      modified_functions = [];
      deleted_functions = functions;
      added_types = [];
      modified_types = [];
      deleted_types = types;
      added_externals = [];
      modified_externals = [];
      deleted_externals = externals;
    }

(* Process module safely *)
let process_module_safe file_path =
  let module_name = extract_module_name file_path in
  if Sys.file_exists file_path then
    match parse_file file_path with
    | Some result -> (module_name, Some result, true)
    | None -> 
        eprintf "Error parsing module: %s\n" file_path;
        (module_name, None, true)
  else
    (module_name, None, false)

(* Git operations *)
let run_command cmd =
  let ic = Unix.open_process_in cmd in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file -> List.rev acc
  in
  let lines = read_lines [] in
  ignore (Unix.close_process_in ic);
  lines

let clone_repo repo_url local_path =
  if not (Sys.file_exists local_path) then (
    let cmd = sprintf "git clone %s %s" repo_url local_path in
    ignore (Sys.command cmd);
    printf "Cloned repository to %s\n" local_path
  ) else
    printf "Repository already exists at %s\n" local_path

let get_changed_files branch_name new_commit local_path =
  let old_dir = Sys.getcwd () in
  Sys.chdir local_path;
  ignore (Sys.command (sprintf "git checkout %s" branch_name));
  let commit_lines = run_command (sprintf "git rev-parse %s" branch_name) in
  let commit = match commit_lines with
    | h :: _ -> String.trim h
    | [] -> branch_name
  in
  let diff_lines = run_command (sprintf "git diff --name-only %s %s" commit new_commit) in
  Sys.chdir old_dir;
  List.filter (fun f -> Filename.extension f = ".res") diff_lines

(* Partition modules *)
let partition_modules ast_tuples =
  let new_modules = List.fold_left (fun acc ((_, old_ast, old_exists), (name, new_ast, new_exists)) ->
    match (old_exists, new_exists, old_ast, new_ast) with
    | (false, true, None, Some ast) -> (name, ast) :: acc
    | _ -> acc
  ) [] ast_tuples in
  
  let deleted_modules = List.fold_left (fun acc ((name, old_ast, old_exists), (_, _, new_exists)) ->
    match (old_exists, new_exists, old_ast) with
    | (true, false, Some ast) -> (name, ast) :: acc
    | _ -> acc
  ) [] ast_tuples in
  
  let modified_modules = List.fold_left (fun acc ((old_name, old_ast, old_exists), (new_name, new_ast, new_exists)) ->
    match (old_exists, new_exists, old_ast, new_ast) with
    | (true, true, Some old_ast, Some new_ast) -> ((old_name, old_ast), (new_name, new_ast)) :: acc
    | _ -> acc
  ) [] ast_tuples in

  (new_modules, deleted_modules, modified_modules)

(* Create output files *)
let create_output_files changes =
  let detailed_json = `List (List.map detailed_changes_to_yojson changes) in
  let oc = open_out "detailed_changes.json" in
  Yojson.Safe.pretty_to_channel oc detailed_json;
  close_out oc;
  printf "Created output file: detailed_changes.json\n"

(* Main entry point *)
let run () =
  let argv = Sys.argv in
  if Array.length argv <> 6 then (
    eprintf "Usage: %s <repo_url> <local_path> <branch_name> <current_commit> <path>\n" argv.(0);
    exit 1
  ) else
    let repo_url = argv.(1) in
    let local_repo_path = argv.(2) in
    let branch_name = argv.(3) in
    let current_commit = argv.(4) in
    let _path = argv.(5) in
    
    printf "Cloning repository...\n";
    clone_repo repo_url local_repo_path;
    
    printf "Getting changed files...\n";
    let changed_files = get_changed_files branch_name current_commit local_repo_path in
    printf "Found %d changed ReScript files\n" (List.length changed_files);
    
    let file_paths = List.map (Filename.concat local_repo_path) changed_files in
    
    printf "Processing modules for previous commit...\n";
    let previous_ast = List.map process_module_safe file_paths in
    
    printf "Switching to current commit...\n";
    let old_dir = Sys.getcwd () in
    Sys.chdir local_repo_path;
    ignore (Sys.command (sprintf "git checkout %s" current_commit));
    Sys.chdir old_dir;
    
    printf "Processing modules for current commit...\n";
    let current_ast = List.map process_module_safe file_paths in
    
    let ast_tuples = List.combine previous_ast current_ast in
    let (new_modules, deleted_modules, modified_modules) = partition_modules ast_tuples in
    
    printf "Found: %d new, %d deleted, %d modified modules\n" 
      (List.length new_modules) (List.length deleted_modules) (List.length modified_modules);
    
    let new_module_changes = List.map (fun (_, ast) ->
      get_entire_module_as_changes ast true
    ) new_modules in
    
    let deleted_module_changes = List.map (fun (_, ast) ->
      get_entire_module_as_changes ast false
    ) deleted_modules in
    
    let modified_changes = List.map (fun ((_, old_ast), (_, new_ast)) ->
      get_all_changes_with_code old_ast new_ast
    ) modified_modules in
    
    let all_changes = new_module_changes @ deleted_module_changes @ modified_changes in
    
    printf "Creating output files...\n";
    create_output_files all_changes;
    printf "Processing complete!\n"